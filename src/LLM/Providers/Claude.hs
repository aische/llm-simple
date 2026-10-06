module LLM.Providers.Claude
  ( claudeGateway,
    claudeGatewayWith,
    claudeProvider,
    claudeProviderWith,
    parseClaudeResponse,
    parseClaudeStream,
    parseClaudeUsage,
    claudeBuildBody,
    encodeTurn,
    effortToBudgetTokens,
  )
where

import Data.Aeson
  ( KeyValue ((.=)),
    Value (Object, String),
    decodeStrict',
    encode,
    object,
    toJSON,
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Object, Pair, Parser, parseMaybe)
import Data.ByteString qualified as BS
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import LLM.Core.LLMProvider (LLMProvider (..), toGateway)
import LLM.Core.ProviderUtils (handleStreamResponse, lenientConfig, stripJsonFences)
import LLM.Core.SSE (SSEEvent (sseData, sseEvent), readSSEEvents)
import LLM.Core.Types
  ( ChatRequest
      ( reqConversation,
        reqMaxTokens,
        reqModel,
        reqSystem,
        reqTemperature,
        reqThinking,
        reqTools
      ),
    CacheHint (..),
    ContentPart (..),
    LLMError (EmptyResponse),
    LLMGateway,
    LLMResult,
    LLMTextResult,
    PartBody (..),
    ProviderOpaque (..),
    StreamEvent (..),
    ThinkingContent (..),
    ThinkingMode (..),
    ToolCall (..),
    ToolDef (toolDescription, toolName, toolParameters),
    ToolResult (trCallId, trContent),
    Turn (..),
    ImageSource (..),
    mkChatResponse,
    mkToolCall,
    stripForeignOpaque,
    textPart,
    thinkingPart,
    toolCallPart,
    pattern UserTurn,
  )
import LLM.Core.Usage (Usage (..), emptyUsage)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Req
  ( Option,
    POST (POST),
    ReqBodyJson (ReqBodyJson),
    Url,
    header,
    https,
    jsonResponse,
    req,
    reqBr,
    responseBody,
    responseStatusCode,
    runReq,
    (/:),
  )

claudeProviderName :: Text
claudeProviderName = "claude"

-- | Create a LLMGateway for the Claude provider at api.anthropic.com.
claudeGateway :: Text -> LLMGateway
claudeGateway apiKey = toGateway $ claudeProvider apiKey

-- | Create a Claude-compatible client with a custom base URL (origin).
-- The library appends @/v1/messages@.
claudeGatewayWith :: Url scheme -> Option scheme -> Text -> LLMGateway
claudeGatewayWith baseUrl baseOpts apiKey = toGateway (claudeProviderWith baseUrl baseOpts apiKey)

-- | Create a LLMProvider for the Claude provider at api.anthropic.com.
claudeProvider :: Text -> LLMProvider
claudeProvider = claudeProviderWith (https "api.anthropic.com") mempty

-- | Claude-compatible provider with a custom base URL (origin).
-- The library appends @/v1/messages@.
claudeProviderWith :: Url scheme -> Option scheme -> Text -> LLMProvider
claudeProviderWith baseUrl baseOpts apiKey =
  LLMProvider
    { providerName = claudeProviderName,
      buildBody = claudeBuildBody,
      sendRequest = sendRequest,
      sendStreamRequest = \body callback ->
        runReq lenientConfig $ do
          let url = baseUrl /: "v1" /: "messages"
              opts = baseOpts <> claudeAuthOpts apiKey
              modelHint = fromMaybe "" (parseMaybe (withObject "body" (.: "model")) body)
          reqBr POST url (ReqBodyJson body) opts $ \resp ->
            handleStreamResponse resp (\br -> parseClaudeStream modelHint (HC.brRead br) callback),
      parseResponse = pure . parseClaudeResponse,
      buildObjectBody = \r schema ->
        let schemaText = TL.toStrict . decodeUtf8 $ encode schema
            instruction = "Respond with a raw JSON object matching this schema. No markdown, no explanation, no code fences:\n" <> schemaText
            conv' = r.reqConversation <> [UserTurn instruction]
         in claudeBuildBody False (r {reqConversation = conv'}),
      sendObjectRequest = sendRequest,
      parseObjectResponse = parseClaudeObjectResponse
    }
  where
    sendRequest body =
      runReq lenientConfig $ do
        let url = baseUrl /: "v1" /: "messages"
            opts = baseOpts <> claudeAuthOpts apiKey
        resp <- req POST url (ReqBodyJson body) jsonResponse opts
        pure (responseStatusCode resp, responseBody resp)

claudeAuthOpts :: Text -> Option scheme
claudeAuthOpts apiKey =
  header "x-api-key" (encodeUtf8 apiKey)
    <> header "anthropic-version" "2023-06-01"

-- | Map catalog effort labels to Claude extended-thinking @budget_tokens@.
effortToBudgetTokens :: Text -> Int
effortToBudgetTokens = \case
  "low" -> 1024
  "medium" -> 4096
  "high" -> 10000
  "max" -> 16000
  other ->
    case reads (T.unpack other) of
      [(n, "")] | n >= 1024 -> n
      _ -> 10000

claudeBuildBody :: Bool -> ChatRequest -> Value
claudeBuildBody stream r =
  object $
    [ "model" .= r.reqModel,
      "max_tokens" .= r.reqMaxTokens,
      "messages" .= concatMap (encodeTurn r.reqModel) r.reqConversation
    ]
      ++ ["system" .= sys | Just sys <- [r.reqSystem]]
      ++ temperaturePairs r
      ++ thinkingPairs r
      ++ ["tools" .= map encodeToolDef r.reqTools | not (null r.reqTools)]
      ++ ["stream" .= True | stream]

-- | Anthropic rejects non-default temperature while thinking is active.
-- Omit temperature whenever thinking is enabled.
temperaturePairs :: ChatRequest -> [Pair]
temperaturePairs r =
  case r.reqThinking of
    Just tm | tm.tmEnabled -> []
    _ -> ["temperature" .= t | Just t <- [r.reqTemperature]]

thinkingPairs :: ChatRequest -> [Pair]
thinkingPairs r =
  case r.reqThinking of
    Just tm
      | tm.tmEnabled ->
          let budget = maybe 10000 effortToBudgetTokens tm.tmEffort
           in [ "thinking"
                  .= object
                    [ "type" .= ("enabled" :: Text),
                      "budget_tokens" .= budget
                    ]
              ]
    Just tm
      | not tm.tmEnabled ->
          ["thinking" .= object ["type" .= ("disabled" :: Text)]]
    _ -> []

encodeTurn :: Text -> Turn -> [Value]
encodeTurn _ (UserMessage parts) =
  [ object
      [ "role" .= ("user" :: Text),
        "content" .= encodeUserContent parts
      ]
  ]
encodeTurn currentModel (AssistantMessage parts) =
  [ object
      [ "role" .= ("assistant" :: Text),
        "content" .= mapMaybe (encodeAssistantPart currentModel) (map (stripForeignOpaque claudeProviderName) parts)
      ]
  ]
encodeTurn _ (ToolTurn results) =
  [ object
      [ "role" .= ("user" :: Text),
        "content" .= map encodeToolResult results
      ]
  ]

encodeUserContent :: [ContentPart] -> Value
-- Bare string only when a single unannotated text part (fixture-compatible).
encodeUserContent [ContentPart (TextPart t) Nothing] = String t
encodeUserContent parts = toJSON (mapMaybe encodeUserPart parts)
  where
    encodeUserPart (ContentPart (TextPart t) hint) =
      Just $ withCacheControl hint $ object ["type" .= ("text" :: Text), "text" .= t]
    encodeUserPart (ContentPart (ImagePart src) hint) =
      Just $ withCacheControl hint $ encodeImageBlock src
    encodeUserPart _ = Nothing

encodeImageBlock :: ImageSource -> Value
encodeImageBlock (ImageUrl url) =
  object
    [ "type" .= ("image" :: Text),
      "source"
        .= object
          [ "type" .= ("url" :: Text),
            "url" .= url
          ]
    ]
encodeImageBlock (ImageBase64 mediaType data_) =
  object
    [ "type" .= ("image" :: Text),
      "source"
        .= object
          [ "type" .= ("base64" :: Text),
            "media_type" .= mediaType,
            "data" .= data_
          ]
    ]

-- | Attach Anthropic @cache_control@ for the default ephemeral TTL.
withCacheControl :: Maybe CacheHint -> Value -> Value
withCacheControl (Just CacheEphemeral) (Object o) =
  Object $ KM.insert "cache_control" (object ["type" .= ("ephemeral" :: Text)]) o
withCacheControl _ v = v

encodeAssistantPart :: Text -> ContentPart -> Maybe Value
encodeAssistantPart currentModel (ContentPart (ThinkingPart tc) hint) =
  case opaqueForClaude currentModel tc.thinkingOpaque of
    Just payload -> Just $ withCacheControl hint payload
    Nothing ->
      -- Without a Claude signature we must not invent a thinking block.
      Nothing
encodeAssistantPart _ (ContentPart (TextPart t) hint)
  | T.null t = Nothing
  | otherwise =
      Just $
        withCacheControl hint $
          object ["type" .= ("text" :: Text), "text" .= t]
encodeAssistantPart _ (ContentPart (ToolCallPart tc) hint) =
  Just $ withCacheControl hint $ encodeToolUseBlock tc
encodeAssistantPart _ (ContentPart (ImagePart _) _) = Nothing

opaqueForClaude :: Text -> Maybe ProviderOpaque -> Maybe Value
opaqueForClaude _ (Just o)
  | o.poProvider == claudeProviderName = Just o.poPayload
opaqueForClaude _ _ = Nothing

encodeToolDef :: ToolDef -> Value
encodeToolDef td =
  object
    [ "name" .= td.toolName,
      "description" .= td.toolDescription,
      "input_schema" .= td.toolParameters
    ]

encodeToolUseBlock :: ToolCall -> Value
encodeToolUseBlock tc =
  object
    [ "type" .= ("tool_use" :: Text),
      "id" .= tc.tcId,
      "name" .= tc.tcName,
      "input" .= tc.tcArguments
    ]

encodeToolResult :: ToolResult -> Value
encodeToolResult tr =
  object
    [ "type" .= ("tool_result" :: Text),
      "tool_use_id" .= tr.trCallId,
      "content" .= tr.trContent
    ]

parseClaudeResponse :: Value -> LLMTextResult
parseClaudeResponse v = case parseMaybe (go modelVer) v of
  Nothing -> Left EmptyResponse
  Just parts ->
    case parts of
      [] -> Left EmptyResponse
      _ -> Right (mkChatResponse parts (parseClaudeUsage v))
  where
    modelVer = parseMaybe (withObject "ClaudeResponse" (.: "model")) v

    go :: Maybe Text -> Value -> Parser [ContentPart]
    go mv = withObject "ClaudeResponse" $ \o -> do
      content <- o .: "content" :: Parser [Value]
      mapM (parseBlock mv) content

parseBlock :: Maybe Text -> Value -> Parser ContentPart
parseBlock mv = withObject "content_block" $ \o -> do
  typ <- o .: "type" :: Parser Text
  case typ of
    "text" -> textPart <$> o .: "text"
    "thinking" -> do
      thinkingTxt <- o .:? "thinking" :: Parser (Maybe Text)
      signature <- o .:? "signature" :: Parser (Maybe Text)
      let payload =
            object $
              ["type" .= ("thinking" :: Text)]
                ++ ["thinking" .= t | Just t <- [thinkingTxt]]
                ++ ["signature" .= s | Just s <- [signature]]
          opaque =
            ProviderOpaque
              { poProvider = claudeProviderName,
                poModel = mv,
                poPayload = payload
              }
          mText = case thinkingTxt of
            Just t | not (T.null t) -> Just t
            _ -> Nothing
      pure $ thinkingPart (ThinkingContent mText (Just opaque))
    "redacted_thinking" -> do
      data_ <- o .: "data" :: Parser Text
      let payload =
            object
              [ "type" .= ("redacted_thinking" :: Text),
                "data" .= data_
              ]
          opaque =
            ProviderOpaque
              { poProvider = claudeProviderName,
                poModel = mv,
                poPayload = payload
              }
      pure $ thinkingPart (ThinkingContent Nothing (Just opaque))
    "tool_use" -> do
      cid <- o .: "id"
      name <- o .: "name"
      args <- o .: "input"
      pure $ toolCallPart (mkToolCall cid name args)
    _ -> fail $ "Unknown content block type: " <> T.unpack typ

parseClaudeUsage :: Value -> Maybe Usage
parseClaudeUsage = parseMaybe $ withObject "ClaudeResponse" $ \o -> do
  u <- o .: "usage"
  withObject "usage" parseClaudeUsageObject u

parseClaudeUsageObject :: Object -> Parser Usage
parseClaudeUsageObject uo = do
  -- Anthropic: input_tokens is uncached-only; total = uncached + read + creation.
  uncached <- uo .: "input_tokens"
  output <- uo .: "output_tokens"
  cacheRead <- uo .:? "cache_read_input_tokens" .!= 0
  cacheCreate <- uo .:? "cache_creation_input_tokens" .!= 0
  pure
    Usage
      { usageInputTokens = uncached + cacheRead + cacheCreate,
        usageOutputTokens = output,
        usageCacheReadTokens = cacheRead,
        usageCacheCreationTokens = cacheCreate,
        usageTotalCost = 0
      }

parseClaudeObjectResponse :: Value -> IO (LLMResult (Value, Maybe Usage))
parseClaudeObjectResponse v = case parseMaybe go v of
  Nothing -> pure $ Left EmptyResponse
  Just text -> case decodeStrict' (encodeUtf8 (stripJsonFences text)) of
    Nothing -> pure $ Left EmptyResponse
    Just obj -> pure $ Right (obj, parseClaudeUsage v)
  where
    go :: Value -> Parser Text
    go = withObject "ClaudeResponse" $ \o -> do
      content <- o .: "content" :: Parser [Value]
      texts <- mapMaybeM textOf content
      case texts of
        (t : _) -> pure t
        _ -> fail "No content"
    textOf = parseMaybe $ withObject "content_block" $ \o -> do
      typ <- o .: "type" :: Parser Text
      case typ of
        "text" -> o .: "text"
        _ -> fail "not text"
    mapMaybeM f xs = pure (mapMaybe f xs)

-- Streaming ------------------------------------------------------------------

data StreamBlock
  = StreamText Text
  | StreamThinking Text (Maybe Text) -- text, signature
  | StreamRedacted Text -- data
  | StreamTool Text Text Value -- id, name, args

parseClaudeStream :: Text -> IO BS.ByteString -> (StreamEvent -> IO ()) -> IO LLMTextResult
parseClaudeStream modelHint readChunk callback = do
  blocksRef <- newIORef ([] :: [StreamBlock])
  usageRef <- newIORef emptyUsage
  toolAccRef <- newIORef (Nothing :: Maybe (Text, Text, Text))
  thinkingAccRef <- newIORef (Nothing :: Maybe (Text, Maybe Text)) -- text, signature
  redactedRef <- newIORef (Nothing :: Maybe Text)
  modelRef <- newIORef modelHint
  readSSEEvents readChunk $ \sse -> do
    case sse.sseEvent of
      Just "message_start" ->
        case decodeStrict' (encodeUtf8 sse.sseData) of
          Just v -> do
            case parseMaybe parseMessageStartUsage v of
              Just startUsage -> modifyIORef' usageRef $ \u ->
                u
                  { usageInputTokens = startUsage.usageInputTokens,
                    usageCacheReadTokens = startUsage.usageCacheReadTokens,
                    usageCacheCreationTokens = startUsage.usageCacheCreationTokens
                  }
              Nothing -> pure ()
            case parseMaybe parseMessageStartModel v of
              Just m -> writeIORef modelRef m
              Nothing -> pure ()
          Nothing -> pure ()
      Just "content_block_start" ->
        case decodeStrict' (encodeUtf8 sse.sseData) of
          Just v -> case parseMaybe parseContentBlockStart v of
            Just (StartTool cid name) -> writeIORef toolAccRef (Just (cid, name, ""))
            Just StartThinking -> writeIORef thinkingAccRef (Just ("", Nothing))
            Just (StartRedacted data_) -> writeIORef redactedRef (Just data_)
            Just StartText -> pure ()
            Nothing -> pure ()
          Nothing -> pure ()
      Just "content_block_delta" ->
        case decodeStrict' (encodeUtf8 sse.sseData) of
          Just v -> do
            case parseMaybe parseTextDelta v of
              Just txt -> do
                modifyIORef' blocksRef (StreamText txt :)
                callback (StreamDelta txt)
              Nothing -> pure ()
            case parseMaybe parseThinkingDelta v of
              Just txt -> do
                modifyIORef' thinkingAccRef $ fmap (\(acc, sig) -> (acc <> txt, sig))
                callback (StreamReasoningDelta txt)
              Nothing -> pure ()
            case parseMaybe parseSignatureDelta v of
              Just sig ->
                modifyIORef' thinkingAccRef $ fmap (\(acc, _) -> (acc, Just sig))
              Nothing -> pure ()
            case parseMaybe parseInputJsonDelta v of
              Just fragment ->
                modifyIORef' toolAccRef $ fmap (\(cid, name, acc) -> (cid, name, acc <> fragment))
              Nothing -> pure ()
          Nothing -> pure ()
      Just "content_block_stop" -> do
        mThinking <- readIORef thinkingAccRef
        case mThinking of
          Just (txt, mSig) -> do
            modifyIORef' blocksRef (StreamThinking txt mSig :)
            writeIORef thinkingAccRef Nothing
          Nothing -> pure ()
        mRedacted <- readIORef redactedRef
        case mRedacted of
          Just data_ -> do
            modifyIORef' blocksRef (StreamRedacted data_ :)
            writeIORef redactedRef Nothing
          Nothing -> pure ()
        mTool <- readIORef toolAccRef
        case mTool of
          Just (cid, name, jsonStr) -> do
            let args = case decodeStrict' (encodeUtf8 jsonStr) of
                  Just a -> a
                  Nothing -> String jsonStr
                tc = mkToolCall cid name args
            modifyIORef' blocksRef (StreamTool cid name args :)
            callback (StreamToolCall tc)
            writeIORef toolAccRef Nothing
          Nothing -> pure ()
      Just "message_delta" ->
        case decodeStrict' (encodeUtf8 sse.sseData) of
          Just v -> case parseMaybe parseMessageDeltaUsage v of
            Just outputToks -> modifyIORef' usageRef $ \u -> u {usageOutputTokens = outputToks}
            Nothing -> pure ()
          Nothing -> pure ()
      _ -> pure ()
  rawBlocks <- reverse <$> readIORef blocksRef
  usage <- readIORef usageRef
  model <- readIORef modelRef
  let parts = map (streamBlockToPart model) rawBlocks
  if null parts
    then pure $ Left EmptyResponse
    else pure $ Right (mkChatResponse parts (Just usage))

streamBlockToPart :: Text -> StreamBlock -> ContentPart
streamBlockToPart _ (StreamText t) = textPart t
streamBlockToPart model (StreamThinking txt mSig) =
  let payload =
        object $
          ["type" .= ("thinking" :: Text)]
            ++ ["thinking" .= txt | not (T.null txt)]
            ++ ["signature" .= s | Just s <- [mSig]]
      opaque =
        ProviderOpaque
          { poProvider = claudeProviderName,
            poModel = if T.null model then Nothing else Just model,
            poPayload = payload
          }
      mText = if T.null txt then Nothing else Just txt
   in thinkingPart (ThinkingContent mText (Just opaque))
streamBlockToPart model (StreamRedacted data_) =
  let payload =
        object
          [ "type" .= ("redacted_thinking" :: Text),
            "data" .= data_
          ]
      opaque =
        ProviderOpaque
          { poProvider = claudeProviderName,
            poModel = if T.null model then Nothing else Just model,
            poPayload = payload
          }
   in thinkingPart (ThinkingContent Nothing (Just opaque))
streamBlockToPart _model (StreamTool cid name args) =
  toolCallPart (mkToolCall cid name args)

data BlockStart
  = StartText
  | StartThinking
  | StartRedacted Text
  | StartTool Text Text

parseMessageStartUsage :: Value -> Parser Usage
parseMessageStartUsage = withObject "message_start" $ \o -> do
  msg <- o .: "message"
  withObject "message" (\mo -> do u <- mo .: "usage"; withObject "usage" parseClaudeUsageObject u) msg

parseMessageStartModel :: Value -> Parser Text
parseMessageStartModel = withObject "message_start" $ \o -> do
  msg <- o .: "message"
  withObject "message" (.: "model") msg

parseMessageDeltaUsage :: Value -> Parser Int
parseMessageDeltaUsage = withObject "message_delta" $ \o -> do
  u <- o .: "usage"
  withObject "usage" (.: "output_tokens") u

parseContentBlockStart :: Value -> Parser BlockStart
parseContentBlockStart = withObject "content_block_start" $ \o -> do
  cb <- o .: "content_block"
  withObject
    "content_block"
    ( \cbo -> do
        typ <- cbo .: "type" :: Parser Text
        case typ of
          "tool_use" -> StartTool <$> cbo .: "id" <*> cbo .: "name"
          "thinking" -> pure StartThinking
          "redacted_thinking" -> StartRedacted <$> (cbo .:? "data" .!= "")
          "text" -> pure StartText
          _ -> fail "unknown block start"
    )
    cb

parseTextDelta :: Value -> Parser Text
parseTextDelta = withObject "delta_event" $ \o -> do
  d <- o .: "delta"
  withObject
    "delta"
    ( \d' -> do
        typ <- d' .: "type" :: Parser Text
        case typ of
          "text_delta" -> d' .: "text"
          _ -> fail "not text_delta"
    )
    d

parseThinkingDelta :: Value -> Parser Text
parseThinkingDelta = withObject "delta_event" $ \o -> do
  d <- o .: "delta"
  withObject
    "delta"
    ( \d' -> do
        typ <- d' .: "type" :: Parser Text
        case typ of
          "thinking_delta" -> d' .: "thinking"
          _ -> fail "not thinking_delta"
    )
    d

parseSignatureDelta :: Value -> Parser Text
parseSignatureDelta = withObject "delta_event" $ \o -> do
  d <- o .: "delta"
  withObject
    "delta"
    ( \d' -> do
        typ <- d' .: "type" :: Parser Text
        case typ of
          "signature_delta" -> d' .: "signature"
          _ -> fail "not signature_delta"
    )
    d

parseInputJsonDelta :: Value -> Parser Text
parseInputJsonDelta = withObject "delta_event" $ \o -> do
  d <- o .: "delta"
  withObject
    "delta"
    ( \d' -> do
        typ <- d' .: "type" :: Parser Text
        case typ of
          "input_json_delta" -> d' .: "partial_json"
          _ -> fail "not input_json_delta"
    )
    d
