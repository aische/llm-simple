module LLM.Providers.OpenAI
  ( openAIGateway,
    openAIGatewayWith,
    openAIGatewayWithName,
    openAIProvider,
    openAIProviderWith,
    openAIProviderWithName,
    parseOpenAIResponse,
    parseOpenAIUsage,
    buildMessages,
    encodeTurn,
    encodeToolDef,
    parseOpenAIStream,
    openAIBuildBody,
    openAIBuildBodyPairs,
    authHeader,
  )
where

import Data.Aeson
  ( KeyValue ((.=)),
    Object,
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
import Data.Aeson.Types (Pair, Parser, parseMaybe)
import Data.ByteString.Lazy qualified as BSL
import Control.Monad (when, zipWithM)
import Data.Either (lefts, rights)
import Data.Foldable (foldl', for_, forM_)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import LLM.Core.LLMProvider (LLMProvider (..), toGateway)
import LLM.Core.ProviderUtils (decodeToolArguments, handleStreamResponse, invalidToolArgumentsError, lenientConfig, normalizeSchemaOpenAI, stripJsonFences)
import LLM.Core.SSE (SSEEvent (sseData), readSSEEvents)
import LLM.Core.Types
  ( commonImageMediaTypes,
    ChatRequest
      ( reqConversation,
        reqMaxTokens,
        reqModel,
        reqSystem,
        reqTemperature,
        reqThinking,
        reqTools
      ),
    ContentPart (..),
    LLMError (EmptyResponse, HttpError),
    LLMGateway,
    LLMTextResult,
    MessageEncodeOptions (..),
    PartBody (..),
    StreamEvent (..),
    ThinkingContent (..),
    ThinkingMode (..),
    ToolCall (..),
    ToolDef (toolDescription, toolName, toolParameters),
    ToolResult (trCallId, trContent),
    Turn (..),
    ImageSource (..),
    defaultMessageEncodeOptions,
    mkChatResponse,
    mkToolCall,
    stripForeignOpaque,
    textPart,
    thinkingPart,
    toolCallPart,
  )
import LLM.Core.Usage (Usage (..))
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

openAIProviderName :: Text
openAIProviderName = "openai"

-- | Create an OpenAI client for api.openai.com. Takes the API key as a parameter.
openAIGateway :: Text -> LLMGateway
openAIGateway apiKey = toGateway $ openAIProvider apiKey

-- | Create an OpenAI-compatible client with a custom base URL.
openAIGatewayWith :: Url scheme -> Option scheme -> Text -> LLMGateway
openAIGatewayWith = openAIGatewayWithName "openai"

-- | Create an OpenAI-compatible client with a custom provider name and base URL.
openAIGatewayWithName :: Text -> Url scheme -> Option scheme -> Text -> LLMGateway
openAIGatewayWithName name baseUrl baseOpts apiKey = toGateway (openAIProviderWithName name baseUrl baseOpts apiKey)

-- | Create an OpenAI provider. Takes the API key as a parameter.
openAIProvider :: Text -> LLMProvider
openAIProvider = openAIProviderWith (https "api.openai.com") mempty

openAIProviderWith :: Url scheme -> Option scheme -> Text -> LLMProvider
openAIProviderWith = openAIProviderWithName "openai"

openAIProviderWithName :: Text -> Url scheme -> Option scheme -> Text -> LLMProvider
openAIProviderWithName name baseUrl baseOpts apiKey =
  LLMProvider
    { providerName = name,
      providerImageMediaTypes = commonImageMediaTypes,
      providerImageUrlSupport = True,
      buildBody = openAIBuildBody,
      sendRequest = sendRequest,
      openStream = \body k ->
        runReq lenientConfig $ do
          let url = baseUrl /: "v1" /: "chat" /: "completions"
              opts = baseOpts <> authHeader apiKey
          reqBr POST url (ReqBodyJson body) opts $ \resp ->
            handleStreamResponse resp k,
      parseStream = const parseOpenAIStream,
      parseResponse = pure . parseOpenAIResponse,
      buildObjectBody = \r schema ->
        object $
          openAIBuildBodyPairs False r
            <> [ "response_format"
                   .= object
                     [ "type" .= ("json_schema" :: Text),
                       "json_schema"
                         .= object
                           [ "name" .= ("response" :: Text),
                             "schema" .= normalizeSchemaOpenAI schema,
                             "strict" .= True
                           ]
                     ]
               ],
      sendObjectRequest = sendRequest,
      parseObjectResponse = \v ->
        let parseObject :: Value -> Parser Text
            parseObject = withObject "OpenAIObjectResponse" $ \o -> do
              (choice : _) <- o .: "choices" :: Parser [Value]
              withObject "choice" (\co -> co .: "message" >>= withObject "message" (.: "content")) choice
         in case parseMaybe parseObject v of
              Nothing -> pure $ Left EmptyResponse
              Just contentStr -> case decodeStrict' (encodeUtf8 (stripJsonFences contentStr)) of
                Nothing -> pure $ Left EmptyResponse
                Just obj -> pure $ Right (obj, parseOpenAIUsage v)
    }
  where
    sendRequest body =
      runReq lenientConfig $ do
        let url = baseUrl /: "v1" /: "chat" /: "completions"
            opts = baseOpts <> authHeader apiKey
        resp <- req POST url (ReqBodyJson body) jsonResponse opts
        pure (responseStatusCode resp, responseBody resp)

authHeader :: Text -> Option scheme
authHeader apiKey
  | T.null apiKey = mempty
  | otherwise = header "Authorization" ("Bearer " <> encodeUtf8 apiKey)

-- Request body

openAIBuildBody :: Bool -> ChatRequest -> Value
openAIBuildBody stream r = object $ openAIBuildBodyPairs stream r

openAIBuildBodyPairs :: Bool -> ChatRequest -> [Pair]
openAIBuildBodyPairs stream r =
  [ "model" .= r.reqModel,
    "max_completion_tokens" .= r.reqMaxTokens,
    "messages" .= buildMessages defaultMessageEncodeOptions r
  ]
    ++ ["temperature" .= t | Just t <- [r.reqTemperature]]
    ++ effortPairs r
    ++ ["tools" .= map encodeToolDef r.reqTools | not (null r.reqTools)]
    ++ ["stream" .= True | stream]
    ++ ["stream_options" .= object ["include_usage" .= True] | stream]

-- | Chat Completions reasoning_effort, when the catalog requests thinking.
effortPairs :: ChatRequest -> [Pair]
effortPairs r =
  case r.reqThinking of
    Just tm
      | tm.tmEnabled,
        Just e <- tm.tmEffort ->
          ["reasoning_effort" .= e]
    _ -> []

buildMessages :: MessageEncodeOptions -> ChatRequest -> [Value]
buildMessages opts r =
  maybe [] (\sys -> [object ["role" .= ("system" :: Text), "content" .= sys]]) r.reqSystem
    ++ concatMap (encodeTurn opts) r.reqConversation

encodeTurn :: MessageEncodeOptions -> Turn -> [Value]
encodeTurn _ (UserMessage parts) =
  [ object
      [ "role" .= ("user" :: Text),
        "content" .= encodeUserContent parts
      ]
  ]
encodeTurn opts (AssistantMessage parts) =
  let cleaned = map (stripForeignOpaque openAIProviderName) parts
      -- Cache hints are ignored on the OpenAI wire; content is unchanged.
      text = T.concat [t | ContentPart (TextPart t) _ <- cleaned]
      mReasoning =
        listToMaybe
          [ t
            | ContentPart (ThinkingPart tc) _ <- cleaned,
              Just t <- [tc.thinkingText],
              not (T.null t)
          ]
      calls = [tc | ContentPart (ToolCallPart tc) _ <- cleaned]
      requireReasoning
        | opts.meoRequireReasoning = Just . fromMaybe ""
        | otherwise = id
   in [ object $
          ["role" .= ("assistant" :: Text)]
            -- Without tool calls, send content even when empty: OpenAI
            -- rejects an assistant message with neither (e.g. a Claude turn
            -- that held only thinking).
            ++ ["content" .= text | not (T.null text) || null calls]
            ++ [ "reasoning_content" .= rc
                 | opts.meoIncludeReasoning,
                   Just rc <- [requireReasoning mReasoning]
               ]
            ++ ["tool_calls" .= map encodeToolCall calls | not (null calls)]
      ]
encodeTurn _ (ToolTurn results) =
  map encodeToolResult results

-- | Single text stays a string (recorded-fixture compatible); mixed or image
-- content uses the OpenAI multimodal content-part array.
-- Cache hints are omitted from the wire JSON.
encodeUserContent :: [ContentPart] -> Value
encodeUserContent [ContentPart (TextPart t) _] = String t
encodeUserContent parts = toJSON (mapMaybe encodeUserPart parts)
  where
    encodeUserPart (ContentPart (TextPart t) _) =
      Just $ object ["type" .= ("text" :: Text), "text" .= t]
    encodeUserPart (ContentPart (ImagePart src) _) =
      Just $ encodeImagePart src
    encodeUserPart _ = Nothing

encodeImagePart :: ImageSource -> Value
encodeImagePart (ImageUrl url) =
  object
    [ "type" .= ("image_url" :: Text),
      "image_url" .= object ["url" .= url]
    ]
encodeImagePart (ImageBase64 mediaType data_) =
  object
    [ "type" .= ("image_url" :: Text),
      "image_url"
        .= object
          [ "url" .= ("data:" <> mediaType <> ";base64," <> data_)
          ]
    ]

encodeToolDef :: ToolDef -> Value
encodeToolDef td =
  object
    [ "type" .= ("function" :: Text),
      "function"
        .= object
          [ "name" .= td.toolName,
            "description" .= td.toolDescription,
            "parameters" .= td.toolParameters
          ]
    ]

encodeToolCall :: ToolCall -> Value
encodeToolCall tc =
  object
    [ "id" .= tc.tcId,
      "type" .= ("function" :: Text),
      "function"
        .= object
          [ "name" .= tc.tcName,
            "arguments" .= decodeUtf8 (BSL.toStrict (encode tc.tcArguments))
          ]
    ]

encodeToolResult :: ToolResult -> Value
encodeToolResult tr =
  object
    [ "role" .= ("tool" :: Text),
      "tool_call_id" .= tr.trCallId,
      "content" .= tr.trContent
    ]

-- Response parsing

parseOpenAIResponse :: Value -> LLMTextResult
parseOpenAIResponse v = case parseMaybe go v of
  Nothing -> Left EmptyResponse
  Just (finishReason, parts) ->
    case lefts parts of
      name : _ -> Left (invalidToolArgumentsError name (truncationReason finishReason))
      []
        | null parts -> Left EmptyResponse
        | otherwise -> Right (mkChatResponse (rights parts) (parseOpenAIUsage v))
  where
    go :: Value -> Parser (Maybe Text, [Either Text ContentPart])
    go = withObject "OpenAIResponse" $ \o -> do
      (choice : _) <- o .: "choices" :: Parser [Value]
      withObject
        "choice"
        ( \co -> do
            finishReason <- co .:? "finish_reason"
            msg <- co .: "message"
            parts <- withObject "message" parseMessage msg
            pure (finishReason, parts)
        )
        choice

    -- Tool calls with invalid JSON arguments come back as 'Left' tool name.
    parseMessage :: Object -> Parser [Either Text ContentPart]
    parseMessage mo = do
      mReasoning <- mo .:? "reasoning_content" :: Parser (Maybe Text)
      contentBlocks <- do
        mc <- mo .:? "content" :: Parser (Maybe Text)
        pure [textPart t | Just t <- [mc], not (T.null t)]
      toolBlocks <- do
        tcs <- mo .:? "tool_calls" .!= [] :: Parser [Value]
        mapM parseToolCall tcs
      let thinkingBlocks =
            [ thinkingPart (ThinkingContent (Just rc) Nothing)
              | Just rc <- [mReasoning],
                not (T.null rc)
            ]
      pure (map Right (thinkingBlocks ++ contentBlocks) ++ toolBlocks)

    parseToolCall :: Value -> Parser (Either Text ContentPart)
    parseToolCall = withObject "tool_call" $ \tc -> do
      cid <- tc .: "id"
      fn <- tc .: "function"
      withObject
        "function"
        ( \f -> do
            name <- f .: "name"
            argsStr <- f .: "arguments" :: Parser Text
            pure $ case decodeToolArguments argsStr of
              Just args -> Right (toolCallPart (mkToolCall cid name args))
              Nothing -> Left name
        )
        fn

-- | The finish reason, if it says the response hit the token limit.
truncationReason :: Maybe Text -> Maybe Text
truncationReason r = if r == Just "length" then r else Nothing

parseOpenAIUsage :: Value -> Maybe Usage
parseOpenAIUsage = parseMaybe $ withObject "OpenAIResponse" $ \o -> do
  u <- o .: "usage"
  withObject "usage" parseOpenAIUsageObject u

-- | Shared OpenAI Chat Completions / DeepSeek usage shape.
--
-- @prompt_tokens@ is already the total input (including cache hits). Cache
-- reads come from DeepSeek's @prompt_cache_hit_tokens@ or OpenAI's
-- @prompt_tokens_details.cached_tokens@. Optional @cache_write_tokens@ map to
-- creation counters.
parseOpenAIUsageObject :: Object -> Parser Usage
parseOpenAIUsageObject uo = do
  prompt <- uo .: "prompt_tokens"
  completion <- uo .: "completion_tokens"
  cacheReadDeepSeek <- uo .:? "prompt_cache_hit_tokens"
  details <- uo .:? "prompt_tokens_details" :: Parser (Maybe Value)
  (cacheReadDetails, cacheWrite) <- case details of
    Nothing -> pure (Nothing, Nothing)
    Just d ->
      withObject
        "prompt_tokens_details"
        ( \dto ->
            (,)
              <$> dto .:? "cached_tokens"
              <*> dto .:? "cache_write_tokens"
        )
        d
  let cacheRead = fromMaybe 0 (cacheReadDeepSeek <|> cacheReadDetails)
      cacheCreate = fromMaybe 0 cacheWrite
  pure
    Usage
      { usageInputTokens = prompt,
        usageOutputTokens = completion,
        usageCacheReadTokens = cacheRead,
        usageCacheCreationTokens = cacheCreate,
        usageTotalCost = 0
      }

-- Streaming

parseOpenAIStream :: HC.BodyReader -> (StreamEvent -> IO ()) -> IO LLMTextResult
parseOpenAIStream reader callback = do
  blocksRef <- newIORef ([] :: [ContentPart])
  reasoningRef <- newIORef Nothing
  usageRef <- newIORef Nothing
  -- Tool calls being assembled, keyed by index: (id, name, argument text).
  toolAccRef <- newIORef (Map.empty :: Map.Map Int (Text, Text, Text))
  errorRef <- newIORef (Nothing :: Maybe LLMError)
  finishReasonRef <- newIORef (Nothing :: Maybe Text)
  badToolRef <- newIORef (Nothing :: Maybe Text) -- first tool with invalid JSON args
  let flushTools = do
        tools <- readIORef toolAccRef
        forM_ (Map.elems tools) $ \(cid, name, argsStr) ->
          case decodeToolArguments argsStr of
            Just args -> do
              let tc = mkToolCall cid name args
              modifyIORef' blocksRef (toolCallPart tc :)
              callback (StreamToolCall tc)
            Nothing -> modifyIORef' badToolRef (<|> Just name)
        writeIORef toolAccRef Map.empty
  readSSEEvents (HC.brRead reader) $ \sse -> do
    let raw = sse.sseData
    if raw == "[DONE]"
      then pure ()
      else case decodeStrict' (encodeUtf8 raw) of
        Nothing -> pure ()
        Just v -> do
          for_ (parseMaybe parseStreamError v) $ \status ->
            modifyIORef' errorRef (<|> Just (HttpError status raw))
          case parseMaybe parseStreamReasoningDelta v of
            Just (Just txt) | not (T.null txt) -> do
              modifyIORef' reasoningRef (Just . maybe txt (<> txt))
              callback (StreamReasoningDelta txt)
            _ -> pure ()
          case parseMaybe parseStreamTextDelta v of
            Just txt | not (T.null txt) -> do
              modifyIORef' blocksRef (textPart txt :)
              callback (StreamDelta txt)
            _ -> pure ()
          for_ (parseMaybe parseStreamToolDeltas v) $ \deltas ->
            modifyIORef' toolAccRef (\acc -> foldl' addToolDelta acc deltas)
          case parseMaybe parseFinishReason v of
            Just reason -> do
              writeIORef finishReasonRef (Just reason)
              when (reason == "tool_calls") flushTools
            Nothing -> pure ()
          case parseMaybe parseStreamUsage v of
            Just u -> writeIORef usageRef (Just u)
            Nothing -> pure ()
  flushTools
  textBlocks <- reverse <$> readIORef blocksRef
  mReasoning <- readIORef reasoningRef
  usage <- readIORef usageRef
  let thinkingBlocks =
        [ thinkingPart (ThinkingContent (Just rc) Nothing)
          | Just rc <- [mReasoning],
            not (T.null rc)
        ]
      parts = thinkingBlocks ++ textBlocks
  mError <- readIORef errorRef
  mBadTool <- readIORef badToolRef
  finishReason <- readIORef finishReasonRef
  pure $ case (mError, mBadTool) of
    (Just err, _) -> Left err
    (Nothing, Just name) -> Left (invalidToolArgumentsError name (truncationReason finishReason))
    _
      | null parts -> Left EmptyResponse
      | otherwise -> Right (mkChatResponse parts usage)

-- | A mid-stream @{"error": ...}@ chunk, mapped to an HTTP status so
-- 'LLM.Core.Utils.isRetryable' treats it like the same error sent before
-- streaming started. Uses a numeric @code@/@status@ when the server sends
-- one; otherwise 429 for rate limits, 503 for overload, else 500.
parseStreamError :: Value -> Parser Int
parseStreamError = withObject "chunk" $ \o -> do
  e <- o .: "error"
  case e of
    String msg -> pure (classify [msg])
    Object eo -> do
      mCode <- eo .:? "code" <|> pure Nothing
      mStatus <- eo .:? "status" <|> pure Nothing
      texts <- mapM (\k -> eo .:? k <|> pure Nothing) ["type", "code", "message"]
      pure $ case filter (\c -> c >= 400 && c < 600) (catMaybes [mCode, mStatus]) of
        c : _ -> c
        [] -> classify (catMaybes texts)
    _ -> fail "not an error"
  where
    classify ts
      | any (hasWord "rate") ts = 429
      | any (hasWord "overload") ts = 503
      | otherwise = 500
    hasWord w t = w `T.isInfixOf` T.toLower t

parseStreamReasoningDelta :: Value -> Parser (Maybe Text)
parseStreamReasoningDelta = withObject "chunk" $ \o -> do
  (c : _) <- o .: "choices" :: Parser [Value]
  withObject "choice" (\co -> do d <- co .: "delta"; withObject "delta" (.:? "reasoning_content") d) c

parseStreamTextDelta :: Value -> Parser Text
parseStreamTextDelta = withObject "chunk" $ \o -> do
  (c : _) <- o .: "choices" :: Parser [Value]
  withObject "choice" (\co -> do d <- co .: "delta"; withObject "delta" (.: "content") d) c

-- | One streamed tool-call fragment: index, id, name, argument text.
type ToolDelta = (Int, Maybe Text, Maybe Text, Text)

-- | Merge a fragment into the accumulated tool calls. A fragment whose id
-- differs from the call already at its index starts a new call (servers
-- that omit @index@ would otherwise merge distinct calls).
addToolDelta :: Map.Map Int (Text, Text, Text) -> ToolDelta -> Map.Map Int (Text, Text, Text)
addToolDelta acc (idx, mId, mName, argChunk) =
  case Map.lookup idx acc of
    Just (cid, n, a)
      | maybe True (== cid) mId || T.null cid ->
          Map.insert idx (fromMaybe cid mId, fromMaybe n mName, a <> argChunk) acc
      | otherwise -> Map.insert (nextIndex acc) new acc
    Nothing -> Map.insert idx new acc
  where
    new = (fromMaybe "" mId, fromMaybe "" mName, argChunk)
    nextIndex m = maybe 0 ((+ 1) . fst) (Map.lookupMax m)

-- | All tool-call fragments in a chunk. Ollama may send several complete
-- calls in one chunk and may omit @index@; the position is used then.
parseStreamToolDeltas :: Value -> Parser [ToolDelta]
parseStreamToolDeltas = withObject "chunk" $ \o -> do
  (c : _) <- o .: "choices" :: Parser [Value]
  withObject
    "choice"
    ( \co -> do
        d <- co .: "delta"
        withObject
          "delta"
          ( \d' -> do
              tcs <- d' .: "tool_calls" :: Parser [Value]
              zipWithM parseOne [0 ..] tcs
          )
          d
    )
    c
  where
    parseOne pos = withObject "tool_call" $ \tco -> do
      idx <- tco .:? "index" .!= pos
      mId <- tco .:? "id"
      fn <- tco .:? "function" .!= object []
      withObject
        "function"
        ( \f -> do
            mName <- f .:? "name"
            args <- f .:? "arguments" .!= ""
            pure (idx, mId, mName, args)
        )
        fn

parseFinishReason :: Value -> Parser Text
parseFinishReason = withObject "chunk" $ \o -> do
  (c : _) <- o .: "choices" :: Parser [Value]
  withObject "choice" (.: "finish_reason") c

parseStreamUsage :: Value -> Parser Usage
parseStreamUsage = withObject "chunk" $ \o -> do
  u <- o .: "usage"
  withObject "usage" parseOpenAIUsageObject u
