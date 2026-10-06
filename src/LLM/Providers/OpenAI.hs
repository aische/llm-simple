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
    Value (String),
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
import Data.Foldable (forM_)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import LLM.Core.LLMProvider (LLMProvider (..), toGateway)
import LLM.Core.ProviderUtils (handleStreamResponse, lenientConfig, normalizeSchemaOpenAI, stripJsonFences)
import LLM.Core.SSE (SSEEvent (sseData), readSSEEvents)
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
    ContentPart (..),
    LLMError (EmptyResponse),
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
      buildBody = openAIBuildBody,
      sendRequest = sendRequest,
      sendStreamRequest = \body callback ->
        runReq lenientConfig $ do
          let url = baseUrl /: "v1" /: "chat" /: "completions"
              opts = baseOpts <> authHeader apiKey
          reqBr POST url (ReqBodyJson body) opts $ \resp ->
            handleStreamResponse resp (`parseOpenAIStream` callback),
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
   in [ object $
          ["role" .= ("assistant" :: Text)]
            ++ ["content" .= text | not (T.null text)]
            ++ [ "reasoning_content" .= rc
                 | opts.meoIncludeReasoning,
                   Just rc <- [mReasoning]
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
  Just parts ->
    if null parts
      then Left EmptyResponse
      else Right (mkChatResponse parts (parseOpenAIUsage v))
  where
    go :: Value -> Parser [ContentPart]
    go = withObject "OpenAIResponse" $ \o -> do
      (choice : _) <- o .: "choices" :: Parser [Value]
      withObject
        "choice"
        ( \co -> do
            msg <- co .: "message"
            withObject "message" parseMessage msg
        )
        choice

    parseMessage :: Object -> Parser [ContentPart]
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
      pure (thinkingBlocks ++ contentBlocks ++ toolBlocks)

    parseToolCall :: Value -> Parser ContentPart
    parseToolCall = withObject "tool_call" $ \tc -> do
      cid <- tc .: "id"
      fn <- tc .: "function"
      withObject
        "function"
        ( \f -> do
            name <- f .: "name"
            argsStr <- f .: "arguments" :: Parser Text
            let args = case decodeStrict' (encodeUtf8 argsStr) of
                  Just v' -> v'
                  Nothing -> String argsStr
            pure $ toolCallPart (mkToolCall cid name args)
        )
        fn

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
  toolAccRef <- newIORef ([] :: [(Int, Text, Text, Text)])
  readSSEEvents (HC.brRead reader) $ \sse -> do
    let raw = sse.sseData
    if raw == "[DONE]"
      then pure ()
      else case decodeStrict' (encodeUtf8 raw) of
        Nothing -> pure ()
        Just v -> do
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
          case parseMaybe parseStreamToolDelta v of
            Just (idx, mId, mName, argChunk) -> do
              modifyIORef' toolAccRef $ \acc ->
                case lookup idx [(i, (i, cid, n, a)) | (i, cid, n, a) <- acc] of
                  Nothing ->
                    let cid = fromMaybe "" mId
                        n = fromMaybe "" mName
                     in acc ++ [(idx, cid, n, argChunk)]
                  Just (_, cid, n, a) ->
                    [(if i == idx then (i, cid, n, a <> argChunk) else entry) | entry@(i, _, _, _) <- acc]
            Nothing -> pure ()
          case parseMaybe parseFinishReason v of
            Just "tool_calls" -> do
              tools <- readIORef toolAccRef
              forM_ tools $ \(_, cid, name, argsStr) -> do
                let args = case decodeStrict' (encodeUtf8 argsStr) of
                      Just a -> a
                      Nothing -> String argsStr
                    tc = mkToolCall cid name args
                modifyIORef' blocksRef (toolCallPart tc :)
                callback (StreamToolCall tc)
              writeIORef toolAccRef []
            _ -> pure ()
          case parseMaybe parseStreamUsage v of
            Just u -> writeIORef usageRef (Just u)
            Nothing -> pure ()
  tools <- readIORef toolAccRef
  forM_ tools $ \(_, cid, name, argsStr) -> do
    let args = case decodeStrict' (encodeUtf8 argsStr) of
          Just a -> a
          Nothing -> String argsStr
        tc = mkToolCall cid name args
    modifyIORef' blocksRef (toolCallPart tc :)
    callback (StreamToolCall tc)
  textBlocks <- reverse <$> readIORef blocksRef
  mReasoning <- readIORef reasoningRef
  usage <- readIORef usageRef
  let thinkingBlocks =
        [ thinkingPart (ThinkingContent (Just rc) Nothing)
          | Just rc <- [mReasoning],
            not (T.null rc)
        ]
      parts = thinkingBlocks ++ textBlocks
  if null parts
    then pure $ Left EmptyResponse
    else pure $ Right (mkChatResponse parts usage)

parseStreamReasoningDelta :: Value -> Parser (Maybe Text)
parseStreamReasoningDelta = withObject "chunk" $ \o -> do
  (c : _) <- o .: "choices" :: Parser [Value]
  withObject "choice" (\co -> do d <- co .: "delta"; withObject "delta" (.:? "reasoning_content") d) c

parseStreamTextDelta :: Value -> Parser Text
parseStreamTextDelta = withObject "chunk" $ \o -> do
  (c : _) <- o .: "choices" :: Parser [Value]
  withObject "choice" (\co -> do d <- co .: "delta"; withObject "delta" (.: "content") d) c

parseStreamToolDelta :: Value -> Parser (Int, Maybe Text, Maybe Text, Text)
parseStreamToolDelta = withObject "chunk" $ \o -> do
  (c : _) <- o .: "choices" :: Parser [Value]
  withObject
    "choice"
    ( \co -> do
        d <- co .: "delta"
        withObject
          "delta"
          ( \d' -> do
              (tc : _) <- d' .: "tool_calls" :: Parser [Value]
              withObject
                "tool_call"
                ( \tco -> do
                    idx <- tco .: "index"
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
                )
                tc
          )
          d
    )
    c

parseFinishReason :: Value -> Parser Text
parseFinishReason = withObject "chunk" $ \o -> do
  (c : _) <- o .: "choices" :: Parser [Value]
  withObject "choice" (.: "finish_reason") c

parseStreamUsage :: Value -> Parser Usage
parseStreamUsage = withObject "chunk" $ \o -> do
  u <- o .: "usage"
  withObject "usage" parseOpenAIUsageObject u
