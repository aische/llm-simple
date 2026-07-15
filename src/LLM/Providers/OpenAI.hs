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
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import Data.Aeson.Types (Pair, Parser, parseMaybe)
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (forM_)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isNothing)
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
        reqTools
      ),
    ChatResponse (ChatResponse, respContent, respReasoning, respText, respUsage),
    ContentBlock (..),
    LLMError (EmptyResponse),
    LLMGateway,
    LLMTextResult,
    MessageEncodeOptions (..),
    StreamEvent (..),
    ToolCall (..),
    mkToolCall,
    ToolDef (toolDescription, toolName, toolParameters),
    ToolResult (trCallId, trContent),
    Turn (..),
    defaultMessageEncodeOptions,
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
    ++ ["tools" .= map encodeToolDef r.reqTools | not (null r.reqTools)]
    ++ ["stream" .= True | stream]
    ++ ["stream_options" .= object ["include_usage" .= True] | stream]

buildMessages :: MessageEncodeOptions -> ChatRequest -> [Value]
buildMessages opts r =
  maybe [] (\sys -> [object ["role" .= ("system" :: Text), "content" .= sys]]) r.reqSystem
    ++ concatMap (encodeTurn opts) r.reqConversation

encodeTurn :: MessageEncodeOptions -> Turn -> [Value]
encodeTurn _ (UserTurn content) =
  [ object
      [ "role" .= ("user" :: Text),
        "content" .= content
      ]
  ]
encodeTurn opts (AssistantTurn text mReasoning calls) =
  [ object $
      ["role" .= ("assistant" :: Text)]
        ++ ["content" .= text | not (T.null text)]
        ++ [ "reasoning_content" .= rc
             | opts.meoIncludeReasoning,
               Just rc <- [mReasoning],
               not (T.null rc)
           ]
        ++ ["tool_calls" .= map encodeToolCall calls | not (null calls)]
  ]
encodeTurn _ (ToolTurn results) =
  map encodeToolResult results

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
  Just (mReasoning, blocks) ->
    if null blocks && isNothing mReasoning
      then Left EmptyResponse
      else
        let text = T.concat [t | TextBlock t <- blocks]
         in Right
              ChatResponse
                { respText = text,
                  respContent = blocks,
                  respUsage = parseOpenAIUsage v,
                  respReasoning = mReasoning
                }
  where
    go :: Value -> Parser (Maybe Text, [ContentBlock])
    go = withObject "OpenAIResponse" $ \o -> do
      (choice : _) <- o .: "choices" :: Parser [Value]
      withObject
        "choice"
        ( \co -> do
            msg <- co .: "message"
            withObject "message" parseMessage msg
        )
        choice

    parseMessage :: Object -> Parser (Maybe Text, [ContentBlock])
    parseMessage mo = do
      mReasoning <- mo .:? "reasoning_content" :: Parser (Maybe Text)
      contentBlocks <- do
        mc <- mo .:? "content" :: Parser (Maybe Text)
        pure [TextBlock t | Just t <- [mc], not (T.null t)]
      toolBlocks <- do
        tcs <- mo .:? "tool_calls" .!= [] :: Parser [Value]
        mapM parseToolCall tcs
      pure (mReasoning, contentBlocks ++ toolBlocks)

    parseToolCall :: Value -> Parser ContentBlock
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
            pure $ ToolCallBlock (mkToolCall cid name args)
        )
        fn

parseOpenAIUsage :: Value -> Maybe Usage
parseOpenAIUsage = parseMaybe $ withObject "OpenAIResponse" $ \o -> do
  u <- o .: "usage"
  withObject "usage" (\uo -> Usage <$> uo .: "prompt_tokens" <*> uo .: "completion_tokens" <*> pure 0) u

-- Streaming

parseOpenAIStream :: HC.BodyReader -> (StreamEvent -> IO ()) -> IO LLMTextResult
parseOpenAIStream reader callback = do
  blocksRef <- newIORef ([] :: [ContentBlock])
  reasoningRef <- newIORef Nothing
  usageRef <- newIORef Nothing
  -- Track in-flight tool calls: index -> (id, name, accumulated args)
  toolAccRef <- newIORef ([] :: [(Int, Text, Text, Text)])
  readSSEEvents (HC.brRead reader) $ \sse -> do
    let raw = sse.sseData
    if raw == "[DONE]"
      then pure ()
      else case decodeStrict' (encodeUtf8 raw) of
        Nothing -> pure ()
        Just v -> do
          -- Reasoning deltas
          case parseMaybe parseStreamReasoningDelta v of
            Just (Just txt) | not (T.null txt) -> do
              modifyIORef' reasoningRef (Just . maybe txt (<> txt))
              callback (StreamReasoningDelta txt)
            _ -> pure ()
          -- Text deltas
          case parseMaybe parseStreamTextDelta v of
            Just txt | not (T.null txt) -> do
              modifyIORef' blocksRef (TextBlock txt :)
              callback (StreamDelta txt)
            _ -> pure ()
          -- Tool call deltas
          case parseMaybe parseStreamToolDelta v of
            Just (idx, mId, mName, argChunk) -> do
              modifyIORef' toolAccRef $ \acc ->
                case lookup idx [(i, (i, cid, n, a)) | (i, cid, n, a) <- acc] of
                  Nothing ->
                    -- New tool call
                    let cid = fromMaybe "" mId
                        n = fromMaybe "" mName
                     in acc ++ [(idx, cid, n, argChunk)]
                  Just (_, cid, n, a) ->
                    -- Accumulate arguments
                    [(if i == idx then (i, cid, n, a <> argChunk) else entry) | entry@(i, _, _, _) <- acc]
            Nothing -> pure ()
          -- Finish reason: emit accumulated tool calls
          case parseMaybe parseFinishReason v of
            Just "tool_calls" -> do
              tools <- readIORef toolAccRef
              forM_ tools $ \(_, cid, name, argsStr) -> do
                let args = case decodeStrict' (encodeUtf8 argsStr) of
                      Just a -> a
                      Nothing -> String argsStr
                    tc = mkToolCall cid name args
                modifyIORef' blocksRef (ToolCallBlock tc :)
                callback (StreamToolCall tc)
              writeIORef toolAccRef []
            _ -> pure ()
          -- Usage (in the final chunk when stream_options.include_usage is set)
          case parseMaybe parseStreamUsage v of
            Just u -> writeIORef usageRef (Just u)
            Nothing -> pure ()
  -- Flush any remaining tool calls
  tools <- readIORef toolAccRef
  forM_ tools $ \(_, cid, name, argsStr) -> do
    let args = case decodeStrict' (encodeUtf8 argsStr) of
          Just a -> a
          Nothing -> String argsStr
        tc = mkToolCall cid name args
    modifyIORef' blocksRef (ToolCallBlock tc :)
    callback (StreamToolCall tc)
  blocks <- reverse <$> readIORef blocksRef
  mReasoning <- readIORef reasoningRef
  usage <- readIORef usageRef
  let text = T.concat [t | TextBlock t <- blocks]
  if null blocks && isNothing mReasoning
    then pure $ Left EmptyResponse
    else
      pure $
        Right
          ChatResponse
            { respText = text,
              respContent = blocks,
              respUsage = usage,
              respReasoning = mReasoning
            }

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
  withObject "usage" (\uo -> Usage <$> uo .: "prompt_tokens" <*> uo .: "completion_tokens" <*> pure 0) u
