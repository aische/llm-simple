module LLM.Providers.Claude
  ( claudeGateway,
    claudeProvider,
    parseClaudeResponse,
    parseClaudeUsage,
  )
where

import Data.Aeson
  ( KeyValue ((.=)),
    Value (String),
    decodeStrict',
    encode,
    object,
    withObject,
    (.:),
  )
import Data.Aeson.Types (Parser, parseMaybe)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
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
        reqTools
      ),
    ChatResponse (ChatResponse),
    ContentBlock (..),
    LLMError (EmptyResponse),
    LLMGateway,
    LLMResult,
    LLMTextResult,
    StreamEvent (..),
    ToolCall (..),
    mkToolCall,
    ToolDef (toolDescription, toolName, toolParameters),
    ToolResult (trCallId, trContent),
    Turn (..),
  )
import LLM.Core.Usage (Usage (..), emptyUsage)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Req
  ( Option,
    POST (POST),
    ReqBodyJson (ReqBodyJson),
    Scheme (Https),
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

-- | Create a LLMGateway for the Claude provider. Takes the API key as a parameter.
claudeGateway :: Text -> LLMGateway
claudeGateway apiKey = toGateway $ claudeProvider apiKey

-- | Create a LLMProvider for the Claude provider. Takes the API key as a parameter.
claudeProvider :: Text -> LLMProvider
claudeProvider apiKey =
  LLMProvider
    { providerName = "claude",
      buildBody = claudeBuildBody,
      sendRequest = sendRequest,
      sendStreamRequest = \body callback ->
        runReq lenientConfig $
          reqBr POST claudeUrl (ReqBodyJson body) (claudeOpts apiKey) $ \resp ->
            handleStreamResponse resp (`parseClaudeStream` callback),
      parseResponse = pure . parseClaudeResponse,
      -- buildObjectBody _ r schema = claudeBuildBody False (r {reqConversation = reqConversation r <> Conversation [UserTurn ("Generate a JSON object matching this schema: " <> T.pack (show schema))]})
      buildObjectBody = \r schema ->
        let schemaText = TL.toStrict . decodeUtf8 $ encode schema
            instruction = "Respond with a raw JSON object matching this schema. No markdown, no explanation, no code fences:\n" <> schemaText
            conv' = r.reqConversation <> [UserTurn instruction]
         in claudeBuildBody False (r {reqConversation = conv'}),
      sendObjectRequest = sendRequest,
      -- parseObjectResponse _ = parseClaudeObjectResponse
      parseObjectResponse = parseClaudeObjectResponse
    }
  where
    sendRequest body =
      runReq lenientConfig $ do
        resp <- req POST claudeUrl (ReqBodyJson body) jsonResponse (claudeOpts apiKey)
        pure (responseStatusCode resp, responseBody resp)

-- Internal helpers

claudeUrl :: Url 'Https
claudeUrl = https "api.anthropic.com" /: "v1" /: "messages"

claudeOpts :: Text -> Option 'Https
claudeOpts apiKey =
  header "x-api-key" (encodeUtf8 apiKey)
    <> header "anthropic-version" "2023-06-01"

-- | Create an LLMClient from Claude credentials
-- claudeGateway :: Text -> LLMGateway
-- claudeGateway apiKey = toGateway (Claude apiKey)
parseClaudeStream :: HC.BodyReader -> (StreamEvent -> IO ()) -> IO LLMTextResult
parseClaudeStream reader callback = do
  blocksRef <- newIORef ([] :: [ContentBlock])
  usageRef <- newIORef emptyUsage
  -- For accumulating tool_use input JSON across deltas
  toolAccRef <- newIORef (Nothing :: Maybe (Text, Text, Text)) -- (id, name, json_so_far)
  readSSEEvents (HC.brRead reader) $ \sse -> do
    case sse.sseEvent of
      Just "message_start" ->
        -- Extract input token count from message.usage
        case decodeStrict' (encodeUtf8 sse.sseData) of
          Just v -> case parseMaybe parseMessageStartUsage v of
            Just inputToks -> modifyIORef' usageRef $ \u -> u {usageInputTokens = inputToks}
            Nothing -> pure ()
          Nothing -> pure ()
      Just "content_block_start" ->
        case decodeStrict' (encodeUtf8 sse.sseData) of
          Just v -> case parseMaybe parseContentBlockStart v of
            Just (cid, name) -> writeIORef toolAccRef (Just (cid, name, ""))
            Nothing -> pure () -- text block start, nothing to do
          Nothing -> pure ()
      Just "content_block_delta" ->
        case decodeStrict' (encodeUtf8 sse.sseData) of
          Just v -> do
            -- Try text delta
            case parseMaybe parseTextDelta v of
              Just txt -> do
                modifyIORef' blocksRef (TextBlock txt :)
                callback (StreamDelta txt)
              Nothing -> pure ()
            -- Try tool input delta
            case parseMaybe parseInputJsonDelta v of
              Just fragment ->
                modifyIORef' toolAccRef $ fmap (\(cid, name, acc) -> (cid, name, acc <> fragment))
              Nothing -> pure ()
          Nothing -> pure ()
      Just "content_block_stop" -> do
        mTool <- readIORef toolAccRef
        case mTool of
          Just (cid, name, jsonStr) | not (T.null jsonStr) -> do
            let args = case decodeStrict' (encodeUtf8 jsonStr) of
                  Just v -> v
                  Nothing -> String jsonStr
                tc = mkToolCall cid name args
            modifyIORef' blocksRef (ToolCallBlock tc :)
            callback (StreamToolCall tc)
            writeIORef toolAccRef Nothing
          _ -> writeIORef toolAccRef Nothing
      Just "message_delta" ->
        case decodeStrict' (encodeUtf8 sse.sseData) of
          Just v -> case parseMaybe parseMessageDeltaUsage v of
            Just outputToks -> modifyIORef' usageRef $ \u -> u {usageOutputTokens = outputToks}
            Nothing -> pure ()
          Nothing -> pure ()
      _ -> pure () -- message_stop, ping, etc.
  blocks <- reverse <$> readIORef blocksRef
  usage <- readIORef usageRef
  let text = T.concat [t | TextBlock t <- blocks]
  if null blocks
    then pure $ Left EmptyResponse
    else pure $ Right (ChatResponse text blocks (Just usage) Nothing)

-- Parsers for streaming events
parseMessageStartUsage :: Value -> Parser Int
parseMessageStartUsage = withObject "message_start" $ \o -> do
  msg <- o .: "message"
  withObject "message" (\mo -> do u <- mo .: "usage"; withObject "usage" (.: "input_tokens") u) msg

parseMessageDeltaUsage :: Value -> Parser Int
parseMessageDeltaUsage = withObject "message_delta" $ \o -> do
  u <- o .: "usage"
  withObject "usage" (.: "output_tokens") u

parseContentBlockStart :: Value -> Parser (Text, Text)
parseContentBlockStart = withObject "content_block_start" $ \o -> do
  cb <- o .: "content_block"
  withObject
    "content_block"
    ( \cbo -> do
        typ <- cbo .: "type" :: Parser Text
        case typ of
          "tool_use" -> (,) <$> cbo .: "id" <*> cbo .: "name"
          _ -> fail "not tool_use"
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

claudeBuildBody :: Bool -> ChatRequest -> Value
claudeBuildBody stream r =
  object $
    [ "model" .= r.reqModel,
      "max_tokens" .= r.reqMaxTokens,
      "messages" .= concatMap encodeTurn r.reqConversation
    ]
      ++ ["system" .= sys | Just sys <- [r.reqSystem]]
      ++ ["temperature" .= t | Just t <- [r.reqTemperature]]
      ++ ["tools" .= map encodeToolDef r.reqTools | not (null r.reqTools)]
      ++ ["stream" .= True | stream]

encodeTurn :: Turn -> [Value]
encodeTurn (UserTurn content) =
  [ object
      [ "role" .= ("user" :: Text),
        "content" .= content
      ]
  ]
encodeTurn (AssistantTurn text _mReasoning calls) =
  [ object
      [ "role" .= ("assistant" :: Text),
        "content" .= (textBlocks ++ toolBlocks)
      ]
  ]
  where
    textBlocks = [object ["type" .= ("text" :: Text), "text" .= text] | not (T.null text)]
    toolBlocks = map encodeToolUseBlock calls
encodeTurn (ToolTurn results) =
  [ object
      [ "role" .= ("user" :: Text),
        "content" .= map encodeToolResult results
      ]
  ]

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
parseClaudeResponse v = case parseMaybe go v of
  Nothing -> Left EmptyResponse
  Just blocks -> case blocks of
    [] -> Left EmptyResponse
    _ ->
      let text = T.concat [t | TextBlock t <- blocks]
       in Right (ChatResponse text blocks (parseClaudeUsage v) Nothing)
  where
    go :: Value -> Parser [ContentBlock]
    go = withObject "ClaudeResponse" $ \o -> do
      content <- o .: "content" :: Parser [Value]
      mapM parseBlock content

    parseBlock :: Value -> Parser ContentBlock
    parseBlock = withObject "content_block" $ \o -> do
      typ <- o .: "type" :: Parser Text
      case typ of
        "text" -> TextBlock <$> o .: "text"
        "tool_use" -> do
          cid <- o .: "id"
          name <- o .: "name"
          args <- o .: "input"
          pure $ ToolCallBlock (mkToolCall cid name args)
        _ -> fail $ "Unknown content block type: " <> T.unpack typ

parseClaudeUsage :: Value -> Maybe Usage
parseClaudeUsage = parseMaybe $ withObject "ClaudeResponse" $ \o -> do
  u <- o .: "usage"
  withObject "usage" (\uo -> Usage <$> uo .: "input_tokens" <*> uo .: "output_tokens" <*> pure 0) u

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
      case content of
        (block : _) -> withObject "content_block" (.: "text") block
        _ -> fail "No content"