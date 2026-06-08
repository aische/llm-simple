module LLM.Providers.Gemini
  ( geminiGateway,
    geminiProvider,
    parseGeminiResponse,
    parseGeminiUsage,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson
  ( KeyValue ((.=)),
    Value (Object, String),
    decodeStrict',
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Pair, Parser, parseMaybe)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Unique (hashUnique, newUnique)
import LLM.Core.LLMProvider (LLMProvider (..), toGateway)
import LLM.Core.ProviderUtils (handleStreamResponse, lenientConfig, stripBoundsAndComments, stripJsonFences)
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
    ChatResponse (ChatResponse),
    ContentBlock (..),
    LLMError (EmptyResponse),
    LLMGateway,
    LLMObjectResult,
    LLMTextResult,
    StreamEvent (..),
    ToolCall (..),
    ToolDef (toolDescription, toolName, toolParameters),
    ToolResult (trContent, trName),
    Turn (..),
    mkToolCall,
  )
import LLM.Core.Usage (Usage (..))
import Network.HTTP.Client qualified as HC
import Network.HTTP.Req
  ( POST (POST),
    ReqBodyJson (ReqBodyJson),
    header,
    https,
    jsonResponse,
    req,
    reqBr,
    responseBody,
    responseStatusCode,
    runReq,
    (/:),
    (=:),
  )

-- | Create a LLMGateway for the Gemini provider. Takes the API key as a parameter.
geminiGateway :: Text -> LLMGateway
geminiGateway apiKey = toGateway (geminiProvider apiKey)

-- | Create a LLMProvider for the Gemini provider. Takes the API key as a parameter.
geminiProvider :: Text -> LLMProvider
geminiProvider apiKey =
  LLMProvider
    { providerName = "gemini",
      buildBody = const geminiBuildBody,
      sendRequest = sendRequest,
      sendStreamRequest = \body callback ->
        runReq lenientConfig $ do
          let model = extractModel body
              url =
                https "generativelanguage.googleapis.com"
                  /: "v1beta"
                  /: "models"
                  /: (model <> ":streamGenerateContent")
          reqBr POST url (ReqBodyJson (stripBoundsAndComments $ stripModel body)) (header "x-goog-api-key" (encodeUtf8 apiKey) <> "alt" =: ("sse" :: Text)) $ \resp ->
            handleStreamResponse resp (`parseGeminiStream` callback),
      parseResponse = parseGeminiResponse,
      buildObjectBody = \r schema ->
        object $
          [ "_model" .= r.reqModel,
            "contents" .= concatMap (encodeTurn r.reqModel) r.reqConversation,
            "generationConfig"
              .= object
                ( [ "maxOutputTokens" .= r.reqMaxTokens,
                    "responseMimeType" .= ("application/json" :: Text),
                    "responseSchema" .= schema
                  ]
                    ++ ["temperature" .= t | Just t <- [r.reqTemperature]]
                )
          ]
            ++ [ "system_instruction" .= object ["parts" .= [object ["text" .= sys]]]
                 | Just sys <- [r.reqSystem]
               ]
            ++ [ "tools" .= [object ["function_declarations" .= map encodeToolDef r.reqTools]]
                 | not (null r.reqTools)
               ],
      sendObjectRequest = sendRequest,
      parseObjectResponse = parseGeminiObjectResponse
    }
  where
    sendRequest body =
      runReq lenientConfig $ do
        -- For non-streaming we need the model name from the body to construct the URL.
        -- We extract it from the request body JSON since the LLMProvider only passes Value.
        let model = extractModel body
            url =
              https "generativelanguage.googleapis.com"
                /: "v1beta"
                /: "models"
                /: (model <> ":generateContent")
        resp <- req POST url (ReqBodyJson (stripBoundsAndComments $ stripModel body)) jsonResponse (header "x-goog-api-key" (encodeUtf8 apiKey))
        pure (responseStatusCode resp, responseBody resp)

-- | Extract model name stashed in the request body by geminiBuildBody.
extractModel :: Value -> Text
extractModel v = fromMaybe "gemini-2.0-flash" (parseMaybe (withObject "body" (.: "_model")) v)

-- | Remove the internal '_model' field before sending to the API.
stripModel :: Value -> Value
stripModel (Object o) = Object (KM.delete "_model" o)
stripModel v = v

parseGeminiStream :: HC.BodyReader -> (StreamEvent -> IO ()) -> IO LLMTextResult
parseGeminiStream reader callback = do
  blocksRef <- newIORef ([] :: [ContentBlock])
  usageRef <- newIORef Nothing
  readSSEEvents (HC.brRead reader) $ \sse -> do
    case decodeStrict' (encodeUtf8 sse.sseData) of
      Nothing -> pure ()
      Just v -> do
        -- modelVersion is needed so we can later guard against replaying
        -- thought signatures into a different model than the one that
        -- produced them.
        let modelVer = parseMaybe parseModelVersion v
        case parseMaybe (parseChunkParts modelVer) v of
          Just parts -> do
            newBlocks <- mapM (assignToolId callback) parts
            modifyIORef' blocksRef (++ newBlocks)
          Nothing -> pure ()
        case parseMaybe parseUsageMetadata v of
          Just u -> writeIORef usageRef (Just u)
          Nothing -> pure ()
  blocks <- readIORef blocksRef
  usage <- readIORef usageRef
  let text = T.concat [t | TextBlock t <- blocks]
  if null blocks
    then pure $ Left EmptyResponse
    else pure $ Right (ChatResponse text blocks usage Nothing)
  where
    assignToolId :: (StreamEvent -> IO ()) -> ContentBlock -> IO ContentBlock
    assignToolId cb (TextBlock t) = do
      cb (StreamDelta t)
      pure (TextBlock t)
    assignToolId cb (ToolCallBlock tc) = do
      tc' <- normalizeToolCallId tc
      cb (StreamToolCall tc')
      pure (ToolCallBlock tc')

    parseChunkParts :: Maybe Text -> Value -> Parser [ContentBlock]
    parseChunkParts modelVer = withObject "GeminiChunk" $ \o -> do
      (cand : _) <- o .: "candidates" :: Parser [Value]
      withObject
        "candidate"
        ( \co -> do
            cont <- co .: "content"
            withObject "content" (\cco -> cco .: "parts" >>= mapM (parsePartBlock modelVer)) cont
        )
        cand

    parsePartBlock :: Maybe Text -> Value -> Parser ContentBlock
    parsePartBlock modelVer = withObject "part" $ \o -> do
      mSig <- o .:? "thoughtSignature" :: Parser (Maybe Text)
      let tryText = TextBlock <$> (o .: "text")
          tryFunctionCall = do
            fc <- o .: "functionCall"
            withObject
              "functionCall"
              ( \fco -> do
                  name <- fco .: "name"
                  args <- fco .:? "args" .!= object []
                  pure $ ToolCallBlock (attachGeminiMeta modelVer mSig (mkToolCall name name args))
              )
              fc
      tryText <|> tryFunctionCall

    parseUsageMetadata :: Value -> Parser Usage
    parseUsageMetadata = withObject "GeminiChunk" $ \o -> do
      u <- o .: "usageMetadata"
      withObject
        "usageMetadata"
        (\uo -> Usage <$> uo .: "promptTokenCount" <*> uo .: "candidatesTokenCount" <*> pure 0)
        u

geminiBuildBody :: ChatRequest -> Value
geminiBuildBody r = object $ geminiBuildBodyPairs r

geminiBuildBodyPairs :: ChatRequest -> [Pair]
geminiBuildBodyPairs r =
  [ "_model" .= r.reqModel,
    "contents" .= concatMap (encodeTurn r.reqModel) r.reqConversation,
    "generationConfig" .= genConfig r
  ]
    ++ [ "system_instruction" .= object ["parts" .= [object ["text" .= sys]]]
         | Just sys <- [r.reqSystem]
       ]
    ++ [ "tools" .= [object ["function_declarations" .= map encodeToolDef r.reqTools]]
         | not (null r.reqTools)
       ]

-- | Encode a turn for Gemini. The current request model is threaded through
-- so that thought signatures captured from a previous response can be
-- replayed only when the receiving model matches the one that emitted them.
encodeTurn :: Text -> Turn -> [Value]
encodeTurn _ (UserTurn content) =
  [ object
      [ "role" .= ("user" :: Text),
        "parts" .= [object ["text" .= content]]
      ]
  ]
encodeTurn currentModel (AssistantTurn text _mReasoning calls) =
  [ object
      [ "role" .= ("model" :: Text),
        "parts" .= (textParts ++ callParts)
      ]
  ]
  where
    textParts = [object ["text" .= text] | not (T.null text)]
    callParts = map (encodeFunctionCall currentModel) calls
encodeTurn _ (ToolTurn results) =
  [ object
      [ "role" .= ("user" :: Text),
        "parts" .= map encodeFunctionResponse results
      ]
  ]

encodeToolDef :: ToolDef -> Value
encodeToolDef td =
  object
    [ "name" .= td.toolName,
      "description" .= td.toolDescription,
      "parameters" .= td.toolParameters
    ]

-- | Encode an assistant tool call back to a Gemini @functionCall@ part.
--
-- Gemini 2.5 thinking models attach an opaque 'thoughtSignature' to each
-- function-call part. That signature must be replayed verbatim on subsequent
-- requests against the same model, or the API rejects the request with a
-- @function call is missing a thought_signature@ error. We stored the
-- signature plus the emitting model name in 'tcProviderMeta' at parse time;
-- here we re-emit it only when 'currentModel' matches, because signatures
-- are bound to the model that produced them.
encodeFunctionCall :: Text -> ToolCall -> Value
encodeFunctionCall currentModel tc =
  object $
    [ "functionCall"
        .= object
          [ "name" .= tc.tcName,
            "args" .= tc.tcArguments
          ]
    ]
      ++ ["thoughtSignature" .= s | Just s <- [signatureForModel currentModel tc.tcProviderMeta]]

encodeFunctionResponse :: ToolResult -> Value
encodeFunctionResponse tr =
  object
    [ "functionResponse"
        .= object
          [ "name" .= tr.trName,
            "response" .= object ["result" .= tr.trContent]
          ]
    ]

-- | Generate a unique call ID for Gemini tool calls (which lack native IDs)
normalizeToolCallId :: ToolCall -> IO ToolCall
normalizeToolCallId tc = do
  u <- newUnique
  let callId = "call_" <> T.pack (show (hashUnique u))
  pure tc {tcId = callId}

normalizeBlock :: ContentBlock -> IO ContentBlock
normalizeBlock (ToolCallBlock tc) = ToolCallBlock <$> normalizeToolCallId tc
normalizeBlock b = pure b

genConfig :: ChatRequest -> Value
genConfig r =
  object $
    ("maxOutputTokens" .= r.reqMaxTokens)
      : ["temperature" .= t | Just t <- [r.reqTemperature]]

parseGeminiResponse :: Value -> IO LLMTextResult
parseGeminiResponse v = case parseMaybe (go modelVer) v of
  Nothing -> pure $ Left EmptyResponse
  Just blocks -> do
    blocks' <- mapM normalizeBlock blocks
    case blocks' of
      [] -> pure $ Left EmptyResponse
      _ ->
        let text = T.concat [t | TextBlock t <- blocks']
         in pure $ Right (ChatResponse text blocks' (parseGeminiUsage v) Nothing)
  where
    modelVer = parseMaybe parseModelVersion v

    go :: Maybe Text -> Value -> Parser [ContentBlock]
    go mv = withObject "GeminiResponse" $ \o -> do
      (cand : _) <- o .: "candidates" :: Parser [Value]
      withObject
        "candidate"
        ( \co -> do
            cont <- co .: "content"
            withObject
              "content"
              ( \cco -> do
                  parts <- cco .: "parts" :: Parser [Value]
                  mapM (parsePart mv) parts
              )
              cont
        )
        cand

    parsePart :: Maybe Text -> Value -> Parser ContentBlock
    parsePart mv = withObject "part" $ \o -> do
      mSig <- o .:? "thoughtSignature" :: Parser (Maybe Text)
      let tryText = TextBlock <$> (o .: "text")
          tryFunctionCall = do
            fc <- o .: "functionCall"
            withObject
              "functionCall"
              ( \fco -> do
                  name <- fco .: "name"
                  args <- fco .:? "args" .!= object []
                  -- Gemini doesn't provide a call id; use the function name.
                  -- normalizeBlock replaces it with a unique id later.
                  pure $ ToolCallBlock (attachGeminiMeta mv mSig (mkToolCall name name args))
              )
              fc
      tryText <|> tryFunctionCall

-- | Extract @modelVersion@ from a Gemini response or chunk. This is the
-- canonical model name (e.g. @gemini-2.5-pro-001@) used as the binding key
-- for thought signatures.
parseModelVersion :: Value -> Parser Text
parseModelVersion = withObject "GeminiResponse" (.: "modelVersion")

-- | Build the provider-metadata bag stored on a 'ToolCall' for Gemini.
-- Returns the original tool call unchanged when there's no signature to
-- preserve, so we don't pollute logs with empty metadata.
attachGeminiMeta :: Maybe Text -> Maybe Text -> ToolCall -> ToolCall
attachGeminiMeta _ Nothing tc = tc
attachGeminiMeta mModel (Just sig) tc =
  tc {tcProviderMeta = Just (object (("thoughtSignature" .= sig) : modelField))}
  where
    modelField = ["model" .= m | Just m <- [mModel]]

-- | Pull a thought signature out of 'tcProviderMeta' iff it was emitted by
-- the model we're currently calling. Cross-model replay is unsafe — Gemini
-- treats the signature as an opaque per-model token and will reject it.
signatureForModel :: Text -> Maybe Value -> Maybe Text
signatureForModel currentModel (Just (Object o)) = do
  String sig <- KM.lookup "thoughtSignature" o
  case KM.lookup "model" o of
    Just (String m) | not (modelsMatch currentModel m) -> Nothing
    _ -> Just sig
signatureForModel _ _ = Nothing

-- | A signature emitted by @gemini-2.5-pro-001@ is safe to replay against
-- @gemini-2.5-pro@ (and vice-versa): the response carries the resolved
-- version string while requests typically use an alias. We accept either
-- direction being a prefix of the other.
modelsMatch :: Text -> Text -> Bool
modelsMatch a b = a == b || T.isPrefixOf a b || T.isPrefixOf b a

parseGeminiUsage :: Value -> Maybe Usage
parseGeminiUsage = parseMaybe $ withObject "GeminiResponse" $ \o -> do
  u <- o .: "usageMetadata"
  withObject
    "usageMetadata"
    (\uo -> Usage <$> uo .: "promptTokenCount" <*> uo .: "candidatesTokenCount" <*> pure 0)
    u

parseGeminiObjectResponse :: Value -> IO LLMObjectResult
parseGeminiObjectResponse v = case parseMaybe go v of
  Nothing -> pure $ Left EmptyResponse
  Just text -> case decodeStrict' (encodeUtf8 (stripJsonFences text)) of
    Nothing -> pure $ Left EmptyResponse
    Just obj -> pure $ Right (obj, parseGeminiUsage v)
  where
    go :: Value -> Parser Text
    go = withObject "GeminiObjectResponse" $ \o -> do
      (cand : _) <- o .: "candidates" :: Parser [Value]
      withObject "candidate" (\co -> co .: "content" >>= withObject "content" (\cco -> cco .: "parts" >>= \case (p : _) -> withObject "part" (.: "text") p; _ -> fail "No parts")) cand