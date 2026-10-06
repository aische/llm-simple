module LLM.Providers.Gemini
  ( geminiGateway,
    geminiGatewayWith,
    geminiProvider,
    geminiProviderWith,
    parseGeminiResponse,
    parseGeminiUsage,
    encodeTurn,
    signatureForModel,
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
import Data.Maybe (fromMaybe, mapMaybe)
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
        reqThinking,
        reqTools
      ),
    ContentPart (..),
    LLMError (EmptyResponse),
    LLMGateway,
    LLMObjectResult,
    LLMTextResult,
    PartBody (..),
    ProviderOpaque (..),
    StreamEvent (..),
    ThinkingContent (..),
    ThinkingMode (..),
    ToolCall (..),
    ToolDef (toolDescription, toolName, toolParameters),
    ToolResult (trContent, trName),
    Turn (..),
    ImageSource (..),
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
    (=:),
  )

geminiProviderName :: Text
geminiProviderName = "gemini"

-- | Create a LLMGateway for the Gemini provider at generativelanguage.googleapis.com.
geminiGateway :: Text -> LLMGateway
geminiGateway apiKey = toGateway (geminiProvider apiKey)

-- | Create a Gemini-compatible client with a custom base URL (origin).
-- The library appends @/v1beta/models/{model}:generateContent@ (and the stream variant).
geminiGatewayWith :: Url scheme -> Option scheme -> Text -> LLMGateway
geminiGatewayWith baseUrl baseOpts apiKey = toGateway (geminiProviderWith baseUrl baseOpts apiKey)

-- | Create a LLMProvider for the Gemini provider at generativelanguage.googleapis.com.
geminiProvider :: Text -> LLMProvider
geminiProvider = geminiProviderWith (https "generativelanguage.googleapis.com") mempty

-- | Gemini-compatible provider with a custom base URL (origin).
-- The library appends @/v1beta/models/{model}:generateContent@ (and the stream variant).
geminiProviderWith :: Url scheme -> Option scheme -> Text -> LLMProvider
geminiProviderWith baseUrl baseOpts apiKey =
  LLMProvider
    { providerName = geminiProviderName,
      buildBody = const geminiBuildBody,
      sendRequest = sendRequest,
      sendStreamRequest = \body callback ->
        runReq lenientConfig $ do
          let model = extractModel body
              url =
                baseUrl
                  /: "v1beta"
                  /: "models"
                  /: (model <> ":streamGenerateContent")
              opts = baseOpts <> geminiAuthOpts apiKey <> "alt" =: ("sse" :: Text)
          reqBr POST url (ReqBodyJson (stripBoundsAndComments $ stripModel body)) opts $ \resp ->
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
                    ++ thinkingConfigPairs r
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
        let model = extractModel body
            url =
              baseUrl
                /: "v1beta"
                /: "models"
                /: (model <> ":generateContent")
            opts = baseOpts <> geminiAuthOpts apiKey
        resp <- req POST url (ReqBodyJson (stripBoundsAndComments $ stripModel body)) jsonResponse opts
        pure (responseStatusCode resp, responseBody resp)

geminiAuthOpts :: Text -> Option scheme
geminiAuthOpts apiKey = header "x-goog-api-key" (encodeUtf8 apiKey)

extractModel :: Value -> Text
extractModel v = fromMaybe "gemini-2.0-flash" (parseMaybe (withObject "body" (.: "_model")) v)

stripModel :: Value -> Value
stripModel (Object o) = Object (KM.delete "_model" o)
stripModel v = v

parseGeminiStream :: HC.BodyReader -> (StreamEvent -> IO ()) -> IO LLMTextResult
parseGeminiStream reader callback = do
  partsRef <- newIORef ([] :: [ContentPart])
  usageRef <- newIORef Nothing
  readSSEEvents (HC.brRead reader) $ \sse -> do
    case decodeStrict' (encodeUtf8 sse.sseData) of
      Nothing -> pure ()
      Just v -> do
        let modelVer = parseMaybe parseModelVersion v
        case parseMaybe (parseChunkParts modelVer) v of
          Just parts -> do
            newParts <- mapM (assignToolId callback) parts
            modifyIORef' partsRef (++ newParts)
          Nothing -> pure ()
        case parseMaybe parseUsageMetadata v of
          Just u -> writeIORef usageRef (Just u)
          Nothing -> pure ()
  parts <- readIORef partsRef
  usage <- readIORef usageRef
  if null parts
    then pure $ Left EmptyResponse
    else pure $ Right (mkChatResponse parts usage)
  where
    assignToolId :: (StreamEvent -> IO ()) -> ContentPart -> IO ContentPart
    assignToolId cb (ContentPart (TextPart t)) = do
      cb (StreamDelta t)
      pure (textPart t)
    assignToolId cb (ContentPart (ThinkingPart tc)) = do
      case tc.thinkingText of
        Just t | not (T.null t) -> cb (StreamReasoningDelta t)
        _ -> pure ()
      pure (thinkingPart tc)
    assignToolId cb (ContentPart (ToolCallPart tc)) = do
      tc' <- normalizeToolCallId tc
      cb (StreamToolCall tc')
      pure (toolCallPart tc')
    assignToolId _ (ContentPart (ImagePart src)) =
      pure (ContentPart (ImagePart src))

    parseChunkParts :: Maybe Text -> Value -> Parser [ContentPart]
    parseChunkParts modelVer = withObject "GeminiChunk" $ \o -> do
      (cand : _) <- o .: "candidates" :: Parser [Value]
      withObject
        "candidate"
        ( \co -> do
            cont <- co .: "content"
            withObject "content" (\cco -> cco .: "parts" >>= mapM (parsePart modelVer)) cont
        )
        cand

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

-- | Encode a turn for Gemini. Foreign opaque thinking / tool metadata is omitted
-- at encode time so fallbacks never replay Claude (or other) state.
encodeTurn :: Text -> Turn -> [Value]
encodeTurn _ (UserMessage parts) =
  [ object
      [ "role" .= ("user" :: Text),
        "parts" .= mapMaybe encodeUserPart parts
      ]
  ]
  where
    encodeUserPart (ContentPart (TextPart t)) = Just $ object ["text" .= t]
    encodeUserPart (ContentPart (ImagePart src)) = Just $ encodeImagePart src
    encodeUserPart _ = Nothing
encodeTurn currentModel (AssistantMessage parts) =
  [ object
      [ "role" .= ("model" :: Text),
        "parts" .= mapMaybe (encodeAssistantPart currentModel) cleaned
      ]
  ]
  where
    cleaned = map (stripForeignOpaque geminiProviderName) parts
encodeTurn _ (ToolTurn results) =
  [ object
      [ "role" .= ("user" :: Text),
        "parts" .= map encodeFunctionResponse results
      ]
  ]

encodeImagePart :: ImageSource -> Value
encodeImagePart (ImageUrl url) =
  object
    [ "fileData"
        .= object
          [ "mimeType" .= guessImageMimeFromUrl url,
            "fileUri" .= url
          ]
    ]
encodeImagePart (ImageBase64 mediaType data_) =
  object
    [ "inlineData"
        .= object
          [ "mimeType" .= mediaType,
            "data" .= data_
          ]
    ]

-- | Best-effort MIME guess for Gemini fileData URL parts.
guessImageMimeFromUrl :: Text -> Text
guessImageMimeFromUrl url =
  let path = T.toLower $ T.takeWhile (/= '?') url
   in case () of
        _
          | ".png" `T.isSuffixOf` path -> "image/png"
          | ".gif" `T.isSuffixOf` path -> "image/gif"
          | ".webp" `T.isSuffixOf` path -> "image/webp"
          | ".heic" `T.isSuffixOf` path -> "image/heic"
          | ".heif" `T.isSuffixOf` path -> "image/heif"
          | otherwise -> "image/jpeg"

encodeAssistantPart :: Text -> ContentPart -> Maybe Value
encodeAssistantPart _ (ContentPart (TextPart t))
  | T.null t = Nothing
  | otherwise = Just $ object ["text" .= t]
encodeAssistantPart currentModel (ContentPart (ThinkingPart tc)) =
  case tc.thinkingOpaque of
    Just o
      | o.poProvider == geminiProviderName,
        modelOk currentModel o.poModel ->
          Just o.poPayload
    _ ->
      case tc.thinkingText of
        Just t
          | not (T.null t) ->
              Just $ object ["text" .= t, "thought" .= True]
        _ -> Nothing
encodeAssistantPart currentModel (ContentPart (ToolCallPart tc)) =
  Just $ encodeFunctionCall currentModel tc
encodeAssistantPart _ (ContentPart (ImagePart _)) = Nothing

modelOk :: Text -> Maybe Text -> Bool
modelOk _ Nothing = True
modelOk current (Just m) = modelsMatch current m

encodeToolDef :: ToolDef -> Value
encodeToolDef td =
  object
    [ "name" .= td.toolName,
      "description" .= td.toolDescription,
      "parameters" .= td.toolParameters
    ]

encodeFunctionCall :: Text -> ToolCall -> Value
encodeFunctionCall currentModel tc =
  object $
    ( "functionCall"
        .= object
          [ "name" .= tc.tcName,
            "args" .= tc.tcArguments
          ]
    )
      : ["thoughtSignature" .= s | Just s <- [signatureForModel currentModel tc.tcProviderMeta]]

encodeFunctionResponse :: ToolResult -> Value
encodeFunctionResponse tr =
  object
    [ "functionResponse"
        .= object
          [ "name" .= tr.trName,
            "response" .= object ["result" .= tr.trContent]
          ]
    ]

normalizeToolCallId :: ToolCall -> IO ToolCall
normalizeToolCallId tc = do
  u <- newUnique
  let callId = "call_" <> T.pack (show (hashUnique u))
  pure tc {tcId = callId}

normalizePart :: ContentPart -> IO ContentPart
normalizePart (ContentPart (ToolCallPart tc)) = toolCallPart <$> normalizeToolCallId tc
normalizePart p = pure p

genConfig :: ChatRequest -> Value
genConfig r =
  object $
    ("maxOutputTokens" .= r.reqMaxTokens)
      : ["temperature" .= t | Just t <- [r.reqTemperature]]
      ++ thinkingConfigPairs r

-- | Map 'ThinkingMode' to Gemini @thinkingConfig@.
--
-- Level strings (@low@/@medium@/@high@/@minimal@) become @thinkingLevel@
-- (Gemini 3). Numeric effort becomes @thinkingBudget@ (Gemini 2.5). Enabled
-- without effort requests dynamic budget (@-1@).
thinkingConfigPairs :: ChatRequest -> [Pair]
thinkingConfigPairs r =
  case r.reqThinking of
    Nothing -> []
    Just tm
      | not tm.tmEnabled ->
          ["thinkingConfig" .= object ["thinkingBudget" .= (0 :: Int)]]
      | Just e <- tm.tmEffort,
        e `elem` ["minimal", "low", "medium", "high"] ->
          [ "thinkingConfig"
              .= object
                [ "thinkingLevel" .= e,
                  "includeThoughts" .= True
                ]
          ]
      | Just e <- tm.tmEffort,
        Just n <- readMaybeInt e ->
          [ "thinkingConfig"
              .= object
                [ "thinkingBudget" .= n,
                  "includeThoughts" .= True
                ]
          ]
      | otherwise ->
          [ "thinkingConfig"
              .= object
                [ "thinkingBudget" .= (-1 :: Int),
                  "includeThoughts" .= True
                ]
          ]

readMaybeInt :: Text -> Maybe Int
readMaybeInt t =
  case reads (T.unpack t) of
    [(n, "")] -> Just n
    _ -> Nothing

parseGeminiResponse :: Value -> IO LLMTextResult
parseGeminiResponse v = case parseMaybe (go modelVer) v of
  Nothing -> pure $ Left EmptyResponse
  Just parts -> do
    parts' <- mapM normalizePart parts
    case parts' of
      [] -> pure $ Left EmptyResponse
      _ -> pure $ Right (mkChatResponse parts' (parseGeminiUsage v))
  where
    modelVer = parseMaybe parseModelVersion v

    go :: Maybe Text -> Value -> Parser [ContentPart]
    go mv = withObject "GeminiResponse" $ \o -> do
      (cand : _) <- o .: "candidates" :: Parser [Value]
      withObject
        "candidate"
        ( \co -> do
            cont <- co .: "content"
            withObject
              "content"
              ( \cco -> do
                  ps <- cco .: "parts" :: Parser [Value]
                  mapM (parsePart mv) ps
              )
              cont
        )
        cand

parsePart :: Maybe Text -> Value -> Parser ContentPart
parsePart mv = withObject "part" $ \o -> do
  mSig <- o .:? "thoughtSignature" :: Parser (Maybe Text)
  mThought <- o .:? "thought" :: Parser (Maybe Bool)
  let tryText = do
        t <- o .: "text"
        if mThought == Just True
          then do
            let opaque =
                  case mSig of
                    Nothing -> Nothing
                    Just sig ->
                      Just
                        ProviderOpaque
                          { poProvider = geminiProviderName,
                            poModel = mv,
                            poPayload =
                              object $
                                ["text" .= t, "thought" .= True]
                                  ++ ["thoughtSignature" .= sig]
                          }
            pure $ thinkingPart (ThinkingContent (Just t) opaque)
          else
            -- Plain text; preserve signature as opaque thinking sidecar only when
            -- required — Gemini 2.5 may put the signature on the first part.
            pure $ textPart t
      tryFunctionCall = do
        fc <- o .: "functionCall"
        withObject
          "functionCall"
          ( \fco -> do
              name <- fco .: "name"
              args <- fco .:? "args" .!= object []
              pure $ toolCallPart (attachGeminiMeta mv mSig (mkToolCall name name args))
          )
          fc
  tryFunctionCall <|> tryText

parseModelVersion :: Value -> Parser Text
parseModelVersion = withObject "GeminiResponse" (.: "modelVersion")

attachGeminiMeta :: Maybe Text -> Maybe Text -> ToolCall -> ToolCall
attachGeminiMeta _ Nothing tc = tc
attachGeminiMeta mModel (Just sig) tc =
  tc
    { tcProviderMeta =
        Just
          ProviderOpaque
            { poProvider = geminiProviderName,
              poModel = mModel,
              poPayload = object ["thoughtSignature" .= sig]
            }
    }

-- | Pull a thought signature out of 'tcProviderMeta' iff it was emitted by
-- the model we're currently calling.
signatureForModel :: Text -> Maybe ProviderOpaque -> Maybe Text
signatureForModel currentModel (Just o)
  | o.poProvider == geminiProviderName,
    modelOk currentModel o.poModel =
      case o.poPayload of
        Object m | Just (String sig) <- KM.lookup "thoughtSignature" m -> Just sig
        _ -> Nothing
signatureForModel _ _ = Nothing

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
