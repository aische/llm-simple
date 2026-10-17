module LLM.TestKit
  ( MockRequestResponse (..),
    MockConversation,
    MockConversationMap,
    recordedConversationPrompts,
    recordedConversationSystemPrompt,
    readMockRequestResponse,
    loadRecordedConversation,
    mockProvider,
    streamChatLoop,
    streamChatLoopMain,
    recordConversation,
    writeRecordedConversation,
    mkRecordedConversationRuntime,
    recordedDeepSeekThinking,
  )
where

import Data.Aeson (FromJSON, ToJSON, Value (Array, Object, String), decodeFileStrict, encode)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import Heptapod (generate)
import LLM.Agent.Events (noEventObserver)
import LLM.Agent.Generate (generateText, streamText)
import LLM.Agent.ToolUtils (toTool)
import LLM.Agent.Types (Agent (..), RuntimeArgs (..), ToolMap)
import LLM.Core.LLMProvider (LLMProvider (..), toGateway)
import LLM.Core.Types (LLMGateway, ThinkingMode (..), Turn (..), pattern UserTurn)
import LLM.Core.Usage (addUsage, defaultPricingInfo, emptyUsage)
import LLM.Core.Utils (streamResponseJson)
import LLM.Generate.GenerateUtils (llmHooks)
import LLM.Generate.Logger (Hooks (..), noHooks)
import LLM.Generate.ModelConfig
  ( ModelCapabilities (..),
    ModelConfig (..),
    ModelWithFallbacks (..),
    defaultModelCapabilities,
  )
import LLM.Generate.Types (GenerateErrorResult (..), GenerateTextResult (..))
import LLM.WeatherTool (weatherToolTyped)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

data MockRequestResponse = MockRequestResponse
  { prompt :: Maybe Text,
    request :: Value,
    -- | Raw provider JSON for non-streaming calls; for streaming calls the
    -- library's own encoding of the parsed response ('streamResponseJson').
    response :: Value,
    -- | Raw streaming body as received from the provider (streaming calls only).
    sse :: Maybe Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

type MockConversation = [MockRequestResponse]

type MockConversationMap = M.Map Value MockRequestResponse

recordedConversationPrompts :: [Text]
recordedConversationPrompts =
  [ "how's the weather in london?",
    "and in paris?"
  ]

recordedConversationSystemPrompt :: Text
recordedConversationSystemPrompt =
  "You are a helpful assistant who answers questions and executes tools for the user. Always use tools when asked to, but use only the tools that are available."

recordedDeepSeekThinking :: ThinkingMode
recordedDeepSeekThinking = ThinkingMode {tmEnabled = True, tmEffort = Nothing}

readMockRequestResponse :: FilePath -> IO (Maybe MockConversation)
readMockRequestResponse = decodeFileStrict

loadRecordedConversation :: FilePath -> IO (MockConversationMap, [Text])
loadRecordedConversation filePath = do
  s <- readMockRequestResponse filePath
  case s of
    Nothing -> error "can't read conversation"
    Just rrs ->
      let pairs = map (\rr -> (rr.request, rr)) rrs
          rrMap = M.fromList pairs
          prompts = rrs >>= \rsp -> case rsp.prompt of Nothing -> []; Just p -> [p]
       in pure (rrMap, prompts)

writeRecordedConversation :: FilePath -> MockConversation -> IO ()
writeRecordedConversation filePath conversation = do
  createDirectoryIfMissing True (takeDirectory filePath)
  BSL.writeFile filePath (encodePretty conversation)

-- | Replay a recorded conversation. Requests must match a recording exactly.
-- Streaming bodies are fed through the provider's real 'parseStream', and the
-- parsed result must match the response recorded alongside them (ignoring
-- tool-call ids; see 'withoutToolCallIds').
mockProvider :: MockConversationMap -> LLMProvider -> LLMProvider
mockProvider mp adapter =
  adapter
    { sendRequest = \val -> do
        case M.lookup val mp of
          Nothing ->
            error (show val <> "\n" <> show (fst $ head $ M.toList mp))
          Just rr -> pure (200, BSL.toStrict (encode rr.response)),
      openStream = \body k -> do
        case M.lookup body mp of
          Nothing -> error ("value not found for:" <> show body)
          Just rr -> case rr.sse of
            Nothing -> error "recorded streaming call has no raw SSE body; re-record the fixture"
            Just raw -> do
              result <- k =<< bytesReader (TE.encodeUtf8 raw)
              case result of
                Right resp
                  | withoutToolCallIds (streamResponseJson resp) /= withoutToolCallIds rr.response ->
                      error $
                        "replayed stream parsed differently than recorded:\n"
                          <> show (streamResponseJson resp)
                          <> "\nexpected:\n"
                          <> show rr.response
                _ -> pure result
    }

-- | Gemini has no wire-level tool-call ids; the library mints them from a
-- process-global counter, so they differ between recording and replay. Where
-- ids do come from the wire, the next request's tool results still pin them.
withoutToolCallIds :: Value -> Value
withoutToolCallIds = \case
  Object o -> Object (fmap stripParts (KM.filterWithKey (\k _ -> k == "content") o) <> o)
  v -> v
  where
    stripParts (Array parts) = Array (fmap stripId parts)
    stripParts v = v
    stripId (Object p)
      | KM.lookup "type" p == Just (String "tool_call") = Object (KM.delete "id" p)
    stripId v = v

-- | Yield the whole payload once, then EOF.
bytesReader :: BS.ByteString -> IO (IO BS.ByteString)
bytesReader bs = do
  ref <- newIORef (Just bs)
  pure $ do
    m <- readIORef ref
    writeIORef ref Nothing
    pure (fromMaybe BS.empty m)

-- | Wrap a provider so every streaming body it reads is appended to @ref@.
teeStream :: IORef [BS.ByteString] -> LLMProvider -> LLMProvider
teeStream ref p =
  p
    { openStream = \body k ->
        p.openStream body $ \readChunk ->
          k $ do
            chunk <- readChunk
            modifyIORef' ref (chunk :)
            pure chunk
    }

streamChatLoopMain :: Bool -> (Agent, ModelWithFallbacks, ToolMap Text, RuntimeArgs) -> [Text] -> IO ()
streamChatLoopMain stream env prompts = do
  putStrLn "\n=== recorded conversation replay ==="
  _ <- streamChatLoop stream env prompts
  pure ()

streamChatLoop :: Bool -> (Agent, ModelWithFallbacks, ToolMap Text, RuntimeArgs) -> [Text] -> IO [Turn]
streamChatLoop stream (agent, models, toolMap, rt) = aux emptyUsage []
  where
    aux _totalUsage conv [] = pure conv
    aux totalUsage conv (prompt : rest) = do
      let conv' = conv ++ [UserTurn prompt]
      result <-
        if stream
          then streamText (const (pure ())) agent models toolMap rt conv'
          else generateText agent models toolMap rt conv'
      case result of
        Left (GenerateErrorResult {gerError = err}) -> do
          print err
          pure conv
        Right (GenerateTextResult {gtrUsage = usage, gtrNewMessages = newMessages}) -> do
          let conv'' = conv' ++ newMessages
          aux (addUsage totalUsage usage) conv'' rest

mkRecordedConversationRuntime ::
  LLMGateway ->
  Text ->
  Maybe ThinkingMode ->
  IORef (Maybe Text) ->
  IORef (Maybe Value) ->
  IORef [BS.ByteString] ->
  IORef [MockRequestResponse] ->
  IO (Agent, ModelWithFallbacks, ToolMap Text, RuntimeArgs)
mkRecordedConversationRuntime gateway modelName thinking promptRef pendingRef sseRef entriesRef = do
  genId <- generate
  let hooks = recordingHooks promptRef pendingRef sseRef entriesRef
      agent =
        Agent
          { agName = "test",
            agSystemPrompt = Just recordedConversationSystemPrompt,
            agTools = ["get_weather"],
            agMaxToolRounds = 3,
            agContextWindow = Nothing
          }
      modelConf =
        ModelConfig
          { mcGateway = gateway,
            mcModel = modelName,
            mcPricing = defaultPricingInfo 0.0 0.0,
            mcMaxTokens = 1024,
            mcTemperature = Nothing,
            mcThinking = thinking,
            mcCapabilities =
              defaultModelCapabilities
                { capThinking = maybe False (.tmEnabled) thinking
                },
            mcRequestTimeout = Nothing,
            mcThrottleDelay = Nothing,
            mcRetryCount = 3,
            mcJitterBackoff = 1_000
          }
      models = ModelWithFallbacks modelConf []
      toolMap = M.fromList [("get_weather", toTool weatherToolTyped)]
      rt =
        RuntimeArgs
          { rtGenerationId = genId,
            rtAbortSignal = Nothing,
            rtLLMHooks = llmHooks hooks,
            rtHooks = hooks,
            rtOnEvent = noEventObserver,
            rtReadonly = False
          }
  pure (agent, models, toolMap, rt)

recordConversation :: Bool -> LLMProvider -> Text -> Maybe ThinkingMode -> FilePath -> IO MockConversation
recordConversation stream provider modelName thinking outPath = do
  promptRef <- newIORef Nothing
  pendingRef <- newIORef Nothing
  sseRef <- newIORef []
  entriesRef <- newIORef []
  let gateway = toGateway (teeStream sseRef provider)
  env <- mkRecordedConversationRuntime gateway modelName thinking promptRef pendingRef sseRef entriesRef
  turns <- recordStreamChatLoop stream promptRef env recordedConversationPrompts
  entries <- readIORef entriesRef
  let conversation = reverse entries
  writeRecordedConversation outPath conversation
  putStrLn $
    unlines
      [ "Wrote " <> outPath,
        "  entries: " <> show (length conversation),
        "  turns: " <> show (length turns)
      ]
  pure conversation

recordStreamChatLoop ::
  Bool ->
  IORef (Maybe Text) ->
  (Agent, ModelWithFallbacks, ToolMap Text, RuntimeArgs) ->
  [Text] ->
  IO [Turn]
recordStreamChatLoop stream promptRef (agent, models, toolMap, rt) = aux emptyUsage []
  where
    aux _totalUsage conv [] = pure conv
    aux totalUsage conv (prompt : rest) = do
      writeIORef promptRef (Just prompt)
      TIO.putStrLn $ "> " <> prompt
      let conv' = conv ++ [UserTurn prompt]
      result <-
        if stream
          then streamText (const (pure ())) agent models toolMap rt conv'
          else generateText agent models toolMap rt conv'
      case result of
        -- Fail before anything is written, so a broken run never replaces a good fixture.
        Left (GenerateErrorResult {gerError = err}) ->
          fail ("recording failed, fixture not written: " <> show err)
        Right (GenerateTextResult {gtrUsage = usage, gtrNewMessages = newMessages}) -> do
          let conv'' = conv' ++ newMessages
          aux (addUsage totalUsage usage) conv'' rest

recordingHooks ::
  IORef (Maybe Text) ->
  IORef (Maybe Value) ->
  IORef [BS.ByteString] ->
  IORef [MockRequestResponse] ->
  Hooks
recordingHooks promptRef pendingRef sseRef entriesRef =
  noHooks
    { onRequest = \_ req -> do
        writeIORef pendingRef (Just req)
        writeIORef sseRef [],
      onResponse = \_ resp -> do
        mReq <- readIORef pendingRef
        mPrompt <- readIORef promptRef
        chunks <- readIORef sseRef
        case mReq of
          Nothing -> pure ()
          Just req -> do
            let raw = BS.concat (reverse chunks)
                mSse = if BS.null raw then Nothing else Just (TE.decodeUtf8 raw)
                entry = MockRequestResponse mPrompt req resp mSse
            entriesRef `modifyIORef'` (entry :)
            writeIORef pendingRef Nothing
            whenJustPrompt mPrompt $ writeIORef promptRef Nothing
    }
  where
    whenJustPrompt :: Maybe Text -> IO () -> IO ()
    whenJustPrompt mp act = if isJust mp then act else pure ()

modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' ref f = do
  v <- readIORef ref
  writeIORef ref (f v)
