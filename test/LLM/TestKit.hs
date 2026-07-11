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

import Data.Aeson (FromJSON, ToJSON, Value, decodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import Heptapod (generate)
import LLM.Agent.Events (noEventObserver)
import LLM.Agent.Generate (generateText, streamText)
import LLM.Agent.ToolUtils (toTool)
import LLM.Agent.Types (Agent (..), RuntimeArgs (..), ToolMap)
import LLM.Core.LLMProvider (LLMProvider (..))
import LLM.Core.Types (LLMGateway, ThinkingMode (..), Turn (..))
import LLM.Core.Usage (PricingInfo (..), addUsage, emptyUsage)
import LLM.Core.Utils (parseChatResponse)
import LLM.Generate.GenerateUtils (llmHooks)
import LLM.Generate.Logger (Hooks (..), noHooks)
import LLM.Generate.ModelConfig (ModelConfig (..), ModelWithFallbacks (..))
import LLM.Generate.Types (GenerateErrorResult (..), GenerateTextResult (..))
import LLM.WeatherTool (weatherToolTyped)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

data MockRequestResponse = MockRequestResponse
  { prompt :: Maybe Text,
    request :: Value,
    response :: Value
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

type MockConversation = [MockRequestResponse]

type MockConversationMap = M.Map Value Value

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
      let pairs = map (\rr -> (rr.request, rr.response)) rrs
          rrMap = M.fromList pairs
          prompts = rrs >>= \rsp -> case rsp.prompt of Nothing -> []; Just p -> [p]
       in pure (rrMap, prompts)

writeRecordedConversation :: FilePath -> MockConversation -> IO ()
writeRecordedConversation filePath conversation = do
  createDirectoryIfMissing True (takeDirectory filePath)
  BSL.writeFile filePath (encodePretty conversation)

mockProvider :: MockConversationMap -> LLMProvider -> LLMProvider
mockProvider mp adapter =
  adapter
    { sendRequest = \val -> do
        case M.lookup val mp of
          Nothing ->
            error (show val <> "\n" <> show (fst $ head $ M.toList mp))
          Just r -> pure (200, r),
      sendStreamRequest = \body _callback -> do
        case M.lookup body mp of
          Nothing -> error ("value not found for:" <> show body)
          Just r -> case parseMaybe parseChatResponse r of
            Nothing -> error "can't parse recorded ChatResponse json"
            Just chatResponse -> pure $ Right chatResponse
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
  IORef [MockRequestResponse] ->
  IO (Agent, ModelWithFallbacks, ToolMap Text, RuntimeArgs)
mkRecordedConversationRuntime gateway modelName thinking promptRef pendingRef entriesRef = do
  genId <- generate
  let hooks = recordingHooks promptRef pendingRef entriesRef
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
            mcPricing = PricingInfo {pricePerMillionInput = 0.0, pricePerMillionOutput = 0.0},
            mcMaxTokens = 1024,
            mcTemperature = Nothing,
            mcThinking = thinking,
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

recordConversation :: Bool -> LLMGateway -> Text -> Maybe ThinkingMode -> FilePath -> IO MockConversation
recordConversation stream gateway modelName thinking outPath = do
  promptRef <- newIORef Nothing
  pendingRef <- newIORef Nothing
  entriesRef <- newIORef []
  env <- mkRecordedConversationRuntime gateway modelName thinking promptRef pendingRef entriesRef
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
        Left (GenerateErrorResult {gerError = err}) -> do
          print err
          pure conv
        Right (GenerateTextResult {gtrUsage = usage, gtrNewMessages = newMessages}) -> do
          let conv'' = conv' ++ newMessages
          aux (addUsage totalUsage usage) conv'' rest

recordingHooks ::
  IORef (Maybe Text) ->
  IORef (Maybe Value) ->
  IORef [MockRequestResponse] ->
  Hooks
recordingHooks promptRef pendingRef entriesRef =
  noHooks
    { onRequest = \_ req -> writeIORef pendingRef (Just req),
      onResponse = \_ resp -> do
        mReq <- readIORef pendingRef
        mPrompt <- readIORef promptRef
        case mReq of
          Nothing -> pure ()
          Just req -> do
            let entry = MockRequestResponse mPrompt req resp
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
