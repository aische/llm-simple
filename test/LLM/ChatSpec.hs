{-# OPTIONS_GHC -Wno-missing-fields #-}

module LLM.ChatSpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (finally)
import Data.Aeson (object, (.=))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Heptapod (generate)
import LLM.Agent.Events (noEventObserver)
import LLM.Agent.Generate (generateText, streamText, toolBudgetUsedUpPrompt)
import LLM.Agent.GenerateObject (generateObjectUntyped)
import LLM.Agent.ToolUtils (validateAgent)
import LLM.Agent.Types
  ( Agent (..),
    RuntimeArgs (..),
    Tool (..),
    ToolMap,
  )
import LLM.Core.Abort (AbortSignal, abort, newAbortSignal)
import LLM.Core.Types
  ( CacheHint (..),
    ChatRequest (..),
    ChatResponse (..),
    ContentPart (..),
    PartBody (..),
    textPart, toolCallPart, pattern UserTurn,
    Turn (..),
    cacheEphemeral,
    commonImageMediaTypes,
    imageBase64Part,
    imageUrlPart,
    supportedImageMediaTypes,
    LLMError (..),
    LLMGateway (..),
    LLMHooks (..),
    ToolDef (ToolDef, toolDescription, toolName, toolParameters, toolReadonly),
    ToolCall (..),
    ToolResult (..),
    mkToolCall,
  )
import LLM.Core.Utils (getToolCalls)
import LLM.Core.Usage (PricingInfo (..), defaultPricingInfo, mkUsage)
import LLM.Generate.Generate (generateTextWithFallbacks, streamTextWithFallbacks)
import LLM.Generate.GenerateUtils (llmHooks)
import LLM.Generate.Logger (Hooks (..), LogLevel (..), noHooks)
import LLM.Generate.ModelConfig
  ( ModelCapabilities (..),
    ModelConfig (..),
    ModelWithFallbacks (..),
    defaultModelCapabilities,
  )
import LLM.Generate.Types
  ( GenRequest (..),
    GenerateError (..),
    GenerateErrorResult (..),
    GenerateTextResult (..),
  )
import Test.Hspec
  ( Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
    shouldContain,
    shouldReturn,
    shouldSatisfy,
  )
import System.Timeout (timeout)

-- | A mock gateway that returns a fixed response
mockGateway :: ChatResponse -> LLMGateway
mockGateway resp =
  LLMGateway
    { gwName = "mock",
      gwImageMediaTypes = supportedImageMediaTypes,
      gwImageUrlSupport = True,
      gwGenerateText = \_ _ -> pure (Right resp),
      gwStreamText = \_ _ _ -> pure (Right resp),
      gwGenerateObject = \_ _ _ -> pure (Right (object [], Nothing))
    }

-- | A mock gateway that returns an error
mockErrorGateway :: LLMError -> LLMGateway
mockErrorGateway err =
  LLMGateway
    { gwName = "mock-error",
      gwImageMediaTypes = supportedImageMediaTypes,
      gwImageUrlSupport = True,
      gwGenerateText = \_ _ -> pure (Left err),
      gwStreamText = \_ _ _ -> pure (Left err),
      gwGenerateObject = \_ _ _ -> pure (Left err)
    }

-- | A mock gateway that calls a tool, then responds with text
mockToolGateway :: LLMGateway
mockToolGateway =
  LLMGateway
    { gwName = "mock-tool",
      gwImageMediaTypes = supportedImageMediaTypes,
      gwImageUrlSupport = True,
      gwGenerateText = \_ req ->
        if any isToolTurn req.reqConversation
          then pure $ Right (ChatResponse "The weather is sunny." [textPart "The weather is sunny."] (Just (mkUsage 80 15)) Nothing)
          else
            let tc = mkToolCall "call_1" "get_weather" (object ["location" .= ("London" :: Text)])
             in pure $ Right (ChatResponse "" [toolCallPart tc] (Just (mkUsage 50 10)) Nothing),
      gwStreamText = \_ _ _ -> pure $ Right (ChatResponse "" [] Nothing Nothing),
      gwGenerateObject = \_ _ _ -> pure $ Right (object [], Nothing)
    }
  where
    isToolTurn (ToolTurn _) = True
    isToolTurn _ = False

zeroPricing :: PricingInfo
zeroPricing = defaultPricingInfo 0 0

-- | Wrap a gateway in a ModelConfig with test defaults
mockModel :: LLMGateway -> ModelConfig
mockModel gw =
  ModelConfig
    { mcGateway = gw,
      mcModel = "test-model",
      mcPricing = zeroPricing,
      mcMaxTokens = 1024,
      mcTemperature = Nothing,
      mcThinking = Nothing,
      mcCapabilities = defaultModelCapabilities,
      mcRequestTimeout = Nothing,
      mcThrottleDelay = Nothing,
      mcRetryCount = 0,
      mcJitterBackoff = 0
    }

weatherTool :: Tool Text
weatherTool =
  Tool
    { toolDef =
        ToolDef
          { toolName = "get_weather",
            toolDescription = "Get weather",
            toolReadonly = True,
            toolParameters = object ["type" .= ("object" :: Text)]
          },
      toolExecute = \_ _ -> pure "Sunny, 22°C"
    }

defaultAgent :: Agent
defaultAgent =
  Agent
    { agName = "test",
      agSystemPrompt = Nothing,
      agTools = [],
      agMaxToolRounds = 10,
      agContextWindow = Nothing
    }

mkRuntime :: Maybe AbortSignal -> IO RuntimeArgs
mkRuntime mSig = do
  uuid <- generate
  pure
    RuntimeArgs
      { rtGenerationId = uuid,
        rtAbortSignal = mSig,
        rtLLMHooks =
          LLMHooks
            { onLLMRequest = \_ _ -> pure (),
              onLLMResponse = \_ _ -> pure (),
              onLLMResponseError = \_ _ -> pure ()
            },
        rtHooks = noHooks,
        rtOnEvent = noEventObserver,
        rtReadonly = False
      }

runGenerate ::
  Agent ->
  ModelWithFallbacks ->
  ToolMap Text ->
  Maybe AbortSignal ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
runGenerate agent models toolMap mSig turns = do
  rt <- mkRuntime mSig
  generateText agent models toolMap rt turns

-- | A tool named @name@ that ignores its arguments and runs @act@.
constTool :: Text -> IO Text -> Tool Text
constTool name act =
  Tool
    { toolDef =
        ToolDef
          { toolName = name,
            toolDescription = "test tool",
            toolReadonly = True,
            toolParameters = object ["type" .= ("object" :: Text)]
          },
      toolExecute = \_ _ -> act
    }

-- | A gateway that always asks for two calls (@c1@, @c2@) of tool @name@.
twoToolCallGateway :: Text -> LLMGateway
twoToolCallGateway name =
  LLMGateway
    { gwName = "mock-two",
      gwImageMediaTypes = supportedImageMediaTypes,
      gwImageUrlSupport = True,
      gwGenerateText = \_ _ ->
        let tc1 = mkToolCall "c1" name (object [])
            tc2 = mkToolCall "c2" name (object [])
         in pure $ Right (ChatResponse "" [toolCallPart tc1, toolCallPart tc2] Nothing Nothing),
      gwStreamText = \_ _ _ -> pure $ Right (ChatResponse "" [] Nothing Nothing),
      gwGenerateObject = \_ _ _ -> pure $ Right (object [], Nothing)
    }

-- | (call id, content) of the results in the last turn, if it is a tool turn.
lastToolResults :: [Turn] -> Maybe [(Text, Text)]
lastToolResults turns = case reverse turns of
  ToolTurn results : _ -> Just [(r.trCallId, r.trContent) | r <- results]
  _ -> Nothing

runStreamGenerate ::
  Agent ->
  ModelWithFallbacks ->
  ToolMap Text ->
  Maybe AbortSignal ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
runStreamGenerate agent models toolMap mSig turns = do
  rt <- mkRuntime mSig
  streamText (\_ -> pure ()) agent models toolMap rt turns

-- | Gateway that records each request conversation, then behaves like 'mockToolGateway'.
capturingToolGateway :: IORef [[Turn]] -> LLMGateway
capturingToolGateway ref =
  let respond req = do
        modifyIORef' ref (req.reqConversation :)
        if any isToolTurn req.reqConversation
          then pure $ Right (ChatResponse "The weather is sunny." [textPart "The weather is sunny."] (Just (mkUsage 80 15)) Nothing)
          else
            let tc = mkToolCall "call_1" "get_weather" (object ["location" .= ("London" :: Text)])
             in pure $ Right (ChatResponse "" [toolCallPart tc] (Just (mkUsage 50 10)) Nothing)
   in LLMGateway
        { gwName = "mock-tool-capture",
          gwImageMediaTypes = supportedImageMediaTypes,
          gwImageUrlSupport = True,
          gwGenerateText = \_ -> respond,
          gwStreamText = \_ req _onEvent -> respond req,
          gwGenerateObject = \_ _ _ -> pure $ Right (object [], Nothing)
        }
  where
    isToolTurn (ToolTurn _) = True
    isToolTurn _ = False

-- | Gateway that appends its label to @calls@ on every request, then answers
-- with @respond@ (given the request).
loggingGateway :: IORef [Text] -> Text -> (ChatRequest -> IO (Either LLMError ChatResponse)) -> LLMGateway
loggingGateway calls label respond =
  let call req = modifyIORef' calls (++ [label]) >> respond req
   in LLMGateway
        { gwName = label,
          gwImageMediaTypes = supportedImageMediaTypes,
          gwImageUrlSupport = True,
          gwGenerateText = \_ -> call,
          gwStreamText = \_ req _onEvent -> call req,
          gwGenerateObject = \_ _ _ -> pure $ Right (object [], Nothing)
        }

-- | Tool call when the conversation has no tool turn yet, text afterwards
-- (like 'mockToolGateway').
toolThenText :: ChatRequest -> IO (Either LLMError ChatResponse)
toolThenText req
  | any isToolTurn req.reqConversation =
      pure $ Right (ChatResponse "The weather is sunny." [textPart "The weather is sunny."] (Just (mkUsage 80 15)) Nothing)
  | otherwise =
      let tc = mkToolCall "call_1" "get_weather" (object ["location" .= ("London" :: Text)])
       in pure $ Right (ChatResponse "" [toolCallPart tc] (Just (mkUsage 50 10)) Nothing)
  where
    isToolTurn (ToolTurn _) = True
    isToolTurn _ = False

-- | Fail while the conversation has no tool turn, then behave like 'toolThenText'.
failFirstRound :: ChatRequest -> IO (Either LLMError ChatResponse)
failFirstRound req
  | any isToolTurn req.reqConversation = toolThenText req
  | otherwise = pure $ Left (HttpError 503 "down")
  where
    isToolTurn (ToolTurn _) = True
    isToolTurn _ = False

-- | Tool call in the first round, failure afterwards.
toolThenFail :: ChatRequest -> IO (Either LLMError ChatResponse)
toolThenFail req
  | any isToolTurn req.reqConversation = pure $ Left (HttpError 503 "down")
  | otherwise = toolThenText req
  where
    isToolTurn (ToolTurn _) = True
    isToolTurn _ = False

alwaysFail :: ChatRequest -> IO (Either LLMError ChatResponse)
alwaysFail _ = pure $ Left (HttpError 503 "down")

-- | Gateway that records every request, then answers with @respond@.
recordingGateway :: IORef [ChatRequest] -> (ChatRequest -> IO (Either LLMError ChatResponse)) -> LLMGateway
recordingGateway reqs respond =
  let call req = modifyIORef' reqs (++ [req]) >> respond req
   in LLMGateway
        { gwName = "recording",
          gwImageMediaTypes = supportedImageMediaTypes,
          gwImageUrlSupport = True,
          gwGenerateText = \_ -> call,
          gwStreamText = \_ req _onEvent -> call req,
          gwGenerateObject = \_ _ _ -> pure $ Right (object [], Nothing)
        }

-- | Always one call of @get_weather@, numbered by the tool turns so far.
alwaysTool :: ChatRequest -> IO (Either LLMError ChatResponse)
alwaysTool req =
  let n = length [() | ToolTurn _ <- req.reqConversation]
      tc = mkToolCall ("call_" <> T.pack (show n)) "get_weather" (object [])
   in pure $ Right (ChatResponse "" [toolCallPart tc] Nothing Nothing)

-- | Calls a tool until the final-call line is in the system prompt, then
-- answers in text.
toolsUntilFinalCall :: ChatRequest -> IO (Either LLMError ChatResponse)
toolsUntilFinalCall req
  | isFinalCall req = pure $ Right (ChatResponse "Done." [textPart "Done."] Nothing Nothing)
  | otherwise = alwaysTool req

isFinalCall :: ChatRequest -> Bool
isFinalCall req = maybe False (toolBudgetUsedUpPrompt `T.isInfixOf`) req.reqSystem

-- | Every tool call in a history has a result with its id in the next turn.
everyCallAnswered :: [Turn] -> Bool
everyCallAnswered turns = and (zipWith answered turns (drop 1 turns ++ [UserTurn ""]))
  where
    answered (AssistantMessage parts) next =
      let ids = map (.tcId) (getToolCalls (ChatResponse "" parts Nothing Nothing))
       in null ids || case next of
            ToolTurn rs -> map (.trCallId) rs == ids
            _ -> False
    answered _ _ = True

-- | A runtime whose hooks record log lines and tool errors.
recordingRuntime :: IO (RuntimeArgs, IORef [(LogLevel, Text)], IORef [(Text, Text)])
recordingRuntime = do
  logs <- newIORef []
  toolErrors <- newIORef []
  rt0 <- mkRuntime Nothing
  let hooks =
        noHooks
          { onLog = \lvl msg -> modifyIORef' logs (++ [(lvl, msg)]),
            onToolError = \name msg -> modifyIORef' toolErrors (++ [(name, msg)])
          }
  pure (rt0 {rtHooks = hooks}, logs, toolErrors)

-- | Shared sticky-fallback checks, run for both 'runGenerate' and 'runStreamGenerate'.
stickyFallbackSpec ::
  (Agent -> ModelWithFallbacks -> ToolMap Text -> Maybe AbortSignal -> [Turn] -> IO (Either GenerateErrorResult GenerateTextResult)) ->
  ToolMap Text ->
  Spec
stickyFallbackSpec run toolMap = do
  let agent = defaultAgent {agTools = ["get_weather"]}
  it "keeps using the fallback that answered for the rest of the run" $ do
    calls <- newIORef []
    let primary = mockModel (loggingGateway calls "primary" alwaysFail)
        fallback = mockModel (loggingGateway calls "fallback" toolThenText)
    result <- run agent (ModelWithFallbacks primary [fallback]) toolMap Nothing [UserTurn "weather?"]
    case result of
      Right r -> r.gtrText `shouldBe` "The weather is sunny."
      Left err -> expectationFailure $ show err
    readIORef calls >>= (`shouldBe` ["primary", "fallback", "fallback"])

  it "tries the remaining models, the earlier-failed one last, when the sticky model fails" $ do
    calls <- newIORef []
    let a = mockModel (loggingGateway calls "a" failFirstRound)
        b = mockModel (loggingGateway calls "b" toolThenFail)
        c = mockModel (loggingGateway calls "c" alwaysFail)
    result <- run agent (ModelWithFallbacks a [b, c]) toolMap Nothing [UserTurn "weather?"]
    case result of
      Right r -> r.gtrText `shouldBe` "The weather is sunny."
      Left err -> expectationFailure $ show err
    readIORef calls >>= (`shouldBe` ["a", "b", "b", "c", "a"])

  it "starts from the caller's chain again on the next call" $ do
    calls <- newIORef []
    let primary = mockModel (loggingGateway calls "primary" alwaysFail)
        fallback = mockModel (loggingGateway calls "fallback" toolThenText)
        models = ModelWithFallbacks primary [fallback]
    _ <- run agent models toolMap Nothing [UserTurn "weather?"]
    _ <- run agent models toolMap Nothing [UserTurn "weather again?"]
    readIORef calls >>= (`shouldBe` ["primary", "fallback", "fallback", "primary", "fallback", "fallback"])

hasCachedUserPrefix :: [Turn] -> Bool
hasCachedUserPrefix =
  any
    ( \case
        UserMessage parts ->
          any
            ( \case
                ContentPart (TextPart _) (Just CacheEphemeral) -> True
                _ -> False
            )
            parts
        _ -> False
    )

spec :: Spec
spec = describe "Chat" $ do
  let toolMap = Map.fromList [("get_weather", weatherTool)]
  describe "generateText" $ do
    it "returns text for a simple response" $ do
      let gw = mockGateway (ChatResponse "Hi there!" [textPart "Hi there!"] (Just (mkUsage 10 5)) Nothing)
          models = ModelWithFallbacks (mockModel gw) []
      result <- runGenerate defaultAgent models toolMap Nothing [UserTurn "hello"]
      case result of
        Right r -> do
          r.gtrText `shouldBe` "Hi there!"
          length r.gtrNewMessages `shouldBe` 1 -- assistantTurn
          r.gtrUsage `shouldBe` mkUsage 10 5
        Left err -> expectationFailure $ show err

    it "propagates errors" $ do
      let gw = mockErrorGateway (HttpError 500 "internal error")
          models = ModelWithFallbacks (mockModel gw) []
      result <- runGenerate defaultAgent models toolMap Nothing [UserTurn "hello"]
      case result of
        Left GenerateErrorResult {gerError = GErrLLM (HttpError 500 _)} -> pure ()
        other -> expectationFailure $ "Expected HttpError 500, got: " <> show other

    it "handles tool call loop" $ do
      let agent = defaultAgent {agTools = ["get_weather"]}
          models = ModelWithFallbacks (mockModel mockToolGateway) []
      result <- runGenerate agent models toolMap Nothing [UserTurn "weather in london?"]
      case result of
        Right r -> do
          r.gtrText `shouldBe` "The weather is sunny."
          -- assistantTurn(tool call) + ToolTurn + assistantTurn(final)
          length r.gtrNewMessages `shouldBe` 3
          r.gtrUsage `shouldBe` mkUsage 130 25 -- 50+80 input, 10+15 output
        Left err -> expectationFailure $ show err

    it "preserves cache hints on user parts across non-streaming tool rounds" $ do
      ref <- newIORef []
      let agent = defaultAgent {agTools = ["get_weather"]}
          models = ModelWithFallbacks (mockModel (capturingToolGateway ref)) []
          msgs =
            [ UserMessage
                [ cacheEphemeral (textPart "static system-like context"),
                  textPart "weather in london?"
                ]
            ]
      result <- runGenerate agent models toolMap Nothing msgs
      case result of
        Right r -> do
          r.gtrText `shouldBe` "The weather is sunny."
          convs <- reverse <$> readIORef ref
          length convs `shouldBe` 2
          mapM_ (`shouldSatisfy` hasCachedUserPrefix) convs
        Left err -> expectationFailure $ show err

    it "falls back to next model on retryable error" $ do
      let failGw = mockErrorGateway (HttpError 503 "service unavailable")
          okGw = mockGateway (ChatResponse "Fallback worked!" [textPart "Fallback worked!"] (Just (mkUsage 10 5)) Nothing)
          models = ModelWithFallbacks (mockModel failGw) [mockModel okGw]
      result <- runGenerate defaultAgent models toolMap Nothing [UserTurn "hello"]
      case result of
        Right r -> r.gtrText `shouldBe` "Fallback worked!"
        Left err -> expectationFailure $ "Expected fallback success, got: " <> show err

    it "falls back on non-retryable error too" $ do
      let failGw = mockErrorGateway (HttpError 400 "bad request")
          okGw = mockGateway (ChatResponse "Fallback worked!" [textPart "Fallback worked!"] (Just (mkUsage 10 5)) Nothing)
          models = ModelWithFallbacks (mockModel failGw) [mockModel okGw]
      result <- runGenerate defaultAgent models toolMap Nothing [UserTurn "hello"]
      case result of
        Right r -> r.gtrText `shouldBe` "Fallback worked!"
        Left err -> expectationFailure $ "Expected fallback success, got: " <> show err

    it "skips a non-vision model and succeeds with a vision-capable fallback" $ do
      let boomGw =
            LLMGateway
              { gwName = "should-not-run",
                gwImageMediaTypes = supportedImageMediaTypes,
                gwImageUrlSupport = True,
                gwGenerateText = \_ _ -> pure (Left (HttpError 500 "should not be called")),
                gwStreamText = \_ _ _ -> pure (Left (HttpError 500 "should not be called")),
                gwGenerateObject = \_ _ _ -> pure (Left (HttpError 500 "should not be called"))
              }
          okGw = mockGateway (ChatResponse "I see a cat." [textPart "I see a cat."] (Just (mkUsage 10 5)) Nothing)
          noVision = mockModel boomGw
          withVision =
            (mockModel okGw)
              { mcCapabilities = defaultModelCapabilities {capVision = True}
              }
          models = ModelWithFallbacks noVision [withVision]
          msgs = [UserMessage [imageUrlPart "https://example.com/cat.png", textPart "what is this?"]]
      result <- runGenerate defaultAgent models toolMap Nothing msgs
      case result of
        Right r -> r.gtrText `shouldBe` "I see a cat."
        Left err -> expectationFailure $ "Expected vision fallback success, got: " <> show err

    it "returns UnsupportedCapability when no vision-capable model remains" $ do
      let noVision1 = mockModel (mockErrorGateway (HttpError 500 "unused"))
          noVision2 = mockModel (mockErrorGateway (HttpError 500 "unused"))
          models = ModelWithFallbacks noVision1 [noVision2]
          msgs = [UserMessage [imageUrlPart "https://example.com/cat.png", textPart "what?"]]
      result <- runGenerate defaultAgent models toolMap Nothing msgs
      case result of
        Left GenerateErrorResult {gerError = GErrLLM (UnsupportedCapability _)} -> pure ()
        other -> expectationFailure $ "Expected UnsupportedCapability, got: " <> show other

    it "skips a model whose gateway does not accept the image media type" $ do
      calls <- newIORef []
      Right heic <- pure $ imageBase64Part "image/heic" "aGVsbG8="
      let vision = defaultModelCapabilities {capVision = True}
          claudeLike =
            (mockModel ((loggingGateway calls "claude-like" toolThenText) {gwImageMediaTypes = commonImageMediaTypes}))
              { mcCapabilities = vision
              }
          geminiLike =
            (mockModel ((loggingGateway calls "gemini-like" toolThenText) {gwImageMediaTypes = supportedImageMediaTypes}))
              { mcCapabilities = vision
              }
          msgs = [UserMessage [heic, textPart "what is this?"]]
      result <- runGenerate defaultAgent (ModelWithFallbacks claudeLike [geminiLike]) toolMap Nothing msgs
      case result of
        Right r -> r.gtrText `shouldBe` "The weather is sunny."
        Left err -> expectationFailure $ "Expected media-type fallback success, got: " <> show err
      readIORef calls >>= (`shouldBe` ["gemini-like", "gemini-like"])

    it "returns UnsupportedCapability when no model accepts the image media type" $ do
      calls <- newIORef []
      Right heic <- pure $ imageBase64Part "image/heic" "aGVsbG8="
      let claudeLike name =
            (mockModel ((loggingGateway calls name toolThenText) {gwImageMediaTypes = commonImageMediaTypes}))
              { mcCapabilities = defaultModelCapabilities {capVision = True}
              }
          msgs = [UserMessage [heic, textPart "what is this?"]]
      result <- runGenerate defaultAgent (ModelWithFallbacks (claudeLike "a") [claudeLike "b"]) toolMap Nothing msgs
      case result of
        Left GenerateErrorResult {gerError = GErrLLM (UnsupportedCapability _)} -> pure ()
        other -> expectationFailure $ "Expected UnsupportedCapability, got: " <> show other
      readIORef calls >>= (`shouldBe` [])

    it "skips a model whose gateway does not accept image URLs" $ do
      calls <- newIORef []
      let vision = defaultModelCapabilities {capVision = True}
          ollamaLike =
            (mockModel ((loggingGateway calls "ollama-like" toolThenText) {gwImageUrlSupport = False}))
              { mcCapabilities = vision
              }
          urlOk = (mockModel (loggingGateway calls "url-ok" toolThenText)) {mcCapabilities = vision}
          msgs = [UserMessage [imageUrlPart "https://example.com/cat.png", textPart "what is this?"]]
      result <- runGenerate defaultAgent (ModelWithFallbacks ollamaLike [urlOk]) toolMap Nothing msgs
      case result of
        Right r -> r.gtrText `shouldBe` "The weather is sunny."
        Left err -> expectationFailure $ "Expected URL-image fallback success, got: " <> show err
      readIORef calls >>= (`shouldBe` ["url-ok", "url-ok"])

    it "accepts base64 images on a gateway without image URL support" $ do
      calls <- newIORef []
      Right png <- pure $ imageBase64Part "image/png" "aGVsbG8="
      let ollamaLike =
            (mockModel ((loggingGateway calls "ollama-like" toolThenText) {gwImageUrlSupport = False}))
              { mcCapabilities = defaultModelCapabilities {capVision = True}
              }
          msgs = [UserMessage [png, textPart "what is this?"]]
      result <- runGenerate defaultAgent (ModelWithFallbacks ollamaLike []) toolMap Nothing msgs
      case result of
        Right r -> r.gtrText `shouldBe` "The weather is sunny."
        Left err -> expectationFailure $ show err

    it "returns error from last model when all fail" $ do
      let failGw1 = mockErrorGateway (HttpError 503 "service unavailable")
          failGw2 = mockErrorGateway (HttpError 400 "bad request")
          models = ModelWithFallbacks (mockModel failGw1) [mockModel failGw2]
      result <- runGenerate defaultAgent models toolMap Nothing [UserTurn "hello"]
      case result of
        Left GenerateErrorResult {gerError = GErrLLM (HttpError 400 _)} -> pure ()
        other -> expectationFailure $ "Expected HttpError 400 from last model, got: " <> show other

    it "returns Aborted when signal is fired before the call" $ do
      let gw = mockGateway (ChatResponse "Hi!" [textPart "Hi!"] Nothing Nothing)
          models = ModelWithFallbacks (mockModel gw) []
      sig <- newAbortSignal
      abort sig
      result <- runGenerate defaultAgent models toolMap (Just sig) [UserTurn "hello"]
      case result of
        Left GenerateErrorResult {gerError = GErrAborted} -> pure ()
        other -> expectationFailure $ "Expected GErrAborted, got: " <> show other

    it "stops before the next tool call once the signal fired" $ do
      sig <- newAbortSignal
      let tm = Map.fromList [("slow", constTool "slow" (pure "done"))]
          agent = defaultAgent {agTools = ["slow"]}
          models = ModelWithFallbacks (mockModel (twoToolCallGateway "slow")) []
      rt <- mkRuntime (Just sig)
      -- Fire the signal once the first call has finished.
      let rt' = rt {rtHooks = noHooks {onToolResult = \_ _ -> abort sig}}
      result <- generateText agent models tm rt' [UserTurn "go"]
      case result of
        Left GenerateErrorResult {gerError = GErrAborted, gerPartialNewMessages} ->
          -- Every tool call in the history must have a result, or the next
          -- request is rejected. c1 ran; c2 never did.
          lastToolResults gerPartialNewMessages
            `shouldBe` Just [("c1", "done"), ("c2", "Aborted before execution")]
        other -> expectationFailure $ "Expected GErrAborted during tools, got: " <> show other

    it "interrupts a running tool" $ do
      sig <- newAbortSignal
      started <- newEmptyMVar
      finished <- newIORef False
      errors <- newIORef []
      let hangingTool =
            constTool "hang" $
              (putMVar started () >> threadDelay 60_000_000 >> pure "done")
                `finally` writeIORef finished True
          tm = Map.fromList [("hang", hangingTool)]
          agent = defaultAgent {agTools = ["hang"]}
          models = ModelWithFallbacks (mockModel (twoToolCallGateway "hang")) []
      rt <- mkRuntime (Just sig)
      let rt' = rt {rtHooks = noHooks {onToolError = \name msg -> modifyIORef' errors ((name, msg) :)}}
      _ <- forkIO (takeMVar started >> abort sig)
      result <- timeout 5_000_000 $ generateText agent models tm rt' [UserTurn "go"]
      case result of
        Just (Left GenerateErrorResult {gerError = GErrAborted, gerPartialNewMessages}) ->
          -- The interrupted call counts as not run.
          lastToolResults gerPartialNewMessages
            `shouldBe` Just [("c1", "Aborted before execution"), ("c2", "Aborted before execution")]
        other -> expectationFailure $ "Expected GErrAborted from the running tool, got: " <> show other
      readIORef finished `shouldReturn` True
      readIORef errors `shouldReturn` [("hang", "Aborted")]

    describe "sticky fallback" $ stickyFallbackSpec runGenerate toolMap

  describe "streamText" $ do
    describe "sticky fallback" $ stickyFallbackSpec runStreamGenerate toolMap

    it "preserves cache hints on user parts across streaming tool rounds" $ do
      ref <- newIORef []
      let agent = defaultAgent {agTools = ["get_weather"]}
          models = ModelWithFallbacks (mockModel (capturingToolGateway ref)) []
          msgs =
            [ UserMessage
                [ cacheEphemeral (textPart "static system-like context"),
                  textPart "weather in london?"
                ]
            ]
      result <- runStreamGenerate agent models toolMap Nothing msgs
      case result of
        Right r -> do
          r.gtrText `shouldBe` "The weather is sunny."
          convs <- reverse <$> readIORef ref
          length convs `shouldBe` 2
          mapM_ (`shouldSatisfy` hasCachedUserPrefix) convs
        Left err -> expectationFailure $ show err

  describe "throwing hooks" $ do
    it "an onToolResult that throws does not fail the run after a mutating tool ran" $ do
      ran <- newIORef (0 :: Int)
      let tool = constTool "get_weather" (modifyIORef' ran (+ 1) >> pure "done")
          agent = defaultAgent {agTools = ["get_weather"]}
          models = ModelWithFallbacks (mockModel mockToolGateway) []
      rt0 <- mkRuntime Nothing
      let rt = rt0 {rtHooks = noHooks {onToolResult = \_ _ -> ioError (userError "hook failed")}}
      result <- generateText agent models (Map.fromList [("get_weather", tool)]) rt [UserTurn "weather?"]
      readIORef ran `shouldReturn` 1
      case result of
        Right r -> do
          r.gtrText `shouldBe` "The weather is sunny."
          lastToolResults (init r.gtrNewMessages) `shouldBe` Just [("call_1", "done")]
        Left err -> expectationFailure $ show err

    it "an onLog that throws does not stop a model fallback" $ do
      let gr =
            GenRequest
              { grSystemPrompt = Nothing,
                grMessages = [UserTurn "hi"],
                grTools = [],
                grAbortSignal = Nothing,
                grLLMHooks = llmHooks noHooks,
                grHooks = noHooks {onLog = \_ _ -> ioError (userError "log failed")}
              }
          ok = ChatResponse "fallback answer" [textPart "fallback answer"] Nothing Nothing
          models = ModelWithFallbacks (mockModel (mockErrorGateway (HttpError 503 "down"))) [mockModel (mockGateway ok)]
      result <- generateTextWithFallbacks gr models
      fmap (.respText) result `shouldBe` Right "fallback answer"

    it "an LLM wire hook that throws does not fail the call" $ do
      let gw = (mockGateway (ChatResponse "x" [textPart "x"] Nothing Nothing))
            { gwGenerateText = \hooks _ -> do
                hooks.onLLMRequest "mock" (object [])
                pure (Right (ChatResponse "x" [textPart "x"] Nothing Nothing))
            }
          throwing = (llmHooks noHooks) {onLLMRequest = \_ _ -> ioError (userError "wire hook failed")}
      rt0 <- mkRuntime Nothing
      result <- generateText defaultAgent (ModelWithFallbacks (mockModel gw) []) Map.empty rt0 {rtLLMHooks = throwing} [UserTurn "hi"]
      fmap (.gtrText) result `shouldBe` Right "x"

  describe "tool round limit (final call)" $ do
    let countingTool ref = Map.fromList [("get_weather", constTool "get_weather" (modifyIORef' ref (+ 1) >> pure "Sunny"))]
        limited n = defaultAgent {agMaxToolRounds = n, agTools = ["get_weather"], agSystemPrompt = Just "Be brief."}

    it "runs tools exactly N times, then makes one final call that runs none" $ do
      reqs <- newIORef []
      ran <- newIORef (0 :: Int)
      result <- runGenerate (limited 2) (ModelWithFallbacks (mockModel (recordingGateway reqs alwaysTool)) []) (countingTool ran) Nothing [UserTurn "go"]
      readIORef ran `shouldReturn` 2
      sent <- readIORef reqs
      map isFinalCall sent `shouldBe` [False, False, True]
      fmap (.reqSystem) (drop 2 sent) `shouldBe` [Just ("Be brief.\n\n" <> toolBudgetUsedUpPrompt)]
      -- The final call still declares the tools.
      map (map (.toolName) . (.reqTools)) sent `shouldBe` replicate 3 ["get_weather"]
      case result of
        Left GenerateErrorResult {gerError = GErrToolExceeded, gerPartialNewMessages = msgs} -> do
          length msgs `shouldBe` 6
          msgs `shouldSatisfy` everyCallAnswered
          lastToolResults msgs `shouldBe` Just [("call_2", "Not run: tool round limit reached")]
        other -> expectationFailure $ "Expected GErrToolExceeded, got: " <> show other

    it "returns the final call's text answer as a success" $ do
      reqs <- newIORef []
      ran <- newIORef (0 :: Int)
      result <- runGenerate (limited 1) (ModelWithFallbacks (mockModel (recordingGateway reqs toolsUntilFinalCall)) []) (countingTool ran) Nothing [UserTurn "go"]
      readIORef ran `shouldReturn` 1
      case result of
        Right r -> do
          r.gtrText `shouldBe` "Done."
          length r.gtrNewMessages `shouldBe` 3
          r.gtrNewMessages `shouldSatisfy` everyCallAnswered
        Left err -> expectationFailure $ show err

    it "adds the line as the whole system prompt when the agent has none" $ do
      reqs <- newIORef []
      ran <- newIORef (0 :: Int)
      _ <- runGenerate (limited 0) {agSystemPrompt = Nothing} (ModelWithFallbacks (mockModel (recordingGateway reqs toolsUntilFinalCall)) []) (countingTool ran) Nothing [UserTurn "go"]
      map (.reqSystem) <$> readIORef reqs `shouldReturn` [Just toolBudgetUsedUpPrompt]

    it "agMaxToolRounds = 0 makes exactly one call and runs no tool" $ do
      reqs <- newIORef []
      ran <- newIORef (0 :: Int)
      result <- runGenerate (limited 0) (ModelWithFallbacks (mockModel (recordingGateway reqs alwaysTool)) []) (countingTool ran) Nothing [UserTurn "go"]
      readIORef ran `shouldReturn` 0
      length <$> readIORef reqs `shouldReturn` 1
      case result of
        Left GenerateErrorResult {gerError = GErrToolExceeded, gerPartialNewMessages = msgs} -> do
          length msgs `shouldBe` 2
          msgs `shouldSatisfy` everyCallAnswered
        other -> expectationFailure $ "Expected GErrToolExceeded, got: " <> show other

    it "a negative agMaxToolRounds behaves like 0" $ do
      reqs <- newIORef []
      ran <- newIORef (0 :: Int)
      _ <- runGenerate (limited (-3)) (ModelWithFallbacks (mockModel (recordingGateway reqs alwaysTool)) []) (countingTool ran) Nothing [UserTurn "go"]
      readIORef ran `shouldReturn` 0
      map isFinalCall <$> readIORef reqs `shouldReturn` [True]

    it "streamText makes the same final call" $ do
      reqs <- newIORef []
      ran <- newIORef (0 :: Int)
      result <- runStreamGenerate (limited 1) (ModelWithFallbacks (mockModel (recordingGateway reqs alwaysTool)) []) (countingTool ran) Nothing [UserTurn "go"]
      readIORef ran `shouldReturn` 1
      map isFinalCall <$> readIORef reqs `shouldReturn` [False, True]
      fmap (const ()) result `shouldSatisfy` either (\e -> e.gerError == GErrToolExceeded) (const False)

  describe "agent configuration checks" $ do
    let warnings logs = [m | (Warn, m) <- logs]

    it "sends a tool listed twice once, with a warning" $ do
      reqs <- newIORef []
      (rt, logs, _) <- recordingRuntime
      let agent = defaultAgent {agTools = ["get_weather", "get_weather"]}
      _ <- generateText agent (ModelWithFallbacks (mockModel (recordingGateway reqs toolThenText)) []) toolMap rt [UserTurn "go"]
      sent <- readIORef reqs
      map (map (.toolName) . (.reqTools)) sent `shouldBe` [["get_weather"], ["get_weather"]]
      ws <- warnings <$> readIORef logs
      length (filter ("more than once" `T.isInfixOf`) ws) `shouldBe` 1

    it "logs an unknown tool name once per run, not once per round" $ do
      reqs <- newIORef []
      (rt, logs, _) <- recordingRuntime
      let agent = defaultAgent {agTools = ["no_such_tool", "get_weather"]}
      result <- generateText agent (ModelWithFallbacks (mockModel (recordingGateway reqs toolThenText)) []) toolMap rt [UserTurn "go"]
      fmap (.gtrText) result `shouldBe` Right "The weather is sunny."
      length <$> readIORef reqs `shouldReturn` 2
      ws <- warnings <$> readIORef logs
      filter ("no_such_tool" `T.isInfixOf`) ws
        `shouldBe` ["[test] agTools lists no_such_tool, which is not in the tool map; it is not sent to the model"]

    it "uses the built-in get_history over a caller tool of that name when windowing is on" $ do
      reqs <- newIORef []
      (rt, logs, _) <- recordingRuntime
      let callerHistory = constTool "get_history" (pure "caller history")
          tm = Map.insert "get_history" callerHistory toolMap
          agent = defaultAgent {agTools = ["get_weather", "get_history"], agContextWindow = Just 2}
          gw = mockGateway (ChatResponse "ok" [textPart "ok"] Nothing Nothing)
      _ <- generateText agent (ModelWithFallbacks (mockModel (recordingGateway reqs (gw.gwGenerateText (llmHooks noHooks)))) []) tm rt [UserTurn "go"]
      [req] <- readIORef reqs
      let histories = [d | d <- req.reqTools, d.toolName == "get_history"]
      map (.toolDescription) histories `shouldSatisfy` \ds -> length ds == 1 && ds /= ["test tool"]
      ws <- warnings <$> readIORef logs
      length (filter ("get_history" `T.isInfixOf`) ws) `shouldBe` 1

    it "keeps a caller tool named get_history when windowing is off" $ do
      reqs <- newIORef []
      (rt, logs, _) <- recordingRuntime
      let tm = Map.insert "get_history" (constTool "get_history" (pure "caller history")) toolMap
          agent = defaultAgent {agTools = ["get_history"]}
          gw = mockGateway (ChatResponse "ok" [textPart "ok"] Nothing Nothing)
      _ <- generateText agent (ModelWithFallbacks (mockModel (recordingGateway reqs (gw.gwGenerateText (llmHooks noHooks)))) []) tm rt [UserTurn "go"]
      [req] <- readIORef reqs
      map (.toolDescription) req.reqTools `shouldBe` ["test tool"]
      warnings <$> readIORef logs `shouldReturn` []

    it "fires onToolError for a call to an unknown tool" $ do
      (rt, _, toolErrors) <- recordingRuntime
      let gw = twoToolCallGateway "no_such_tool"
          agent = defaultAgent {agMaxToolRounds = 1}
      _ <- generateText agent (ModelWithFallbacks (mockModel gw) []) toolMap rt [UserTurn "go"]
      errs <- readIORef toolErrors
      errs `shouldContain` [("no_such_tool", "Unknown tool: no_such_tool")]
      length errs `shouldBe` 2

    it "treats a non-positive context window as no windowing, with one warning" $ do
      let conv = [UserTurn "one", AssistantMessage [textPart "a"], UserTurn "two"]
      mapM_
        ( \n -> do
            reqs <- newIORef []
            (rt, logs, _) <- recordingRuntime
            let agent = defaultAgent {agTools = ["get_weather"], agContextWindow = Just n}
            _ <- generateText agent (ModelWithFallbacks (mockModel (recordingGateway reqs toolThenText)) []) toolMap rt conv
            sent <- readIORef reqs
            -- The whole conversation is sent and no get_history is added.
            map (take 3 . (.reqConversation)) sent `shouldBe` replicate (length sent) conv
            map (map (.toolName) . (.reqTools)) sent `shouldBe` replicate (length sent) ["get_weather"]
            ws <- warnings <$> readIORef logs
            filter ("agContextWindow" `T.isInfixOf`) ws
              `shouldBe` ["[test] agContextWindow = Just " <> T.pack (show n) <> " is not positive; the whole conversation is sent"]
        )
        [0, -1]

    it "validateAgent reports nothing for a valid agent" $ do
      validateAgent defaultAgent {agTools = ["get_weather"], agContextWindow = Just 3} toolMap `shouldBe` []

  describe "turn validation (warning only)" $ do
    let invalidHistory =
          [ UserTurn "look at this",
            AssistantMessage [textPart "here"],
            UserMessage [textPart "and now?", toolCallPart (mkToolCall "c1" "get_weather" (object []))]
          ]
        request logs turns =
          GenRequest
            { grSystemPrompt = Nothing,
              grMessages = turns,
              grTools = [],
              grAbortSignal = Nothing,
              grLLMHooks = llmHooks noHooks,
              grHooks = noHooks {onLog = \lvl msg -> modifyIORef' logs (++ [(lvl, msg)])}
            }
        answer _ = pure $ Right (ChatResponse "ok" [textPart "ok"] Nothing Nothing)

    it "logs one warning per invalid turn and still sends the request" $ do
      logs <- newIORef []
      calls <- newIORef []
      result <- generateTextWithFallbacks (request logs invalidHistory) (ModelWithFallbacks (mockModel (loggingGateway calls "m" answer)) [])
      fmap (.respText) result `shouldBe` Right "ok"
      readIORef calls `shouldReturn` ["m"]
      ws <- (\ls -> [m | (Warn, m) <- ls]) <$> readIORef logs
      ws `shouldBe` ["turn 2: user messages may not contain tool-call parts"]

    it "logs once per request, not once per fallback model" $ do
      logs <- newIORef []
      calls <- newIORef []
      let models = ModelWithFallbacks (mockModel (loggingGateway calls "a" alwaysFail)) [mockModel (loggingGateway calls "b" answer)]
      _ <- streamTextWithFallbacks (\_ -> pure ()) (request logs invalidHistory) models
      readIORef calls `shouldReturn` ["a", "b"]
      ws <- (\ls -> [m | (Warn, m) <- ls, "turn " `T.isPrefixOf` m]) <$> readIORef logs
      ws `shouldBe` ["turn 2: user messages may not contain tool-call parts"]

    it "logs nothing for a valid history" $ do
      logs <- newIORef []
      calls <- newIORef []
      _ <- generateTextWithFallbacks (request logs [UserTurn "hi", AssistantMessage [textPart "hello"], UserTurn "bye"]) (ModelWithFallbacks (mockModel (loggingGateway calls "m" answer)) [])
      ws <- (\ls -> [m | (Warn, m) <- ls]) <$> readIORef logs
      ws `shouldBe` []

  describe "agent name in log messages" $ do
    it "prefixes every onLog message of a run with [agName]" $ do
      (rt, logs, _) <- recordingRuntime
      let agent = defaultAgent {agName = "helper", agTools = ["get_weather", "no_such_tool"]}
      _ <- generateText agent (ModelWithFallbacks (mockModel mockToolGateway) []) toolMap rt [UserTurn "go"]
      msgs <- map snd <$> readIORef logs
      msgs `shouldSatisfy` (not . null)
      msgs `shouldSatisfy` all ("[helper] " `T.isPrefixOf`)
      msgs `shouldSatisfy` any ("Trying model" `T.isInfixOf`)

    it "prefixes the log messages of generateObjectUntyped" $ do
      (rt, logs, _) <- recordingRuntime
      _ <- generateObjectUntyped defaultAgent {agName = "helper"} (ModelWithFallbacks (mockModel mockToolGateway) []) rt [UserTurn "go"] (object [])
      msgs <- map snd <$> readIORef logs
      msgs `shouldSatisfy` (not . null)
      msgs `shouldSatisfy` all ("[helper] " `T.isPrefixOf`)

    it "adds no prefix for an empty name" $ do
      (rt, logs, _) <- recordingRuntime
      _ <- generateText defaultAgent {agName = ""} (ModelWithFallbacks (mockModel mockToolGateway) []) toolMap rt [UserTurn "go"]
      msgs <- map snd <$> readIORef logs
      msgs `shouldSatisfy` any ("Trying model" `T.isPrefixOf`)
