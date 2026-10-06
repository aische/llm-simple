{-# OPTIONS_GHC -Wno-missing-fields #-}

module LLM.ChatSpec (spec) where

import Data.Aeson (object, (.=))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map qualified as Map
import Data.Text (Text)
import Heptapod (generate)
import LLM.Agent.Events (noEventObserver)
import LLM.Agent.Generate (generateText, streamText)
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
    textPart, toolCallPart, mkChatResponse, assistantTurn, pattern UserTurn,
    Turn (..),
    cacheEphemeral,
    imageUrlPart,
    LLMError (..),
    LLMGateway (..),
    LLMHooks (..),
    ToolDef (ToolDef, toolDescription, toolName, toolParameters, toolReadonly),
    mkToolCall,
  )
import LLM.Core.Usage (PricingInfo (..), defaultPricingInfo, mkUsage)
import LLM.Generate.Logger (noHooks)
import LLM.Generate.ModelConfig
  ( ModelCapabilities (..),
    ModelConfig (..),
    ModelWithFallbacks (..),
    defaultModelCapabilities,
  )
import LLM.Generate.Types
  ( GenerateError (..),
    GenerateErrorResult (..),
    GenerateTextResult (..),
  )
import Test.Hspec
  ( Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
    shouldSatisfy,
  )

-- | A mock gateway that returns a fixed response
mockGateway :: ChatResponse -> LLMGateway
mockGateway resp =
  LLMGateway
    { gwName = "mock",
      gwGenerateText = \_ _ -> pure (Right resp),
      gwStreamText = \_ _ _ -> pure (Right resp),
      gwGenerateObject = \_ _ _ -> pure (Right (object [], Nothing))
    }

-- | A mock gateway that returns an error
mockErrorGateway :: LLMError -> LLMGateway
mockErrorGateway err =
  LLMGateway
    { gwName = "mock-error",
      gwGenerateText = \_ _ -> pure (Left err),
      gwStreamText = \_ _ _ -> pure (Left err),
      gwGenerateObject = \_ _ _ -> pure (Left err)
    }

-- | A mock gateway that calls a tool, then responds with text
mockToolGateway :: LLMGateway
mockToolGateway =
  LLMGateway
    { gwName = "mock-tool",
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
          gwGenerateText = \_ -> respond,
          gwStreamText = \_ req _onEvent -> respond req,
          gwGenerateObject = \_ _ _ -> pure $ Right (object [], Nothing)
        }
  where
    isToolTurn (ToolTurn _) = True
    isToolTurn _ = False

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

    it "respects maxToolRounds" $ do
      let infiniteToolGateway =
            LLMGateway
              { gwName = "mock-infinite",
                gwGenerateText = \_ _ ->
                  let tc = mkToolCall "call_1" "get_weather" (object [])
                   in pure $ Right (ChatResponse "" [toolCallPart tc] Nothing Nothing),
                gwStreamText = \_ _ _ -> pure $ Right (ChatResponse "" [] Nothing Nothing),
                gwGenerateObject = \_ _ _ -> pure $ Right (object [], Nothing)
              }
          agent = defaultAgent {agMaxToolRounds = 2, agTools = ["get_weather"]}
          models = ModelWithFallbacks (mockModel infiniteToolGateway) []
      result <- runGenerate agent models toolMap Nothing [UserTurn "test"]
      case result of
        Left GenerateErrorResult {gerError = GErrToolExceeded} -> pure ()
        other -> expectationFailure $ "Expected GErrToolExceeded, got: " <> show other

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

    it "returns Aborted during tool execution" $ do
      sig <- newAbortSignal
      let slowTool =
            Tool
              { toolDef =
                  ToolDef
                    { toolName = "slow",
                      toolDescription = "A slow tool",
                      toolReadonly = True,
                      toolParameters = object ["type" .= ("object" :: Text)]
                    },
                toolExecute = \_ _ -> do
                  abort sig
                  pure "done"
              }
          twoCallGw =
            LLMGateway
              { gwName = "mock-two",
                gwGenerateText = \_ _ ->
                  let tc1 = mkToolCall "c1" "slow" (object [])
                      tc2 = mkToolCall "c2" "slow" (object [])
                   in pure $ Right (ChatResponse "" [toolCallPart tc1, toolCallPart tc2] Nothing Nothing),
                gwStreamText = \_ _ _ -> pure $ Right (ChatResponse "" [] Nothing Nothing),
                gwGenerateObject = \_ _ _ -> pure $ Right (object [], Nothing)
              }
          tm = Map.fromList [("slow", slowTool)]
          agent = defaultAgent {agTools = ["slow"]}
          models = ModelWithFallbacks (mockModel twoCallGw) []
      result <- runGenerate agent models tm (Just sig) [UserTurn "go"]
      case result of
        Left GenerateErrorResult {gerError = GErrAborted} -> pure ()
        other -> expectationFailure $ "Expected GErrAborted during tools, got: " <> show other

    it "does not fall back on Aborted" $ do
      let gw = mockGateway (ChatResponse "Hi!" [textPart "Hi!"] Nothing Nothing)
          okGw = mockGateway (ChatResponse "Fallback" [textPart "Fallback"] Nothing Nothing)
          models = ModelWithFallbacks (mockModel gw) [mockModel okGw]
      sig <- newAbortSignal
      abort sig
      result <- runGenerate defaultAgent models Map.empty (Just sig) [UserTurn "hello"]
      case result of
        Left GenerateErrorResult {gerError = GErrAborted} -> pure ()
        other -> expectationFailure $ "Expected GErrAborted (no fallback), got: " <> show other

  describe "streamText" $ do
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
