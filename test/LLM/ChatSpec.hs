{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module LLM.ChatSpec (spec) where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import Heptapod (generate)
import LLM.Agent.Events (noEventObserver)
import LLM.Agent.Generate (generateText)
import LLM.Agent.Types
  ( Agent (..),
    RuntimeArgs (..),
    Tool (..),
  )
import LLM.Core.Abort (AbortSignal, abort, newAbortSignal)
import LLM.Core.Types
  ( ChatRequest (..),
    ChatResponse (..),
    ContentBlock (TextBlock, ToolCallBlock),
    LLMError (..),
    LLMGateway (..),
    LLMHooks (..),
    ToolCall (ToolCall),
    ToolDef (ToolDef, toolDescription, toolName, toolParameters, toolReadonly),
    Turn (..),
  )
import LLM.Core.Usage (PricingInfo (..), Usage (Usage))
import LLM.Generate.Logger (noHooks)
import LLM.Generate.ModelConfig
  ( ModelConfig (..),
    ModelWithFallbacks (..),
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
  )

-- | A mock gateway that returns a fixed response
mockGateway :: ChatResponse -> LLMGateway
mockGateway resp =
  LLMGateway
    { gwName = "mock",
      gwGenerateText = \_ _ -> pure (Right resp),
      gwStreamText = \_ _ _ -> pure (Right resp)
    }

-- | A mock gateway that returns an error
mockErrorGateway :: LLMError -> LLMGateway
mockErrorGateway err =
  LLMGateway
    { gwName = "mock-error",
      gwGenerateText = \_ _ -> pure (Left err),
      gwStreamText = \_ _ _ -> pure (Left err)
    }

-- | A mock gateway that calls a tool, then responds with text
mockToolGateway :: LLMGateway
mockToolGateway =
  LLMGateway
    { gwName = "mock-tool",
      gwGenerateText = \_ req ->
        if any isToolTurn req.reqConversation
          then pure $ Right (ChatResponse "The weather is sunny." [TextBlock "The weather is sunny."] (Just (Usage 80 15 0)) Nothing)
          else
            let tc = ToolCall "call_1" "get_weather" (object ["location" .= ("London" :: Text)])
             in pure $ Right (ChatResponse "" [ToolCallBlock tc] (Just (Usage 50 10 0)) Nothing),
      gwStreamText = \_ _ _ -> pure $ Right (ChatResponse "" [] Nothing Nothing)
    }
  where
    isToolTurn (ToolTurn _) = True
    isToolTurn _ = False

zeroPricing :: PricingInfo
zeroPricing = PricingInfo 0 0

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
      mcRequestTimeout = Nothing,
      mcThrottleDelay = Nothing,
      mcRetryCount = 0,
      mcJitterBackoff = 0
    }

weatherTool :: Tool
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
  Maybe AbortSignal ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
runGenerate agent models mSig turns = do
  rt <- mkRuntime mSig
  generateText agent models rt turns

spec :: Spec
spec = describe "Chat" $ do
  describe "generateText" $ do
    it "returns text for a simple response" $ do
      let gw = mockGateway (ChatResponse "Hi there!" [TextBlock "Hi there!"] (Just (Usage 10 5 0)) Nothing)
          models = ModelWithFallbacks (mockModel gw) []
      result <- runGenerate defaultAgent models Nothing [UserTurn "hello"]
      case result of
        Right r -> do
          r.gtrText `shouldBe` "Hi there!"
          length r.gtrNewMessages `shouldBe` 1 -- AssistantTurn
          r.gtrUsage `shouldBe` Usage 10 5 0
        Left err -> expectationFailure $ show err

    it "propagates errors" $ do
      let gw = mockErrorGateway (HttpError 500 "internal error")
          models = ModelWithFallbacks (mockModel gw) []
      result <- runGenerate defaultAgent models Nothing [UserTurn "hello"]
      case result of
        Left GenerateErrorResult {gerError = GErrLLM (HttpError 500 _)} -> pure ()
        other -> expectationFailure $ "Expected HttpError 500, got: " <> show other

    it "handles tool call loop" $ do
      let agent = defaultAgent {agTools = [weatherTool]}
          models = ModelWithFallbacks (mockModel mockToolGateway) []
      result <- runGenerate agent models Nothing [UserTurn "weather in london?"]
      case result of
        Right r -> do
          r.gtrText `shouldBe` "The weather is sunny."
          -- AssistantTurn(tool call) + ToolTurn + AssistantTurn(final)
          length r.gtrNewMessages `shouldBe` 3
          r.gtrUsage `shouldBe` Usage 130 25 0 -- 50+80 input, 10+15 output
        Left err -> expectationFailure $ show err

    it "respects maxToolRounds" $ do
      let infiniteToolGateway =
            LLMGateway
              { gwName = "mock-infinite",
                gwGenerateText = \_ _ ->
                  let tc = ToolCall "call_1" "get_weather" (object [])
                   in pure $ Right (ChatResponse "" [ToolCallBlock tc] Nothing Nothing),
                gwStreamText = \_ _ _ -> pure $ Right (ChatResponse "" [] Nothing Nothing)
              }
          agent = defaultAgent {agMaxToolRounds = 2, agTools = [weatherTool]}
          models = ModelWithFallbacks (mockModel infiniteToolGateway) []
      result <- runGenerate agent models Nothing [UserTurn "test"]
      case result of
        Left GenerateErrorResult {gerError = GErrToolExceeded} -> pure ()
        other -> expectationFailure $ "Expected GErrToolExceeded, got: " <> show other

    it "falls back to next model on retryable error" $ do
      let failGw = mockErrorGateway (HttpError 503 "service unavailable")
          okGw = mockGateway (ChatResponse "Fallback worked!" [TextBlock "Fallback worked!"] (Just (Usage 10 5 0)) Nothing)
          models = ModelWithFallbacks (mockModel failGw) [mockModel okGw]
      result <- runGenerate defaultAgent models Nothing [UserTurn "hello"]
      case result of
        Right r -> r.gtrText `shouldBe` "Fallback worked!"
        Left err -> expectationFailure $ "Expected fallback success, got: " <> show err

    it "falls back on non-retryable error too" $ do
      let failGw = mockErrorGateway (HttpError 400 "bad request")
          okGw = mockGateway (ChatResponse "Fallback worked!" [TextBlock "Fallback worked!"] (Just (Usage 10 5 0)) Nothing)
          models = ModelWithFallbacks (mockModel failGw) [mockModel okGw]
      result <- runGenerate defaultAgent models Nothing [UserTurn "hello"]
      case result of
        Right r -> r.gtrText `shouldBe` "Fallback worked!"
        Left err -> expectationFailure $ "Expected fallback success, got: " <> show err

    it "returns error from last model when all fail" $ do
      let failGw1 = mockErrorGateway (HttpError 503 "service unavailable")
          failGw2 = mockErrorGateway (HttpError 400 "bad request")
          models = ModelWithFallbacks (mockModel failGw1) [mockModel failGw2]
      result <- runGenerate defaultAgent models Nothing [UserTurn "hello"]
      case result of
        Left GenerateErrorResult {gerError = GErrLLM (HttpError 400 _)} -> pure ()
        other -> expectationFailure $ "Expected HttpError 400 from last model, got: " <> show other

    it "returns Aborted when signal is fired before the call" $ do
      let gw = mockGateway (ChatResponse "Hi!" [TextBlock "Hi!"] Nothing Nothing)
          models = ModelWithFallbacks (mockModel gw) []
      sig <- newAbortSignal
      abort sig
      result <- runGenerate defaultAgent models (Just sig) [UserTurn "hello"]
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
                  let tc1 = ToolCall "c1" "slow" (object [])
                      tc2 = ToolCall "c2" "slow" (object [])
                   in pure $ Right (ChatResponse "" [ToolCallBlock tc1, ToolCallBlock tc2] Nothing Nothing),
                gwStreamText = \_ _ _ -> pure $ Right (ChatResponse "" [] Nothing Nothing)
              }
          agent = defaultAgent {agTools = [slowTool]}
          models = ModelWithFallbacks (mockModel twoCallGw) []
      result <- runGenerate agent models (Just sig) [UserTurn "go"]
      case result of
        Left GenerateErrorResult {gerError = GErrAborted} -> pure ()
        other -> expectationFailure $ "Expected GErrAborted during tools, got: " <> show other

    it "does not fall back on Aborted" $ do
      let gw = mockGateway (ChatResponse "Hi!" [TextBlock "Hi!"] Nothing Nothing)
          okGw = mockGateway (ChatResponse "Fallback" [TextBlock "Fallback"] Nothing Nothing)
          models = ModelWithFallbacks (mockModel gw) [mockModel okGw]
      sig <- newAbortSignal
      abort sig
      result <- runGenerate defaultAgent models (Just sig) [UserTurn "hello"]
      case result of
        Left GenerateErrorResult {gerError = GErrAborted} -> pure ()
        other -> expectationFailure $ "Expected GErrAborted (no fallback), got: " <> show other
