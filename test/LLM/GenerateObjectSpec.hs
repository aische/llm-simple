{-# OPTIONS_GHC -Wno-missing-fields #-}

module LLM.GenerateObjectSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Heptapod (generate)
import LLM.Agent.GenerateObject (generateObject, generateObjectUntyped)
import LLM.Agent.Types (Agent (..), RuntimeArgs (..))
import LLM.Core.Abort (AbortSignal, abort, newAbortSignal)
import LLM.Core.Types (ChatResponse (..), LLMError (..), LLMGateway (..), LLMHooks (..), Turn (..))
import LLM.Core.Usage (PricingInfo (..), Usage (..))
import LLM.Generate.Logger (noHooks)
import LLM.Generate.ModelConfig
  ( ModelConfig (..),
    ModelWithFallbacks (..),
  )
import LLM.Generate.Types (GenerateError (..), GenerateErrorResult (..))
import LLM.WeatherTool (WeatherToolArgs (..))
import Test.Hspec
  ( Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
    shouldContain,
  )

spec :: Spec
spec = describe "GenerateObject" $ do
  describe "generateObjectUntyped" $ do
    it "returns the provider JSON and usage" $ do
      let gw = objectGateway (object ["location" .= ("Paris" :: Text)]) (Usage 12 3 0)
          models = ModelWithFallbacks (mockModel gw) []
      result <- runUntyped models (object ["type" .= ("object" :: Text)])
      case result of
        Right (value, usage) -> do
          value `shouldBe` object ["location" .= ("Paris" :: Text)]
          usage `shouldBe` Usage 12 3 0
        Left err -> expectationFailure $ show err

    it "propagates provider errors" $ do
      let gw = errorObjectGateway (HttpError 503 "unavailable")
          models = ModelWithFallbacks (mockModel gw) []
      result <- runUntyped models (object ["type" .= ("object" :: Text)])
      case result of
        Left GenerateErrorResult {gerError = GErrLLM (HttpError 503 _)} -> pure ()
        other -> expectationFailure $ "expected HttpError 503, got: " <> show other

    it "returns Aborted when the signal is already set" $ do
      sig <- newAbortSignal
      abort sig
      let gw = objectGateway (object []) (Usage 0 0 0)
          models = ModelWithFallbacks (mockModel gw) []
      rt <- mkRuntime (Just sig)
      result <- generateObjectUntyped defaultAgent models rt [UserTurn "go"] (object ["type" .= ("object" :: Text)])
      case result of
        Left GenerateErrorResult {gerError = GErrAborted} -> pure ()
        other -> expectationFailure $ "expected GErrAborted, got: " <> show other

    it "falls back to the next model on failure" $ do
      let failGw = errorObjectGateway (HttpError 500 "boom")
          okGw = objectGateway (object ["ok" .= True]) (Usage 1 1 0)
          models = ModelWithFallbacks (mockModel failGw) [mockModel okGw]
      result <- runUntyped models (object ["type" .= ("object" :: Text)])
      case result of
        Right (value, _) -> value `shouldBe` object ["ok" .= True]
        Left err -> expectationFailure $ show err

  describe "generateObject" $ do
    it "decodes a typed object from provider JSON" $ do
      let gw = objectGateway (object ["location" .= ("London" :: Text)]) (Usage 4 2 0)
          models = ModelWithFallbacks (mockModel gw) []
      result <- runTyped models
      case result of
        Right (WeatherToolArgs loc, usage) -> do
          loc `shouldBe` "London"
          usage `shouldBe` Usage 4 2 0
        Left err -> expectationFailure $ show err

    it "reports a parse error when provider JSON does not match the codec" $ do
      let gw = objectGateway (object ["wrong" .= (1 :: Int)]) (Usage 0 0 0)
          models = ModelWithFallbacks (mockModel gw) []
      result <- runTyped models
      case result of
        Left GenerateErrorResult {gerError = GErrParseObjectError msg} ->
          T.unpack msg `shouldContain` "Can't decode object"
        _ -> expectationFailure "expected GErrParseObjectError"

objectGateway :: Value -> Usage -> LLMGateway
objectGateway value usage =
  LLMGateway
    { gwName = "object-mock",
      gwGenerateText = \_ _ -> pure (Right (ChatResponse "" [] Nothing Nothing)),
      gwStreamText = \_ _ _ -> pure (Right (ChatResponse "" [] Nothing Nothing)),
      gwGenerateObject = \_ _ _ -> pure (Right (value, Just usage))
    }

errorObjectGateway :: LLMError -> LLMGateway
errorObjectGateway err =
  LLMGateway
    { gwName = "object-error",
      gwGenerateText = \_ _ -> pure (Left err),
      gwStreamText = \_ _ _ -> pure (Left err),
      gwGenerateObject = \_ _ _ -> pure (Left err)
    }

mockModel :: LLMGateway -> ModelConfig
mockModel gw =
  ModelConfig
    { mcGateway = gw,
      mcModel = "test-model",
      mcPricing = zeroPricing,
      mcMaxTokens = 256,
      mcTemperature = Nothing,
      mcThinking = Nothing,
      mcRequestTimeout = Nothing,
      mcThrottleDelay = Nothing,
      mcRetryCount = 0,
      mcJitterBackoff = 0
    }

zeroPricing :: PricingInfo
zeroPricing = PricingInfo 0 0

defaultAgent :: Agent
defaultAgent =
  Agent
    { agName = "test",
      agSystemPrompt = Nothing,
      agTools = [],
      agMaxToolRounds = 3,
      agContextWindow = Nothing
    }

mkRuntime :: Maybe AbortSignal -> IO RuntimeArgs
mkRuntime mSig = do
  genId <- generate
  pure
    RuntimeArgs
      { rtGenerationId = genId,
        rtAbortSignal = mSig,
        rtLLMHooks =
          LLMHooks
            { onLLMRequest = \_ _ -> pure (),
              onLLMResponse = \_ _ -> pure (),
              onLLMResponseError = \_ _ -> pure ()
            },
        rtHooks = noHooks,
        rtOnEvent = \_ -> pure (),
        rtReadonly = False
      }

runUntyped :: ModelWithFallbacks -> Value -> IO (Either GenerateErrorResult (Value, Usage))
runUntyped models schema = do
  rt <- mkRuntime Nothing
  generateObjectUntyped defaultAgent models rt [UserTurn "extract"] schema

runTyped :: ModelWithFallbacks -> IO (Either GenerateErrorResult (WeatherToolArgs, Usage))
runTyped models = do
  rt <- mkRuntime Nothing
  generateObject defaultAgent models rt [UserTurn "weather?"]
