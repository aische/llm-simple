{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module LLM.GenerateObjectSpec (spec) where

import Data.Aeson (Value (Null), object, (.=))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Heptapod (generate)
import LLM.Agent.GenerateObject (generateObject, generateObjectUntyped)
import LLM.Agent.Types (Agent (..), RuntimeArgs (..))
import LLM.Core.Abort (AbortSignal, abort, newAbortSignal)
import LLM.Core.Types (ChatRequest (..), ChatResponse (..), LLMError (..), LLMGateway (..), LLMHooks (..), assistantTurn, supportedImageMediaTypes, pattern UserTurn)
import LLM.Core.Usage (PricingInfo (..), Usage (..), defaultPricingInfo, mkUsage)
import LLM.Generate.Logger (noHooks)
import LLM.Generate.ModelConfig
  ( ModelConfig (..),
    ModelWithFallbacks (..),
    defaultModelCapabilities,
  )
import LLM.Generate.Types (GenerateError (..), GenerateErrorResult (..), GeneratableObject)
import LLM.OptionalFields (Inner (..), Node (..), Optionals (..), innerDefaults)
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
      let gw = objectGateway (object ["location" .= ("Paris" :: Text)]) (mkUsage 12 3)
          models = ModelWithFallbacks (mockModel gw) []
      result <- runUntyped models (object ["type" .= ("object" :: Text)])
      case result of
        Right (value, usage) -> do
          value `shouldBe` object ["location" .= ("Paris" :: Text)]
          usage `shouldBe` mkUsage 12 3
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
      let gw = objectGateway (object []) (mkUsage 0 0)
          models = ModelWithFallbacks (mockModel gw) []
      rt <- mkRuntime (Just sig)
      result <- generateObjectUntyped defaultAgent models rt [UserTurn "go"] (object ["type" .= ("object" :: Text)])
      case result of
        Left GenerateErrorResult {gerError = GErrAborted} -> pure ()
        other -> expectationFailure $ "expected GErrAborted, got: " <> show other

    it "falls back to the next model on failure" $ do
      let failGw = errorObjectGateway (HttpError 500 "boom")
          okGw = objectGateway (object ["ok" .= True]) (mkUsage 1 1)
          models = ModelWithFallbacks (mockModel failGw) [mockModel okGw]
      result <- runUntyped models (object ["type" .= ("object" :: Text)])
      case result of
        Right (value, _) -> value `shouldBe` object ["ok" .= True]
        Left err -> expectationFailure $ show err

  describe "generateObject" $ do
    it "decodes a typed object from provider JSON" $ do
      let gw = objectGateway (object ["location" .= ("London" :: Text)]) (mkUsage 4 2)
          models = ModelWithFallbacks (mockModel gw) []
      result <- runTyped models
      case result of
        Right (WeatherToolArgs loc, usage) -> do
          loc `shouldBe` "London"
          usage `shouldBe` mkUsage 4 2
        Left err -> expectationFailure $ show err

    it "reports a parse error when provider JSON does not match the codec" $ do
      let gw = objectGateway (object ["wrong" .= (1 :: Int)]) (mkUsage 0 0)
          models = ModelWithFallbacks (mockModel gw) []
      result <- runTyped models
      case result of
        Left GenerateErrorResult {gerError = GErrParseObjectError msg} ->
          T.unpack msg `shouldContain` "Can't decode object"
        _ -> expectationFailure "expected GErrParseObjectError"

    it "does not advertise tools even when agContextWindow would inject get_history" $ do
      captured <- newIORef Nothing
      let gw = capturingObjectGateway captured (object ["location" .= ("Paris" :: Text)]) (mkUsage 1 0)
          models = ModelWithFallbacks (mockModel gw) []
          agent = defaultAgent {agContextWindow = Just 1}
          conv =
            [ UserTurn "first",
              assistantTurn "a1" Nothing [],
              UserTurn "second"
            ]
      rt <- mkRuntime Nothing
      result <- generateObjectUntyped agent models rt conv (object ["type" .= ("object" :: Text)])
      case result of
        Right _ -> do
          mReq <- readIORef captured
          case mReq of
            Nothing -> expectationFailure "expected gwGenerateObject to be called"
            Just req -> req.reqTools `shouldBe` []
        Left err -> expectationFailure $ show err

  describe "generateObject with optional fields answered null (strict mode)" $ do
    let nullInner = object ["a" .= Null, "b" .= Null]
        answer =
          object
            [ "req_maybe" .= Null,
              "or_null" .= Null,
              "opt" .= Null,
              "def" .= Null,
              "omit" .= Null,
              "nested" .= nullInner,
              "named" .= nullInner,
              "list" .= [nullInner, object ["a" .= ("x" :: Text), "b" .= (7 :: Int)]],
              "union" .= object ["Left" .= nullInner],
              "plain" .= ("p" :: Text)
            ]

    it "reads null as absent for every optional field, in nested objects, arrays, unions and $refs" $ do
      result <- runTypedWith (objectGateway answer (mkUsage 1 1))
      case result of
        Right (v, _) ->
          v
            `shouldBe` Optionals
              { reqMaybe = Nothing,
                orNull = Nothing,
                opt = Nothing,
                withDefault = 5,
                omitted = 5,
                nested = Just innerDefaults,
                named = Just innerDefaults,
                list = [innerDefaults, Inner (Just "x") 7],
                union = Left innerDefaults,
                plain = "p"
              }
        Left err -> expectationFailure $ show err

    it "reads a null optional object as absent" $ do
      let answer' = object ["req_maybe" .= Null, "nested" .= Null, "named" .= Null, "list" .= ([] :: [Value]), "union" .= object ["Right" .= (1 :: Int)], "plain" .= ("p" :: Text)]
      result <- runTypedWith @Optionals (objectGateway answer' (mkUsage 1 1))
      case result of
        Right (v, _) -> (v.nested, v.named, v.union) `shouldBe` (Nothing, Nothing, Right 1)
        Left err -> expectationFailure $ show err

    it "still reports null for a required field that is not nullable" $ do
      let answer' = object ["req_maybe" .= Null, "list" .= ([] :: [Value]), "union" .= object ["Right" .= (1 :: Int)], "plain" .= Null]
      result <- runTypedWith @Optionals (objectGateway answer' (mkUsage 1 1))
      case result of
        Left GenerateErrorResult {gerError = GErrParseObjectError _} -> pure ()
        other -> expectationFailure $ "expected GErrParseObjectError, got: " <> show (fmap fst other)

    it "reads null as absent in a recursive type" $ do
      let answer' = object ["label" .= ("a" :: Text), "next" .= object ["label" .= ("b" :: Text), "next" .= Null]]
      result <- runTypedWith (objectGateway answer' (mkUsage 1 1))
      case result of
        Right (v, _) -> v `shouldBe` Node "a" (Just (Node "b" Nothing))
        Left err -> expectationFailure $ show err

objectGateway :: Value -> Usage -> LLMGateway
objectGateway value usage =
  LLMGateway
    { gwName = "object-mock",
      gwImageMediaTypes = supportedImageMediaTypes,
      gwImageUrlSupport = True,
      gwGenerateText = \_ _ -> pure (Right (ChatResponse "" [] Nothing Nothing)),
      gwStreamText = \_ _ _ -> pure (Right (ChatResponse "" [] Nothing Nothing)),
      gwGenerateObject = \_ _ _ -> pure (Right (value, Just usage))
    }

-- | Mock that records the 'ChatRequest' seen by 'gwGenerateObject'.
capturingObjectGateway :: IORef (Maybe ChatRequest) -> Value -> Usage -> LLMGateway
capturingObjectGateway ref value usage =
  (objectGateway value usage)
    { gwGenerateObject = \_ _ req -> do
        writeIORef ref (Just req)
        pure (Right (value, Just usage))
    }

errorObjectGateway :: LLMError -> LLMGateway
errorObjectGateway err =
  LLMGateway
    { gwName = "object-error",
      gwImageMediaTypes = supportedImageMediaTypes,
      gwImageUrlSupport = True,
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
      mcCapabilities = defaultModelCapabilities,
      mcRequestTimeout = Nothing,
      mcThrottleDelay = Nothing,
      mcRetryCount = 0,
      mcJitterBackoff = 0
    }

zeroPricing :: PricingInfo
zeroPricing = defaultPricingInfo 0 0

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

runTypedWith :: (GeneratableObject t) => LLMGateway -> IO (Either GenerateErrorResult (t, Usage))
runTypedWith gw = do
  rt <- mkRuntime Nothing
  generateObject defaultAgent (ModelWithFallbacks (mockModel gw) []) rt [UserTurn "extract"]
