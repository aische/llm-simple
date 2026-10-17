{-# OPTIONS_GHC -Wno-missing-fields #-}

module LLM.StreamingSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Retry (RetryPolicyM (getRetryPolicyM), RetryStatus (rsIterNumber), defaultRetryStatus)
import Data.Aeson (object, (.=))
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import LLM.Core.Types
  ( ChatResponse (..),
    LLMError (..),
    LLMGateway (..),
    supportedImageMediaTypes,
    StreamEvent (..),
    Turn (..),
    assistantTurn,
    mkToolCall,
    pattern UserTurn,
    textPart,
    toolCallPart,
  )
import LLM.Core.Usage (PricingInfo (..), defaultPricingInfo, mkUsage)
import LLM.Generate.Generate (generateTextLLM, streamTextLLM, streamTextWithFallbacks)
import LLM.Generate.GenerateUtils (llmHooks)
import LLM.Generate.Logger (noHooks)
import LLM.Generate.ModelConfig (ModelConfig (..), ModelWithFallbacks (..), defaultModelCapabilities, modelRetryPolicy)
import LLM.Generate.Types
  ( GenRequest (..),
    RoundTextRole (..),
    StreamChunk (..),
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Streaming" $ do
  describe "streamTextLLM phase routing" $ do
    it "classifies plain text as answer on finalize" $ do
      let gw =
            streamGateway
              [StreamDelta "hello"]
              (ChatResponse "hello" [textPart "hello"] (Just (mkUsage 1 1)) Nothing)
      chunks <- runStream gw []
      reverse chunks
        `shouldBe` [ TextDelta "hello",
                     RoundTextRoleCommitted AnswerRole
                   ]

    it "routes text to answer after a tool turn" $ do
      let prior = [UserTurn "q", assistantTurn "" Nothing [mkToolCall "1" "t" (object [])], ToolTurn []]
          gw =
            streamGateway
              [StreamDelta "done"]
              (ChatResponse "done" [textPart "done"] (Just (mkUsage 1 1)) Nothing)
      chunks <- runStream gw prior
      reverse chunks `shouldBe` [AnswerDelta "done"]

    it "routes preamble text and tool calls before answer" $ do
      let tc = mkToolCall "c1" "grep" (object ["pattern" .= ("x" :: Text)])
          gw =
            streamGateway
              [StreamDelta "searching", StreamToolCall tc]
              (ChatResponse "" [toolCallPart tc] (Just (mkUsage 2 0)) Nothing)
      chunks <- runStream gw [UserTurn "find x"]
      reverse chunks
        `shouldBe` [ TextDelta "searching",
                     RoundTextRoleCommitted PreambleRole,
                     StreamToolCallChunk tc
                   ]

    it "forwards reasoning deltas unchanged" $ do
      let gw =
            streamGateway
              [StreamReasoningDelta "think"]
              (ChatResponse "ok" [textPart "ok"] Nothing Nothing)
      chunks <- runStream gw []
      reverse chunks `shouldBe` [ReasoningDelta "think", RoundTextRoleCommitted AnswerRole]

  describe "request timeout" $ do
    it "applies the timeout per attempt, so a retry can succeed" $ do
      attempts <- newIORef (0 :: Int)
      let resp = ChatResponse "ok" [textPart "ok"] Nothing Nothing
          gw =
            (streamGateway [] resp)
              { gwGenerateText = \_ _ -> do
                  n <- atomicModifyIORef' attempts (\k -> (k + 1, k))
                  when (n == 0) $ threadDelay 1_000_000
                  pure (Right resp)
              }
          mc = (testModelConfig gw) {mcRequestTimeout = Just 100, mcRetryCount = 1}
      r <- generateTextLLM (testGenRequest []) mc
      fmap (.respText) r `shouldBe` Right "ok"
      readIORef attempts >>= (`shouldBe` 2)

    it "streaming: does not cut off a stream that started within the timeout" $ do
      let resp = ChatResponse "abc" [textPart "abc"] Nothing Nothing
          gw =
            (streamGateway [] resp)
              { gwStreamText = \_ _ onEvent -> do
                  onEvent (StreamDelta "a")
                  threadDelay 150_000
                  onEvent (StreamDelta "b")
                  threadDelay 150_000
                  onEvent (StreamDelta "c")
                  pure (Right resp)
              }
          mc = (testModelConfig gw) {mcRequestTimeout = Just 100}
      ref <- newIORef []
      r <- streamTextLLM (\c -> modifyIORef' ref (c :)) (testGenRequest []) mc
      fmap (.respText) r `shouldBe` Right "abc"
      chunks <- reverse <$> readIORef ref
      take 3 chunks `shouldBe` [TextDelta "a", TextDelta "b", TextDelta "c"]

    it "streaming: times out when no event arrives in time" $ do
      let resp = ChatResponse "late" [textPart "late"] Nothing Nothing
          gw =
            (streamGateway [] resp)
              { gwStreamText = \_ _ onEvent -> do
                  threadDelay 1_000_000
                  onEvent (StreamDelta "late")
                  pure (Right resp)
              }
          mc = (testModelConfig gw) {mcRequestTimeout = Just 100}
      r <- streamTextLLM (\_ -> pure ()) (testGenRequest []) mc
      fmap (.respText) r `shouldBe` Left TimeoutError

  describe "modelRetryPolicy" $ do
    it "caps the backoff delay at 30 s" $ do
      let mc =
            (testModelConfig (streamGateway [] (ChatResponse "" [] Nothing Nothing)))
              { mcRetryCount = 20,
                mcJitterBackoff = 1000
              }
      delays <-
        mapM
          (\n -> getRetryPolicyM (modelRetryPolicy mc) defaultRetryStatus {rsIterNumber = n})
          [10 .. 19]
      all (maybe False (<= 30_000_000)) delays `shouldBe` True

  describe "stream reset between attempts" $ do
    it "emits StreamAttemptReset before a retry when the failed attempt streamed" $ do
      gw <- flakyGateway "abc" "xyz"
      let mc = (testModelConfig gw) {mcRetryCount = 1}
      ref <- newIORef []
      r <- streamTextLLM (\c -> modifyIORef' ref (c :)) (testGenRequest []) mc
      fmap (.respText) r `shouldBe` Right "xyz"
      chunks <- reverse <$> readIORef ref
      chunks
        `shouldBe` [ TextDelta "abc",
                     StreamAttemptReset,
                     TextDelta "xyz",
                     RoundTextRoleCommitted AnswerRole
                   ]

    it "does not emit StreamAttemptReset when the failed attempt streamed nothing" $ do
      gw <- flakyGateway "" "xyz"
      let mc = (testModelConfig gw) {mcRetryCount = 1}
      ref <- newIORef []
      _ <- streamTextLLM (\c -> modifyIORef' ref (c :)) (testGenRequest []) mc
      chunks <- reverse <$> readIORef ref
      chunks `shouldBe` [TextDelta "xyz", RoundTextRoleCommitted AnswerRole]

    it "emits StreamAttemptReset before falling back to the next model" $ do
      let failing =
            (streamGateway [] (ChatResponse "" [] Nothing Nothing))
              { gwStreamText = \_ _ onEvent -> do
                  onEvent (StreamDelta "abc")
                  pure (Left (HttpError 400 "bad"))
              }
          ok = streamGateway [StreamDelta "xyz"] (ChatResponse "xyz" [textPart "xyz"] Nothing Nothing)
          models = ModelWithFallbacks (testModelConfig failing) [testModelConfig ok]
      ref <- newIORef []
      r <- streamTextWithFallbacks (\c -> modifyIORef' ref (c :)) (testGenRequest []) models
      fmap (.respText) r `shouldBe` Right "xyz"
      chunks <- reverse <$> readIORef ref
      chunks
        `shouldBe` [ TextDelta "abc",
                     StreamAttemptReset,
                     TextDelta "xyz",
                     RoundTextRoleCommitted AnswerRole
                   ]

-- | First call streams @first@ (if non-empty) then fails with a retryable
-- 503; later calls stream @second@ and succeed.
flakyGateway :: Text -> Text -> IO LLMGateway
flakyGateway first second = do
  attempts <- newIORef (0 :: Int)
  let resp = ChatResponse second [textPart second] Nothing Nothing
  pure
    (streamGateway [] resp)
      { gwStreamText = \_ _ onEvent -> do
          n <- atomicModifyIORef' attempts (\k -> (k + 1, k))
          if n == 0
            then do
              when (first /= "") $ onEvent (StreamDelta first)
              pure (Left (HttpError 503 "overloaded"))
            else do
              onEvent (StreamDelta second)
              pure (Right resp)
      }

streamGateway :: [StreamEvent] -> ChatResponse -> LLMGateway
streamGateway events resp =
  LLMGateway
    { gwName = "stream-mock",
      gwImageMediaTypes = supportedImageMediaTypes,
      gwImageUrlSupport = True,
      gwGenerateText = \_ _ -> pure (Right resp),
      gwStreamText = \_ _ onEvent -> mapM_ onEvent events >> pure (Right resp),
      gwGenerateObject = \_ _ _ -> pure (Right (object [], Nothing))
    }

runStream :: LLMGateway -> [Turn] -> IO [StreamChunk]
runStream gw turns = do
  ref <- newIORef []
  let onChunk c = modifyIORef' ref (c :)
  _ <- streamTextLLM onChunk (testGenRequest turns) (testModelConfig gw)
  readIORef ref

testGenRequest :: [Turn] -> GenRequest
testGenRequest turns =
  GenRequest
    { grSystemPrompt = Nothing,
      grMessages = turns,
      grTools = [],
      grAbortSignal = Nothing,
      grLLMHooks = llmHooks noHooks,
      grHooks = noHooks
    }

testModelConfig :: LLMGateway -> ModelConfig
testModelConfig gw =
  ModelConfig
    { mcGateway = gw,
      mcModel = "test",
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
