{-# OPTIONS_GHC -Wno-missing-fields #-}

module LLM.StreamingSpec (spec) where

import Data.Aeson (object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import LLM.Core.Types
  ( ChatResponse (..),
    ContentBlock (TextBlock, ToolCallBlock),
    LLMGateway (..),
    StreamEvent (..),
    Turn (..),
    mkToolCall,
  )
import LLM.Core.Usage (PricingInfo (..), Usage (..))
import LLM.Generate.Generate (streamTextLLM)
import LLM.Generate.GenerateUtils (llmHooks)
import LLM.Generate.Logger (noHooks)
import LLM.Generate.ModelConfig (ModelConfig (..))
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
              (ChatResponse "hello" [TextBlock "hello"] (Just (Usage 1 1 0)) Nothing)
      chunks <- runStream gw []
      reverse chunks
        `shouldBe` [ TextDelta "hello",
                     RoundTextRoleCommitted AnswerRole
                   ]

    it "routes text to answer after a tool turn" $ do
      let prior = [UserTurn "q", AssistantTurn "" Nothing [mkToolCall "1" "t" (object [])], ToolTurn []]
          gw =
            streamGateway
              [StreamDelta "done"]
              (ChatResponse "done" [TextBlock "done"] (Just (Usage 1 1 0)) Nothing)
      chunks <- runStream gw prior
      reverse chunks `shouldBe` [AnswerDelta "done"]

    it "routes preamble text and tool calls before answer" $ do
      let tc = mkToolCall "c1" "grep" (object ["pattern" .= ("x" :: Text)])
          gw =
            streamGateway
              [StreamDelta "searching", StreamToolCall tc]
              (ChatResponse "" [ToolCallBlock tc] (Just (Usage 2 0 0)) Nothing)
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
              (ChatResponse "ok" [TextBlock "ok"] Nothing Nothing)
      chunks <- runStream gw []
      reverse chunks `shouldBe` [ReasoningDelta "think", RoundTextRoleCommitted AnswerRole]

streamGateway :: [StreamEvent] -> ChatResponse -> LLMGateway
streamGateway events resp =
  LLMGateway
    { gwName = "stream-mock",
      gwGenerateText = \_ _ -> pure (Right resp),
      gwStreamText = \_ _ onEvent -> mapM_ onEvent events >> pure (Right resp),
      gwGenerateObject = \_ _ _ -> pure (Right (object [], Nothing))
    }

runStream :: LLMGateway -> [Turn] -> IO [StreamChunk]
runStream gw turns = do
  ref <- newIORef []
  let onChunk c = modifyIORef' ref (c :)
      gr =
        GenRequest
          { grSystemPrompt = Nothing,
            grMessages = turns,
            grTools = [],
            grAbortSignal = Nothing,
            grLLMHooks = llmHooks noHooks,
            grHooks = noHooks
          }
      mc =
        ModelConfig
          { mcGateway = gw,
            mcModel = "test",
            mcPricing = zeroPricing,
            mcMaxTokens = 256,
            mcTemperature = Nothing,
            mcThinking = Nothing,
            mcRequestTimeout = Nothing,
            mcThrottleDelay = Nothing,
            mcRetryCount = 0,
            mcJitterBackoff = 0
          }
  _ <- streamTextLLM onChunk gr mc
  readIORef ref

zeroPricing :: PricingInfo
zeroPricing = PricingInfo 0 0
