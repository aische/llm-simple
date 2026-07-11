{-# OPTIONS_GHC -Wno-missing-fields #-}

module LLM.HistoryToolSpec (spec) where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Heptapod (generate)
import LLM.Agent.ToolUtils (toTool, windowOffset)
import LLM.Agent.Tools.HistoryTool (historyToolTyped)
import LLM.Agent.Types (Agent (..), RuntimeArgs (..), Tool (..), ToolContext (..))
import LLM.Core.Types (Turn (..))
import LLM.Generate.GenerateUtils (llmHooks)
import LLM.Generate.Logger (noHooks)
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
    shouldContain,
    shouldNotContain,
  )

spec :: Spec
spec = describe "HistoryTool" $ do
  describe "windowOffset" $ do
    it "returns 0 when no context window is configured" $ do
      windowOffset Nothing sampleConversation `shouldBe` 0

    it "hides all but the last N user messages" $ do
      windowOffset (Just 1) sampleConversation `shouldBe` 4

    it "returns 0 when the conversation has fewer user messages than N" $ do
      windowOffset (Just 5) [UserTurn "only"] `shouldBe` 0

  describe "get_history" $ do
    it "reports no earlier history when nothing is hidden" $ do
      result <- runHistory noWindowAgent sampleConversation 0
      result `shouldBe` "(no earlier history)"

    it "returns the most recent hidden chunk at chunk 0" $ do
      result <- runHistory windowedAgent sampleConversation 0
      T.unpack result `shouldContain` "[User] second question"
      T.unpack result `shouldNotContain` "first question"

    it "returns older hidden chunks at higher indices" $ do
      result <- runHistory windowedAgent sampleConversation 1
      T.unpack result `shouldContain` "[User] first question"
      T.unpack result `shouldNotContain` "second question"

    it "reports no more history for out-of-range chunk indices" $ do
      result <- runHistory windowedAgent sampleConversation 9
      result `shouldBe` "(no more history)"

sampleConversation :: [Turn]
sampleConversation =
  [ UserTurn "first question",
    AssistantTurn "first answer" Nothing [],
    UserTurn "second question",
    AssistantTurn "second answer" Nothing [],
    UserTurn "third question",
    AssistantTurn "third answer" Nothing []
  ]

noWindowAgent :: Agent
noWindowAgent =
  Agent
    { agName = "test",
      agSystemPrompt = Nothing,
      agTools = ["get_history"],
      agMaxToolRounds = 3,
      agContextWindow = Nothing
    }

windowedAgent :: Agent
windowedAgent = noWindowAgent {agContextWindow = Just 1}

runHistory :: Agent -> [Turn] -> Int -> IO Text
runHistory agent conv chunk = do
  genId <- generate
  let offset = windowOffset agent.agContextWindow conv
      ctx =
        ToolContext
          { tcConversation = conv,
            tcUsage = mempty,
            tcWindowOffset = offset,
            tcRuntimeArgs =
              RuntimeArgs
                { rtGenerationId = genId,
                  rtAbortSignal = Nothing,
                  rtLLMHooks = llmHooks noHooks,
                  rtHooks = noHooks,
                  rtOnEvent = \_ -> pure (),
                  rtReadonly = False
                }
          }
      tool = toTool historyToolTyped
  tool.toolExecute ctx (object ["chunk" .= chunk])
