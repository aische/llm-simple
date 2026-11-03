{-# OPTIONS_GHC -Wno-missing-fields #-}

module LLM.HistoryToolSpec (spec) where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Heptapod (generate)
import LLM.Agent.ToolUtils (toTool, windowOffset)
import LLM.Agent.Tools.HistoryTool (historyToolTyped)
import LLM.Agent.Types (Agent (..), RuntimeArgs (..), Tool (..), ToolContext (..))
import LLM.Core.Types (Turn (..), assistantTurn, pattern UserTurn)
import LLM.Generate.GenerateUtils (llmHooks)
import LLM.Generate.Logger (noHooks)
import LLM.Tools.FsLimits (maxOutputBytes)
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
    shouldContain,
    shouldNotContain,
    shouldSatisfy,
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

    it "returns 0 for a non-positive window (no windowing)" $ do
      windowOffset (Just 0) sampleConversation `shouldBe` 0
      windowOffset (Just (-1)) sampleConversation `shouldBe` 0

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

    -- When the visible window has zero UserTurns, page size is 0. Previously
    -- chunkBackward looped forever; it now returns the whole hidden prefix as
    -- a single chunk.
    it "returns the full hidden prefix when the visible window has no user turns" $ do
      let conv =
            [ UserTurn "hidden question",
              assistantTurn "hidden answer" Nothing [],
              assistantTurn "visible assistant only" Nothing []
            ]
          -- Offset past both user+assistant hidden turns; visible slice is
          -- assistant-only so countUserTurns == 0.
          offset = 2
      result <- runHistoryAtOffset offset conv 0
      T.unpack result `shouldContain` "[User] hidden question"
      T.unpack result `shouldContain` "[Assistant] hidden answer"

  describe "get_history page size" $ do
    it "leaves out the oldest turns of a page over maxOutputBytes and says how many" $ do
      let answer i = assistantTurn ("answer-" <> T.pack (show i) <> " " <> T.replicate 5000 "x") Nothing []
          conv =
            [UserTurn "old question"]
              ++ map answer [0 .. 39 :: Int]
              ++ [UserTurn "visible question"]
      result <- runHistory windowedAgent conv 0
      utf8Size result `shouldSatisfy` (<= maxOutputBytes)
      T.unpack result `shouldContain` "answer-39 "
      T.unpack result `shouldNotContain` "answer-0 "
      T.unpack result `shouldNotContain` "old question"
      let keptAnswers = length (filter ("[Assistant] answer-" `T.isPrefixOf`) (T.lines result))
      last (T.lines result)
        `shouldBe` "["
        <> T.pack (show (41 - keptAnswers))
        <> " earlier turn(s) of this page left out: the page is over "
        <> T.pack (show maxOutputBytes)
        <> " bytes]"

    it "cuts the newest turn when it alone is over maxOutputBytes" $ do
      let conv =
            [ UserTurn "old question",
              assistantTurn ("huge " <> T.replicate 200000 "y") Nothing [],
              UserTurn "visible question"
            ]
      result <- runHistory windowedAgent conv 0
      utf8Size result `shouldSatisfy` (<= maxOutputBytes)
      T.unpack result `shouldContain` "[Assistant] huge yyy"
      T.unpack result `shouldContain` "[turn cut: "
      T.unpack result `shouldNotContain` "old question"
      T.unpack result `shouldContain` "[1 earlier turn(s) of this page left out"

sampleConversation :: [Turn]
sampleConversation =
  [ UserTurn "first question",
    assistantTurn "first answer" Nothing [],
    UserTurn "second question",
    assistantTurn "second answer" Nothing [],
    UserTurn "third question",
    assistantTurn "third answer" Nothing []
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
runHistory agent conv = runHistoryAtOffset (windowOffset agent.agContextWindow conv) conv

runHistoryAtOffset :: Int -> [Turn] -> Int -> IO Text
runHistoryAtOffset offset conv chunk = do
  genId <- generate
  let ctx =
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

utf8Size :: Text -> Int
utf8Size = BS.length . encodeUtf8
