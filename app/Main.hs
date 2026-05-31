{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Autodocodec qualified as AC
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException (SomeException), catch)
import Data.Aeson (FromJSON)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import Heptapod (generate)
import LLM (AbortSignal, Hooks (..), ThinkingMode (..), claudeGateway, deepSeekGateway, defaultDebugHooks, mkFsConfig)
import LLM.Agent.Types (ToolContext (..))
import LLM.Core.Types (LLMHooks (..), ToolDef (..), Turn (UserTurn))
import LLM.Core.Usage (PricingInfo (..), Usage)
import LLM.Generate.Logger (noHooks)
import LLM.Generate.ModelConfig (ModelConfig (..), ModelWithFallbacks (..))
import LLM.Generate.Types
  ( GenerateErrorResult (..),
    GenerateTextResult (..),
    StreamChunk (..),
  )
import LLM.Load.LoadModels (loadModelsOrThrow)
import LLM.Workflow.ToolUtils (toTool, workflowToolTyped)
import LLM.Workflow.Tools.FsTools (fsTools)
import LLM.Workflow.Types
  ( Agent (..),
    AgentWithModels (..),
    CID (CID),
    FinalResult (..),
    GenerateEvent (..),
    Kont,
    MergePolicy (MergePolicy),
    Prompt (..),
    PromptArgs (..),
    PromptState (PromptStateFinal, PromptStatePending),
    RuntimeArgs (..),
    Step (RunPrompt, RunWorkflow),
    Tool (..),
    ToolMap,
    TranscriptPolicy (TranscriptPolicy, TranscriptSummaryOnly),
    TypedWorkflowTool,
    Workflow (..),
  )
import LLM.Workflow.Workflow
  ( eval,
    loop,
  )
import System.Environment (getArgs, getEnv)

-- | Orchestrator: no legacy tools, only UTools (subagent + workflow).
createOrchestratorAgent :: Agent
createOrchestratorAgent =
  Agent
    { agName = "orchestrator",
      agSystemPrompt =
        Just
          "You are a helpful assistant. You may delegate work using tools:\n\
          \- subagent: filesystem-capable child agent for a single task\n\
          \- run_workflow: parallel workflow (plan+execute sequence and a review branch)",
      agTools = [],
      agUTools = ["subagent", "run_workflow"],
      agMaxToolRounds = 5,
      agContextWindow = Nothing
    }

-- | Worker agents used inside subagents / workflow nodes (legacy filesystem tools).
worker1 :: Agent
worker1 =
  Agent
    { agName = "worker",
      agSystemPrompt = Just "You are a helpful assistant with filesystem access.",
      agTools =
        [ "readfile",
          "writefile",
          "readdir",
          "directory_tree"
        ],
      agUTools = [],
      agMaxToolRounds = 6,
      agContextWindow = Nothing
    }

worker2 :: Agent
worker2 =
  Agent
    { agName = "worker2",
      agSystemPrompt = Just "You are a friedly assistant who helps the user with their tasks and questions.",
      agTools = ["subagent"],
      agUTools = [],
      agMaxToolRounds = 6,
      agContextWindow = Nothing
    }

expert :: Agent
expert =
  Agent
    { agName = "expert",
      agSystemPrompt =
        Just
          "You are an expert in the user's domain. \
          \Answer the user's question, but keep it short and concise.",
      agTools = [],
      agUTools = [],
      agMaxToolRounds = 6,
      agContextWindow = Nothing
    }

student :: Agent
student =
  Agent
    { agName = "student",
      agSystemPrompt =
        Just
          "You are a student learning from an expert. \
          \Start a conversation with the expert and ask questions to learn more about the topic.",
      agTools = [],
      agUTools = [],
      agMaxToolRounds = 6,
      agContextWindow = Nothing
    }

summarizer :: Agent
summarizer =
  Agent
    { agName = "summarizer",
      agSystemPrompt =
        Just
          "You are a summarizer. \
          \Summarize the conversation between the expert and the student.",
      agTools = [],
      agUTools = [],
      agMaxToolRounds = 6,
      agContextWindow = Nothing
    }

newtype SubagentArgs = SubagentArgs
  { prompt :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec SubagentArgs)

instance AC.HasCodec SubagentArgs where
  codec :: AC.JSONCodec SubagentArgs
  codec =
    AC.object "precise prompt for the subagent" $
      SubagentArgs <$> AC.requiredField "prompt" "a precise prompt for the subagent" AC..= (\x -> x.prompt)

subagent :: Text -> Text -> (SubagentArgs -> ctx -> (Workflow, PromptArgs)) -> TypedWorkflowTool ctx SubagentArgs
subagent = workflowToolTyped @SubagentArgs

main :: IO ()
main = do
  (gpt, llama, haiku, gemini) <-
    loadModelsOrThrow
      "./model-catalog.json"
      ("gpt_4_1", "llama_3_2", "haiku_4_5", "gemini_2_5_flash")

  let _models1 = ModelWithFallbacks {mwfModel = gpt, mwfFallbacks = [gemini, haiku, llama]}
      _models2 = ModelWithFallbacks {mwfModel = haiku, mwfFallbacks = [gemini, gpt, llama]}
      _models3 = ModelWithFallbacks {mwfModel = gemini, mwfFallbacks = [gpt, haiku, llama]}

  ag1 <- mkAgent expert _models3 True
  ag2 <- mkAgent student _models2 True
  ag3 <- mkAgent summarizer _models1 True
  ag4 <- mkAgent worker2 _models2 True
  let workflow1 =
        mkLoop 3 TranscriptPolicy [ag1, ag2] $
          WSeq ag2 ag1 TranscriptPolicy
  let workflow2 = WSeq workflow1 ag3 TranscriptSummaryOnly
  toolMap <-
    fsTools "./user-workspace/"
      <&> addTools
        [ toTool $
            subagent "subagent" "Use this tool to gain expert knowledge about a topic. Provide a topic." $
              \args _ctx ->
                (workflow2, PromptArgs {history = [], prompt = "Ask the expert about the topic: " <> args.prompt})
        ]

  t <- run Nothing toolMap "Which are the best programming languages for AI development? Try to use the subagent tool to gain expert knowledge about the topic." ag4
  TIO.putStrLn t

--     -- pr = Prompt {agentWithModels = ag, history = [], prompt = "What is the capital of France?"}
--     -- step = RunPrompt pr (PromptStatePending [])
--     wpr =
--       WSeq
--         (WPrompt ag (Just (CID "1")))
--         (WPrompt ag2 (Just (CID "2")))
--         TranscriptPolicy
--     pa = PromptArgs {history = [], prompt = "What is the capital of France?"}
--     step = RunWorkflow (WLoop 3 wpr TranscriptPolicy [CID "1", CID "2"]) pa
-- fr <- loop rt step []
-- print fr

-- ---------------------------------------------------------------------------
-- Demos
-- ---------------------------------------------------------------------------

printGenerateResult :: Either GenerateErrorResult GenerateTextResult -> IO ()
printGenerateResult = \case
  Left err -> do
    putStrLn "Generation failed:"
    print err
  Right ok -> do
    putStrLn "Final text:"
    TIO.putStrLn ok.gtrText
    putStrLn "Usage:"
    print ok.gtrUsage

onStreamChunk :: StreamChunk -> IO ()
onStreamChunk = \case
  AnswerDelta txt -> TIO.putStr txt
  ReasoningDelta txt -> TIO.putStr txt
  PreambleDelta txt -> TIO.putStr txt
  StreamToolCallChunk _ -> pure ()

printEvent :: GenerateEvent -> IO ()
printEvent ev = do
  putStrLn "--------------------------------"
  print ev

llmHooks :: Hooks -> LLMHooks
llmHooks hooks =
  LLMHooks
    { onLLMRequest = hooks.onRequest,
      onLLMResponse = hooks.onResponse,
      onLLMResponseError = hooks.onResponseError
    }

run :: Maybe AbortSignal -> ToolMap -> Text -> Workflow -> IO Text
run abortSignal toolMap prompt wf = do
  genId <- generate
  let rt =
        RuntimeArgs
          { rtGenerationId = genId,
            rtAbortSignal = abortSignal,
            rtLLMHooks = llmHooks defaultDebugHooks,
            rtHooks =
              defaultDebugHooks
                { onLog = \level msg -> TIO.putStrLn ("[" <> T.pack (show level) <> "] " <> msg)
                },
            rtOnEvent = printEvent,
            rtReadonly = False,
            rtToolMap = toolMap
          }
  r <- loop rt (RunWorkflow wf (PromptArgs {history = [], prompt})) []
  pure r.text

mkAgent :: Agent -> ModelWithFallbacks -> Bool -> IO Workflow
mkAgent ag models False = pure $ WPrompt (AgentWithModels ag models) Nothing
mkAgent ag models True = do
  WPrompt (AgentWithModels ag models) . Just . CID <$> generate

mkLoop :: Int -> TranscriptPolicy -> [Workflow] -> Workflow -> Workflow
mkLoop n policy scope wf = WLoop n wf policy cids
  where
    getCid (WPrompt _ag (Just cid)) = [cid]
    getCid _ = []
    cids = concatMap getCid scope

addTools :: [Tool] -> ToolMap -> ToolMap
addTools tools toolMap = toolMap <> Map.fromList [(tool.toolDef.toolName, tool) | tool <- tools]
