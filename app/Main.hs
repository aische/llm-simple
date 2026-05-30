{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException (SomeException), catch)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Heptapod (generate)
import LLM (Hooks (..), ThinkingMode (..), claudeGateway, deepSeekGateway, defaultDebugHooks, mkFsConfig, toTool)
import LLM.Agent.Generate6
  ( AgentWithModels (..),
    CID (CID),
    FinalResult (FinalResult),
    Kont,
    MergePolicy (MergePolicy),
    Prompt (..),
    PromptArgs (..),
    PromptState (PromptStateFinal, PromptStatePending),
    Step (RunPrompt, RunWorkflow),
    TranscriptPolicy (TranscriptPolicy),
    Workflow (..),
    eval,
    loop,
  )
import LLM.Agent.Types
  ( Agent (..),
    GenerateEvent (..),
    RuntimeArgs (..),
  )
import LLM.Core.Types (LLMHooks (..), Turn (UserTurn))
import LLM.Core.Usage (PricingInfo (..), Usage)
import LLM.Generate.Logger (noHooks)
import LLM.Generate.ModelConfig (ModelConfig (..), ModelWithFallbacks (..))
import LLM.Generate.Types
  ( GenerateErrorResult (..),
    GenerateTextResult (..),
    StreamChunk (..),
  )
import LLM.Load.LoadModels (loadModelsOrThrow)
import LLM.Tools.DirectoryTree (directoryTreeToolTyped)
import LLM.Tools.FsConfig (FsConfig)
import LLM.Tools.Readdir (readdirToolTyped)
import LLM.Tools.Readfile (readfileToolTyped)
import LLM.Tools.Writefile (writefileToolTyped)
import System.Environment (getArgs, getEnv)
import UTool1 (uTool1)
import UTool2 (uTool2)

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
createWorkerAgent :: FsConfig -> Agent
createWorkerAgent fsConfig =
  Agent
    { agName = "worker",
      agSystemPrompt = Just "You are a helpful assistant with filesystem access.",
      agTools =
        [ toTool $ readfileToolTyped fsConfig,
          toTool $ writefileToolTyped fsConfig,
          toTool $ readdirToolTyped fsConfig,
          toTool $ directoryTreeToolTyped fsConfig
        ],
      agUTools = [],
      agMaxToolRounds = 6,
      agContextWindow = Nothing
    }

createWorker2Agent :: Agent
createWorker2Agent =
  Agent
    { agName = "worker2",
      agSystemPrompt = Just "You are a writer who turns a user prompt into a poem.",
      agTools = [],
      agUTools = [],
      agMaxToolRounds = 6,
      agContextWindow = Nothing
    }

asker :: Agent
asker =
  Agent
    { agName = "asker",
      agSystemPrompt =
        Just
          "The user will provide a statement or a description. \
          \Ask one question about the topic so the user will provide a more detailed answer.",
      agTools = [],
      agUTools = [],
      agMaxToolRounds = 6,
      agContextWindow = Nothing
    }

model1 :: Text -> ModelConfig
model1 apiKey =
  ModelConfig
    { mcGateway = claudeGateway apiKey,
      mcModel = "claude-haiku-4-5-20251001",
      mcPricing = PricingInfo {pricePerMillionInput = 1.0, pricePerMillionOutput = 5.00},
      mcMaxTokens = 1024,
      mcTemperature = Nothing,
      mcThinking = Just ThinkingMode {tmEnabled = True, tmEffort = Just "max"},
      mcRequestTimeout = Nothing,
      mcThrottleDelay = Nothing,
      mcRetryCount = 0,
      mcJitterBackoff = 1_000
    }

llmHooks :: Hooks -> LLMHooks
llmHooks hooks =
  LLMHooks
    { onLLMRequest = hooks.onRequest,
      onLLMResponse = hooks.onResponse,
      onLLMResponseError = hooks.onResponseError
    }

main :: IO ()
main = do
  (gpt, llama) <- loadModelsOrThrow "./model-catalog.json" ("gpt_4_1", "llama_3_2")
  let models = ModelWithFallbacks {mwfModel = gpt, mwfFallbacks = [llama]}
  fsConfig <- mkFsConfig "./user-workspace/"
  genId <- generate
  let worker = createWorkerAgent fsConfig
      rt = RuntimeArgs {rtGenerationId = genId, rtAbortSignal = Nothing, rtLLMHooks = llmHooks defaultDebugHooks, rtHooks = defaultDebugHooks, rtOnEvent = printEvent, rtReadonly = False}
      ag = AgentWithModels {agent = worker, models = models}
      ag2 = AgentWithModels {agent = asker, models = models}
      -- pr = Prompt {agentWithModels = ag, history = [], prompt = "What is the capital of France?"}
      -- step = RunPrompt pr (PromptStatePending [])
      wpr =
        WSeq
          (WPrompt ag (Just (CID "1")))
          (WPrompt ag2 (Just (CID "2")))
          TranscriptPolicy
      pa = PromptArgs {history = [], prompt = "What is the capital of France?"}
      step = RunWorkflow (WLoop 3 wpr TranscriptPolicy [CID "1", CID "2"]) pa
  fr <- loop rt step []
  print fr

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
