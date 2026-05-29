{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException (SomeException), catch)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Heptapod (generate)
import LLM (ThinkingMode (..), deepSeekGateway, mkFsConfig, toTool)
import LLM.Agent.Generate5
  ( StackRuntime (..),
    generateText5,
    streamText5,
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

model1 :: Text -> ModelConfig
model1 apiKey =
  ModelConfig
    { mcGateway = deepSeekGateway apiKey,
      mcModel = "deepseek-v4-flash",
      mcPricing = PricingInfo {pricePerMillionInput = 1.0, pricePerMillionOutput = 5.00},
      mcMaxTokens = 1024,
      mcTemperature = Nothing,
      mcThinking = Just ThinkingMode {tmEnabled = True, tmEffort = Just "max"},
      mcRequestTimeout = Nothing,
      mcThrottleDelay = Nothing,
      mcRetryCount = 0,
      mcJitterBackoff = 1_000
    }

main :: IO ()
main = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()
  apiKey <- T.pack <$> getEnv "DEEPSEEK_API_KEY"
  let models = ModelWithFallbacks (model1 apiKey) []
  genId <- generate
  fsConfig <- mkFsConfig "./user-workspace/"

  let worker = createWorkerAgent fsConfig
      orchestrator = createOrchestratorAgent
      runtime =
        RuntimeArgs
          { rtGenerationId = genId,
            rtAbortSignal = Nothing,
            rtLLMHooks =
              LLMHooks
                { onLLMRequest = \_ _ -> pure (),
                  onLLMResponse = \_ _ -> pure (),
                  onLLMResponseError = \_ _ -> pure ()
                },
            rtHooks = noHooks,
            rtOnEvent = printEvent,
            rtReadonly = False
          }
      stackRuntime =
        StackRuntime
          { srUTools =
              Map.fromList
                [ ("subagent", uTool1 (worker, models, runtime)),
                  ("run_workflow", uTool2 (worker, worker, worker, models, runtime))
                ],
            srLoopPredicates = Map.empty,
            srMergeFns = Map.empty
          }

  mode <- parseMode <$> getArgs
  case mode of
    ModeStream ->
      demoStreamText5 stackRuntime orchestrator models runtime
    ModeGenerate ->
      demoGenerateText5 stackRuntime orchestrator models runtime
    ModeBoth -> do
      demoStreamText5 stackRuntime orchestrator models runtime
      putStrLn ""
      demoGenerateText5 stackRuntime orchestrator models runtime

-- ---------------------------------------------------------------------------
-- Demos
-- ---------------------------------------------------------------------------

data DemoMode = ModeStream | ModeGenerate | ModeBoth

parseMode :: [String] -> DemoMode
parseMode = \case
  ["generate"] -> ModeGenerate
  ["stream"] -> ModeStream
  ["both"] -> ModeBoth
  [] -> ModeStream
  _ -> ModeStream

-- | Non-streaming run via 'generateText5' (one LLM / one tool per internal step).
demoGenerateText5 ::
  StackRuntime ->
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  IO ()
demoGenerateText5 rt agent models runtime = do
  putStrLn "=== generateText5 (subagent UTool) ==="
  let turns =
        [ UserTurn
            "Summarize all files in the workspace. \
            \Use the subagent tool with a clear prompt; do not read files yourself."
        ]
  result <- generateText5 rt agent models runtime turns
  printGenerateResult result

-- | Streaming run via 'streamText5' (workflow UTool returns CmdRunWorkflow).
demoStreamText5 ::
  StackRuntime ->
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  IO ()
demoStreamText5 rt agent models runtime = do
  putStrLn "=== streamText5 (run_workflow UTool) ==="
  let turns =
        [ UserTurn
            "Process this request with the run_workflow tool (not subagent): \
            \'List the top-level files in the workspace and suggest one improvement.\'"
        ]
  putStrLn "--- streaming ---"
  result <- streamText5 rt onStreamChunk agent models runtime turns
  putStrLn ""
  putStrLn "--- done ---"
  printGenerateResult result

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
