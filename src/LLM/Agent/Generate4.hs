{-# LANGUAGE RankNTypes #-}

{- HLINT ignore "Use undefined" -}
module LLM.Agent.Generate4
  ( -- * Identifiers
    EnvId,
    FrameId,
    WorkflowNodeId,
    -- * Environment
    Env (..),
    EnvStore (..),
    EnvOverrides (..),
    mkEnv,
    forkEnv,
    lookupEnv,
    -- * Frame and machine
    Frame (..),
    ResumePoint (..),
    Machine (..),
    MachineConfig (..),
    MachineError (..),
    initialMachine,
    pushFrame,
    popFrame,
    replaceTopFrame,
    topFrame,
    machineDepth,
    -- * ChatStep
    ChatStep (..),
    buildChatStep,
    buildAgentStep,
    -- * Interpreter-only steps
    InterpreterStep (..),
    runInterpreterStep,
    -- * Step interpreter
    StepOutcome (..),
    runMachine,
    runStep,
    runFrame,
    checkAbort,
    checkMaxRounds,
    failGeneration,
    finishSuccess,
    callModel,
    execTools,
    runToolsWithAbortChecks,
    -- * Tool results and commands
    ToolOutcome (..),
    AgentCommand (..),
    ToolExecute,
    executeToolOutcome,
    applyAgentCommand,
    summarizeForParent,
    toolExecuteLegacy,
    -- * Subagent, handoff, dialog
    SubagentSpec (..),
    HandoffSpec (..),
    HandoffContextMode (..),
    DialogSpec (..),
    DialogSummary (..),
    DialogTranscript (..),
    TranscriptPolicy (..),
    PopResult (..),
    runSubagent,
    runHandoff,
    runDialog,
    -- * Static workflow
    Workflow (..),
    AgentNodeInput (..),
    WorkflowInput (..),
    WorkflowDialog (..),
    WorkflowHandoff (..),
    WorkflowSubagent (..),
    MergePolicy (..),
    WorkflowResult (..),
    WorkflowContext (..),
    runWorkflow,
    runPipe,
    runParallel,
    compileWorkflowToSteps,
    compileWorkflowToMachine,
    -- * Events (orchestration extensions)
    OrchestrationEventDetail (..),
    -- * Public entry points
    generateText,
    streamText,
    generateTextWorkflow,
    generateTextMachine,
  )
where

import Data.Aeson (Value)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.UUID.Types (UUID)
import LLM.Agent.Types
  ( Agent,
    EventObserver,
    RuntimeArgs,
    Tool,
    ToolContext,
  )
import LLM.Core.Abort (AbortSignal)
import LLM.Core.Types
  ( ChatResponse,
    LLMHooks,
    ToolCall,
    ToolResult,
    Turn,
  )
import LLM.Core.Usage (Usage)
import LLM.Generate.Logger (Hooks, LogLevel)
import LLM.Generate.ModelConfig (ModelWithFallbacks)
import LLM.Generate.Types
  ( GenerateError,
    GenerateErrorResult,
    GenerateResult,
    GenerateTextResult,
    StreamChunk,
  )

-- ---------------------------------------------------------------------------
-- Identifiers
-- ---------------------------------------------------------------------------

type EnvId = Int

type FrameId = Int

type WorkflowNodeId = Text

-- ---------------------------------------------------------------------------
-- Environment store
-- ---------------------------------------------------------------------------

data Env = Env
  { envId :: EnvId,
    envAgent :: Agent,
    envModels :: ModelWithFallbacks,
    envRt :: RuntimeArgs,
    envCall ::
      Agent ->
      ModelWithFallbacks ->
      RuntimeArgs ->
      [Turn] ->
      IO (GenerateResult ChatResponse)
  }

data EnvStore = EnvStore
  { esNextId :: EnvId,
    esMap :: Map EnvId Env
  }

data EnvOverrides = EnvOverrides
  { eoAgent :: Maybe Agent,
    eoModels :: Maybe ModelWithFallbacks,
    eoRt :: Maybe RuntimeArgs
  }

mkEnv ::
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  (Agent -> ModelWithFallbacks -> RuntimeArgs -> [Turn] -> IO (GenerateResult ChatResponse)) ->
  Env
mkEnv = undefined

forkEnv :: Env -> EnvOverrides -> EnvStore -> (EnvStore, EnvId)
forkEnv = undefined

lookupEnv :: EnvStore -> EnvId -> Maybe Env
lookupEnv = undefined

-- ---------------------------------------------------------------------------
-- Frame and machine
-- ---------------------------------------------------------------------------

data Frame = Frame
  { frId :: FrameId,
    frEnvId :: EnvId,
    frStep :: ChatStep,
    frResume :: Maybe ResumePoint
  }

data ResumePoint
  = ResumeAfterTools
      { rpLoopCount :: Int,
        rpAssistant :: Turn,
        rpAccTurns :: [Turn],
        rpCurrentTurns :: [Turn],
        rpUsage :: Usage
      }
  | ResumeAfterSubagent
      { rpParentFrameId :: FrameId,
        rpToolCallId :: Text
      }
  | ResumeAfterDialog
      { rpParentFrameId :: FrameId,
        rpMergeInto :: ToolCall
      }

data Machine = Machine
  { mEnvStore :: EnvStore,
    mStack :: NonEmpty Frame,
    mNextFrameId :: FrameId,
    mDepth :: Int,
    mMaxDepth :: Int,
    mGlobalUsage :: Usage,
    mRootGenerationId :: UUID
  }

data MachineConfig = MachineConfig
  { mcMaxStackDepth :: Int,
    mcMaxToolRoundsPerFrame :: Maybe Int,
    mcSharedAbortSignal :: Maybe AbortSignal
  }

data MachineError
  = StackOverflow
  | EmptyStack
  | EnvNotFound EnvId
  | FrameNotFound FrameId

initialMachine ::
  Env ->
  ChatStep ->
  MachineConfig ->
  Machine
initialMachine = undefined

pushFrame :: Machine -> Frame -> Either MachineError Machine
pushFrame = undefined

popFrame :: Machine -> PopResult -> Either MachineError Machine
popFrame = undefined

replaceTopFrame :: Machine -> Frame -> Machine
replaceTopFrame = undefined

topFrame :: Machine -> Frame
topFrame = undefined

machineDepth :: Machine -> Int
machineDepth = undefined

-- ---------------------------------------------------------------------------
-- ChatStep (pure agent program, CPS)
-- ---------------------------------------------------------------------------

data ChatStep
  = CallModel
      { csCurrentTurns :: [Turn],
        csAccTurns :: [Turn],
        csUsage :: Usage,
        csLoopCount :: Int,
        csOnModelResult :: GenerateResult ChatResponse -> ChatStep
      }
  | ExecTools
      { csLoopCount :: Int,
        csCalls :: [ToolCall],
        csRespText :: Text,
        csReasoning :: Maybe Text,
        csEnvId :: EnvId,
        csCurrentTurns :: [Turn],
        csAccTurns :: [Turn],
        csUsage :: Usage,
        csOnToolsResult :: GenerateResult [ToolResult] -> ChatStep
      }
  | Done (Either GenerateErrorResult GenerateTextResult)
  | RunDialog DialogSpec (DialogSummary -> ChatStep)
  | RunWorkflow Workflow (WorkflowResult -> ChatStep)

buildChatStep ::
  EnvId ->
  [Turn] ->
  [Turn] ->
  Usage ->
  Int ->
  ChatStep
buildChatStep = undefined

buildAgentStep ::
  Agent ->
  RuntimeArgs ->
  [Turn] ->
  [Turn] ->
  Usage ->
  Int ->
  ChatStep
buildAgentStep = undefined

-- ---------------------------------------------------------------------------
-- Interpreter-only steps
-- ---------------------------------------------------------------------------

data InterpreterStep
  = CheckAbort (Bool -> InterpreterStep)
  | Log LogLevel Text InterpreterStep
  | Throttle Int InterpreterStep
  | RunChat ChatStep

runInterpreterStep ::
  Machine ->
  InterpreterStep ->
  IO (Either GenerateErrorResult GenerateTextResult)
runInterpreterStep = undefined

-- ---------------------------------------------------------------------------
-- Step interpreter (machine driver)
-- ---------------------------------------------------------------------------

data StepOutcome m
  = StepContinue m
  | StepFinished (Either GenerateErrorResult GenerateTextResult)
  | StepPush Frame m
  | StepHandoff Frame m

runMachine ::
  Machine ->
  IO (Either GenerateErrorResult GenerateTextResult)
runMachine = undefined

runStep :: Machine -> ChatStep -> IO (StepOutcome Machine)
runStep = undefined

runFrame :: Machine -> Frame -> IO (StepOutcome Machine)
runFrame = undefined

checkAbort ::
  Machine ->
  [Turn] ->
  Usage ->
  IO (Either GenerateErrorResult ())
checkAbort = undefined

checkMaxRounds ::
  Machine ->
  Int ->
  [Turn] ->
  Usage ->
  IO (Either GenerateErrorResult ())
checkMaxRounds = undefined

failGeneration ::
  Machine ->
  GenerateErrorResult ->
  IO (Either GenerateErrorResult GenerateTextResult)
failGeneration = undefined

finishSuccess ::
  Machine ->
  GenerateTextResult ->
  IO (Either GenerateErrorResult GenerateTextResult)
finishSuccess = undefined

callModel ::
  Machine ->
  EnvId ->
  [Turn] ->
  IO (GenerateResult ChatResponse)
callModel = undefined

execTools ::
  Machine ->
  EnvId ->
  ToolContext ->
  [Tool] ->
  [ToolCall] ->
  IO (GenerateResult [ToolResult])
execTools = undefined

runToolsWithAbortChecks ::
  Machine ->
  [Turn] ->
  Usage ->
  [Tool] ->
  ToolContext ->
  [ToolCall] ->
  IO (GenerateResult [ToolResult])
runToolsWithAbortChecks = undefined

-- ---------------------------------------------------------------------------
-- Tool results and commands
-- ---------------------------------------------------------------------------

data ToolOutcome
  = ToolReply Text
  | ToolCommand AgentCommand
  | ToolReplyAndCommand Text AgentCommand

data AgentCommand
  = PushSubagent SubagentSpec
  | HandoffTo HandoffSpec
  | PushSteps [ChatStep]
  | PopWith Text
  | RunDialogCommand DialogSpec
  | FailCommand GenerateError

type ToolExecute = ToolContext -> Value -> IO ToolOutcome

executeToolOutcome ::
  Machine ->
  ToolContext ->
  Tool ->
  ToolCall ->
  IO (Either GenerateError (ToolOutcome, Maybe ToolResult))
executeToolOutcome = undefined

applyAgentCommand ::
  Machine ->
  AgentCommand ->
  IO (Either MachineError Machine)
applyAgentCommand = undefined

summarizeForParent :: PopResult -> Text
summarizeForParent = undefined

toolExecuteLegacy :: ToolContext -> Value -> IO Text
toolExecuteLegacy = undefined

-- ---------------------------------------------------------------------------
-- Subagent, handoff, dialog
-- ---------------------------------------------------------------------------

data SubagentSpec = SubagentSpec
  { ssAgent :: Agent,
    ssModels :: ModelWithFallbacks,
    ssRtOverrides :: Maybe RuntimeArgs,
    ssInitialTurns :: [Turn],
    ssMaxToolRounds :: Maybe Int,
    ssTranscriptPolicy :: TranscriptPolicy,
    ssSummaryPrompt :: Maybe Text
  }

data HandoffSpec = HandoffSpec
  { hsTarget :: SubagentSpec,
    hsContextMode :: HandoffContextMode,
    hsReplaceStack :: Bool
  }

data HandoffContextMode
  = HandoffFullTranscript
  | HandoffSummary Text
  | HandoffWindow Int

data DialogSpec = DialogSpec
  { dsAgentA :: Agent,
    dsAgentB :: Agent,
    dsModelsA :: ModelWithFallbacks,
    dsModelsB :: ModelWithFallbacks,
    dsRt :: RuntimeArgs,
    dsTopic :: Text,
    dsSeedTurns :: [Turn],
    dsMaxRounds :: Int,
    dsSummarizer :: Maybe Agent
  }

data DialogSummary = DialogSummary
  { dsText :: Text,
    dsTranscript :: [Turn],
    dsUsage :: Usage
  }

data DialogTranscript = DialogTranscript
  { dtTurns :: [Turn],
    dtUsage :: Usage
  }

data TranscriptPolicy
  = TranscriptIsolated
  | TranscriptShared
  | TranscriptSummaryOnly

data PopResult = PopResult
  { prSummary :: Text,
    prStructured :: Maybe Value,
    prUsage :: Usage,
    prNewTurns :: [Turn]
  }

runSubagent :: Machine -> SubagentSpec -> IO (Either GenerateError PopResult)
runSubagent = undefined

runHandoff :: Machine -> HandoffSpec -> IO (Either MachineError Machine)
runHandoff = undefined

runDialog :: Machine -> DialogSpec -> IO (Either GenerateError DialogSummary)
runDialog = undefined

-- ---------------------------------------------------------------------------
-- Static workflow orchestration
-- ---------------------------------------------------------------------------

data Workflow
  = RunAgent AgentNodeInput
  | Seq [Workflow]
  | Par [Workflow] MergePolicy
  | Dialog WorkflowDialog
  | Handoff WorkflowHandoff
  | Subagent WorkflowSubagent

data AgentNodeInput = AgentNodeInput
  { aniAgent :: Agent,
    aniModels :: ModelWithFallbacks,
    aniRt :: RuntimeArgs,
    aniInput :: WorkflowInput
  }

data WorkflowInput
  = WInputTurns [Turn]
  | WInputText Text
  | WInputFromPrior WorkflowNodeId

data WorkflowDialog = WorkflowDialog
  { wdSpec :: DialogSpec,
    wdNodeId :: WorkflowNodeId
  }

data WorkflowHandoff = WorkflowHandoff
  { whSpec :: HandoffSpec,
    whNodeId :: WorkflowNodeId
  }

data WorkflowSubagent = WorkflowSubagent
  { wsSpec :: SubagentSpec,
    wsNodeId :: WorkflowNodeId
  }

data MergePolicy
  = MergeConcat
  | MergeFirstSuccess
  | MergeWithAgent Agent
  | MergeCustom (forall m. Monad m => [WorkflowResult] -> m WorkflowResult)

data WorkflowResult = WorkflowResult
  { wrNodeId :: WorkflowNodeId,
    wrOutput :: Either GenerateErrorResult GenerateTextResult,
    wrUsage :: Usage
  }

data WorkflowContext = WorkflowContext
  { wcAbortSignal :: Maybe AbortSignal,
    wcOnEvent :: EventObserver,
    wcHooks :: Hooks,
    wcLLMHooks :: LLMHooks
  }

runWorkflow :: Workflow -> WorkflowContext -> IO WorkflowResult
runWorkflow = undefined

runPipe :: [Workflow] -> WorkflowContext -> IO WorkflowResult
runPipe = undefined

runParallel :: [Workflow] -> MergePolicy -> WorkflowContext -> IO WorkflowResult
runParallel = undefined

compileWorkflowToSteps :: Workflow -> ChatStep
compileWorkflowToSteps = undefined

compileWorkflowToMachine :: Env -> Workflow -> Machine
compileWorkflowToMachine = undefined

-- ---------------------------------------------------------------------------
-- Events (orchestration extensions; see also 'GenerateEventDetail' in Types)
-- ---------------------------------------------------------------------------

data OrchestrationEventDetail
  = SubagentStarted FrameId SubagentSpec
  | SubagentFinished FrameId PopResult
  | HandoffStarted FrameId HandoffSpec
  | HandoffFinished FrameId
  | DialogStarted FrameId DialogSpec
  | DialogTurn FrameId Int Turn
  | DialogFinished FrameId DialogSummary
  | WorkflowNodeStarted WorkflowNodeId
  | WorkflowNodeFinished WorkflowNodeId WorkflowResult
  | FramePushed FrameId FrameId
  | FramePopped FrameId FrameId

-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

generateText ::
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
generateText = undefined

streamText ::
  (StreamChunk -> IO ()) ->
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
streamText = undefined

generateTextWorkflow :: Workflow -> WorkflowContext -> IO WorkflowResult
generateTextWorkflow = undefined

generateTextMachine ::
  Machine ->
  IO (Either GenerateErrorResult GenerateTextResult)
generateTextMachine = undefined
