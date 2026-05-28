{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Eta reduce" -}
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

    -- * UTool results and commands
    UTool (..),
    utTool,
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

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (foldM)
import Data.Aeson (Value)
import Data.Aeson qualified as AE
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID.Types (UUID)
import LLM.Agent.Events (emitEvent)
import LLM.Agent.ToolUtils
  ( createToolContext,
    getResolvedTools,
    windowOffset,
  )
import LLM.Agent.Types
  ( Agent (..),
    EventObserver,
    GenerateEventDetail (..),
    RuntimeArgs (..),
    Tool (..),
    ToolContext,
  )
import LLM.Core.Abort (AbortSignal, isAbortedMaybe)
import LLM.Core.Types
  ( ChatResponse (..),
    LLMHooks (..),
    ToolCall (..),
    ToolDef (..),
    ToolResult,
    Turn (..),
  )
import LLM.Core.Usage (Usage (..), emptyUsage)
import LLM.Core.Utils (getToolCalls, toolResult)
import LLM.Generate.Generate
  ( generateTextWithFallbacks,
    streamTextWithFallbacks,
  )
import LLM.Generate.Logger (Hooks, LogLevel, noHooks)
import LLM.Generate.ModelConfig (ModelWithFallbacks)
import LLM.Generate.Types
  ( GenRequest (..),
    GenerateError (..),
    GenerateErrorResult (..),
    GenerateResult,
    GenerateTextResult (..),
    StreamChunk (..),
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
mkEnv agent models rt call =
  Env
    { envId = 0,
      envAgent = agent,
      envModels = models,
      envRt = rt,
      envCall = call
    }

forkEnv :: Env -> EnvOverrides -> EnvStore -> (EnvStore, EnvId)
forkEnv parent overrides store =
  let newId = store.esNextId
      child =
        parent
          { envId = newId,
            envAgent = fromMaybe parent.envAgent overrides.eoAgent,
            envModels = fromMaybe parent.envModels overrides.eoModels,
            envRt = fromMaybe parent.envRt overrides.eoRt
          }
      store' =
        store
          { esNextId = newId + 1,
            esMap = Map.insert newId child store.esMap
          }
   in (store', newId)

lookupEnv :: EnvStore -> EnvId -> Maybe Env
lookupEnv store envId = Map.lookup envId store.esMap

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
    mUToolRegistry :: UToolRegistry,
    mStack :: NE.NonEmpty Frame,
    mNextFrameId :: FrameId,
    mDepth :: Int,
    mMaxDepth :: Int,
    mGlobalUsage :: Usage,
    mRootGenerationId :: UUID,
    mConfig :: MachineConfig
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
  deriving stock (Show, Eq)

defaultMachineConfig :: MachineConfig
defaultMachineConfig =
  MachineConfig
    { mcMaxStackDepth = 32,
      mcMaxToolRoundsPerFrame = Nothing,
      mcSharedAbortSignal = Nothing
    }

initialMachine ::
  Env ->
  UToolRegistry ->
  ChatStep ->
  MachineConfig ->
  Machine
initialMachine env utoolRegistry step config =
  let store =
        EnvStore
          { esNextId = env.envId + 1,
            esMap = Map.singleton env.envId env
          }
      frame0 =
        Frame
          { frId = 0,
            frEnvId = env.envId,
            frStep = step,
            frResume = Nothing
          }
   in Machine
        { mEnvStore = store,
          mUToolRegistry = utoolRegistry,
          mStack = frame0 NE.:| [],
          mNextFrameId = 1,
          mDepth = 1,
          mMaxDepth = config.mcMaxStackDepth,
          mGlobalUsage = mempty,
          mRootGenerationId = env.envRt.rtGenerationId,
          mConfig = config
        }

pushFrame :: Machine -> Frame -> Either MachineError Machine
pushFrame machine frame
  | machine.mDepth >= machine.mMaxDepth = Left StackOverflow
  | otherwise =
      Right
        machine
          { mStack = frame NE.<| machine.mStack,
            mDepth = machine.mDepth + 1
          }

popFrame :: Machine -> PopResult -> Either MachineError Machine
popFrame machine pop =
  case NE.uncons machine.mStack of
    (_, Nothing) -> Left EmptyStack
    (_, Just rest) ->
      Right
        machine
          { mStack = rest,
            mDepth = machine.mDepth - 1,
            mGlobalUsage = machine.mGlobalUsage <> pop.prUsage
          }

replaceTopFrame :: Machine -> Frame -> Machine
replaceTopFrame machine frame =
  case NE.uncons machine.mStack of
    (_, Nothing) -> machine {mStack = frame NE.:| []}
    (_, Just rest) -> machine {mStack = frame NE.<| rest}

topFrame :: Machine -> Frame
topFrame machine = NE.head machine.mStack

machineDepth :: Machine -> Int
machineDepth machine = machine.mDepth

-- ---------------------------------------------------------------------------
-- ChatStep (pure agent program, CPS)
-- ---------------------------------------------------------------------------

data ChatStep
  = CallModel
      { csGenerationId :: UUID,
        csCurrentTurns :: [Turn],
        csAccTurns :: [Turn],
        csUsage :: Usage,
        csLoopCount :: Int,
        csEnvId :: EnvId,
        csOnModelResult :: GenerateResult ChatResponse -> ChatStep
      }
  | ExecTools
      { csLoopCount :: Int,
        csCalls :: [ToolCall],
        csRespText :: Text,
        csReasoning :: Maybe Text,
        csEnvId :: EnvId,
        csGenerationId :: UUID,
        csCurrentTurns :: [Turn],
        csAccTurns :: [Turn],
        csUsage :: Usage,
        csOnToolsResult :: GenerateResult [ToolResult] -> ChatStep
      }
  | Done (Either GenerateErrorResult GenerateTextResult)
  | RunDialog DialogSpec (DialogSummary -> ChatStep)
  | RunWorkflow Workflow (WorkflowResult -> ChatStep)
  | SeqSteps [ChatStep]

buildChatStep ::
  UUID ->
  EnvId ->
  [Turn] ->
  [Turn] ->
  Usage ->
  Int ->
  ChatStep
buildChatStep generationId envId currentTurns accTurns usage loopCount =
  CallModel
    { csGenerationId = generationId,
      csCurrentTurns = currentTurns,
      csAccTurns = accTurns,
      csUsage = usage,
      csLoopCount = loopCount,
      csEnvId = envId,
      csOnModelResult = afterModel
    }
  where
    afterModel :: GenerateResult ChatResponse -> ChatStep
    afterModel (Left err) = Done (Left (GenerateErrorResult err accTurns usage))
    afterModel (Right resp) =
      let txt = resp.respText
          toolCalls = getToolCalls resp
          roundUsage = fromMaybe emptyUsage resp.respUsage
          newUsage = usage <> roundUsage
       in case toolCalls of
            [] ->
              let finalTurn = AssistantTurn txt resp.respReasoning []
                  finalAcc = accTurns ++ [finalTurn]
               in Done $
                    Right
                      GenerateTextResult
                        { gtrGenerationId = generationId,
                          gtrNewMessages = finalAcc,
                          gtrText = txt,
                          gtrUsage = newUsage
                        }
            _ ->
              ExecTools
                { csLoopCount = loopCount,
                  csCalls = toolCalls,
                  csRespText = txt,
                  csReasoning = resp.respReasoning,
                  csEnvId = envId,
                  csGenerationId = generationId,
                  csCurrentTurns = currentTurns,
                  csAccTurns = accTurns,
                  csUsage = newUsage,
                  csOnToolsResult = afterTools txt resp.respReasoning toolCalls newUsage
                }

    afterTools ::
      Text ->
      Maybe Text ->
      [ToolCall] ->
      Usage ->
      GenerateResult [ToolResult] ->
      ChatStep
    afterTools txt reasoning toolCalls newUsage = \case
      Left err ->
        let assistantTurn = AssistantTurn txt reasoning toolCalls
         in Done (Left (GenerateErrorResult err (accTurns ++ [assistantTurn]) newUsage))
      Right toolResults ->
        let assistantTurn = AssistantTurn txt reasoning toolCalls
            toolTurn = ToolTurn toolResults
            turnsToAdd = [assistantTurn, toolTurn]
         in buildChatStep
              generationId
              envId
              (currentTurns ++ turnsToAdd)
              (accTurns ++ turnsToAdd)
              newUsage
              (loopCount + 1)

buildAgentStep ::
  Agent ->
  RuntimeArgs ->
  [Turn] ->
  [Turn] ->
  Usage ->
  Int ->
  ChatStep
buildAgentStep _agent rt currentTurns accTurns usage loopCount =
  buildChatStep rt.rtGenerationId 0 currentTurns accTurns usage loopCount

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
runInterpreterStep machine step = case step of
  CheckAbort k -> do
    aborted <- isAbortedForMachine machine
    runInterpreterStep machine (k aborted)
  Log _lvl _msg rest -> runInterpreterStep machine rest
  Throttle ms rest -> do
    threadDelay (ms * 1000)
    runInterpreterStep machine rest
  RunChat chat -> do
    let m' = setTopStep machine chat
    runMachine m'

-- ---------------------------------------------------------------------------
-- Step interpreter (machine driver)
-- ---------------------------------------------------------------------------

data StepOutcome m
  = StepContinue m
  | StepFinished (Either GenerateErrorResult GenerateTextResult)
  | StepPush Frame m
  | StepHandoff Frame m

data ToolsRun
  = ToolsComplete [ToolResult]
  | ToolsInterrupted Machine

runMachine ::
  Machine ->
  IO (Either GenerateErrorResult GenerateTextResult)
runMachine machine = loop machine
  where
    loop m = do
      outcome <- runFrame m (topFrame m)
      case outcome of
        StepFinished r -> pure r
        StepContinue m' -> loop m'
        StepPush fr m' -> case pushFrame m' fr of
          Left _ -> failWithMachineError m'
          Right m'' -> loop m''
        StepHandoff fr m' -> loop (replaceTopFrame m' fr)

runStep :: Machine -> ChatStep -> IO (StepOutcome Machine)
runStep machine step = case step of
  Done r ->
    if machine.mDepth <= 1
      then StepFinished <$> finishOrFail machine r
      else handleChildDone machine r
  CallModel {csCurrentTurns, csAccTurns, csUsage, csLoopCount, csEnvId, csOnModelResult} -> do
    checkAbort machine csAccTurns csUsage >>= \case
      Left errResult -> StepFinished <$> failGeneration machine errResult
      Right () ->
        checkMaxRounds machine csLoopCount csAccTurns csUsage >>= \case
          Left errResult -> StepFinished <$> failGeneration machine errResult
          Right () -> do
            result <- callModel machine csEnvId csCurrentTurns
            case result of
              Left err ->
                StepFinished
                  <$> failGeneration
                    machine
                    (GenerateErrorResult err csAccTurns csUsage)
              Right resp -> do
                let m' = setTopStep machine (csOnModelResult (Right resp))
                pure (StepContinue m')
  ExecTools {csLoopCount, csCalls, csRespText, csReasoning, csEnvId, csCurrentTurns, csAccTurns, csUsage, csOnToolsResult} -> do
    let assistantTurn = AssistantTurn csRespText csReasoning csCalls
        accWithAssistant = csAccTurns ++ [assistantTurn]
    checkAbort machine accWithAssistant csUsage >>= \case
      Left errResult -> StepFinished <$> failGeneration machine errResult
      Right () ->
        case lookupEnv machine.mEnvStore csEnvId of
          Nothing ->
            StepFinished
              <$> failGeneration
                machine
                (GenerateErrorResult GErrAllModelsFailed accWithAssistant csUsage)
          Just env -> do
            emitEvent env.envRt (MessageCreated assistantTurn)
            emitEvent env.envRt (ToolRoundStarted csLoopCount)
            let toolContext = createToolContext env.envAgent csCurrentTurns csUsage env.envRt
                tools = getResolvedTools env.envAgent env.envRt
                utools = map utTool tools
            runResult <-
              runToolsWithAbortChecks
                machine
                accWithAssistant
                csUsage
                utools
                toolContext
                csCalls
            case runResult of
              Left err ->
                StepFinished
                  <$> failGeneration
                    machine
                    (GenerateErrorResult err accWithAssistant csUsage)
              Right (ToolsInterrupted m') -> pure (StepContinue m')
              Right (ToolsComplete toolResults) -> do
                let toolTurn = ToolTurn toolResults
                emitEvent env.envRt (MessageCreated toolTurn)
                emitEvent env.envRt (ToolRoundFinished csLoopCount)
                let m' = setTopStep machine (csOnToolsResult (Right toolResults))
                pure (StepContinue m')
  RunDialog spec k -> do
    result <- runDialog machine spec
    case result of
      Left err ->
        let tf = topFrame machine
         in case lookupEnv machine.mEnvStore tf.frEnvId of
              Nothing ->
                pure (StepFinished (Left (GenerateErrorResult GErrAborted [] mempty)))
              Just _env ->
                StepFinished
                  <$> failGeneration
                    machine
                    (GenerateErrorResult err [] mempty)
      Right summary -> runStep machine (k summary)
  RunWorkflow wf k -> do
    ctx <- workflowContextFromMachine machine
    wr <- runWorkflow wf ctx
    runStep machine (k wr)
  SeqSteps [] -> pure (StepContinue machine)
  SeqSteps (s : ss) -> do
    outcome <- runStep machine s
    case outcome of
      StepFinished r -> pure (StepFinished r)
      StepContinue m' -> runStep m' (SeqSteps ss)
      StepPush fr m' -> pure (StepPush fr m')
      StepHandoff fr m' -> pure (StepHandoff fr m')

runFrame :: Machine -> Frame -> IO (StepOutcome Machine)
runFrame machine frame = runStep machine frame.frStep

checkAbort ::
  Machine ->
  [Turn] ->
  Usage ->
  IO (Either GenerateErrorResult ())
checkAbort machine acc usage = do
  aborted <- isAbortedForMachine machine
  if aborted
    then pure (Left (GenerateErrorResult GErrAborted acc usage))
    else pure (Right ())

checkMaxRounds ::
  Machine ->
  Int ->
  [Turn] ->
  Usage ->
  IO (Either GenerateErrorResult ())
checkMaxRounds machine loopCount acc usage =
  case machineTopEnv machine of
    Nothing -> pure (Left (GenerateErrorResult GErrAllModelsFailed acc usage))
    Just env ->
      let limit =
            fromMaybe
              env.envAgent.agMaxToolRounds
              machine.mConfig.mcMaxToolRoundsPerFrame
       in if loopCount >= limit
            then pure (Left (GenerateErrorResult GErrToolExceeded acc usage))
            else pure (Right ())

failGeneration ::
  Machine ->
  GenerateErrorResult ->
  IO (Either GenerateErrorResult GenerateTextResult)
failGeneration machine errResult =
  case machineTopEnv machine of
    Nothing -> pure (Left errResult)
    Just env -> do
      emitEvent env.envRt (GenerationFailed errResult.gerError errResult)
      pure (Left errResult)

finishSuccess ::
  Machine ->
  GenerateTextResult ->
  IO (Either GenerateErrorResult GenerateTextResult)
finishSuccess machine success =
  case machineTopEnv machine of
    Nothing -> pure (Right success)
    Just env -> do
      let finalTurn = case reverse success.gtrNewMessages of
            (t : _) -> t
            [] -> AssistantTurn success.gtrText Nothing []
      emitEvent env.envRt (MessageFinalized finalTurn)
      emitEvent env.envRt (GenerationFinished success)
      pure (Right success)

callModel ::
  Machine ->
  EnvId ->
  [Turn] ->
  IO (GenerateResult ChatResponse)
callModel machine envId turns =
  case lookupEnv machine.mEnvStore envId of
    Nothing -> pure (Left GErrAllModelsFailed)
    Just env -> env.envCall env.envAgent env.envModels env.envRt turns

execTools ::
  Machine ->
  EnvId ->
  ToolContext ->
  [UTool] ->
  [ToolCall] ->
  IO (Either GenerateError ToolsRun)
execTools machine _envId ctx tools toolCalls =
  runToolsWithAbortChecks machine [] mempty tools ctx toolCalls

runToolsWithAbortChecks ::
  Machine ->
  [Turn] ->
  Usage ->
  [UTool] ->
  ToolContext ->
  [ToolCall] ->
  IO (Either GenerateError ToolsRun)
runToolsWithAbortChecks machine partialAcc usage tools ctx toolCalls =
  go [] toolCalls
  where
    go acc [] = pure (Right (ToolsComplete (reverse acc)))
    go acc (tc : rest) = do
      checkAbort machine partialAcc usage >>= \case
        Left _ -> pure (Left GErrAborted)
        Right () -> do
          outcomeE <- case findTool tools tc of
            Nothing ->
              pure (Right (ToolReply ("Unknown tool: " <> tc.tcName), Just (toolResult tc ("Unknown tool: " <> tc.tcName))))
            Just tool -> executeToolOutcome machine ctx tool tc
          case outcomeE of
            Left err -> pure (Left err)
            Right (toolOutcome, mResult) -> do
              case toolOutcome of
                ToolCommand cmd -> do
                  cmdResult <- applyAgentCommand' machine (Just tc) cmd
                  case cmdResult of
                    Left _ -> pure (Left GErrAborted)
                    Right m' -> pure (Right (ToolsInterrupted m'))
                ToolReplyAndCommand _txt cmd -> do
                  cmdResult <- applyAgentCommand' machine (Just tc) cmd
                  case cmdResult of
                    Left _ -> pure (Left GErrAborted)
                    Right m' -> pure (Right (ToolsInterrupted m'))
                ToolReply _ ->
                  case mResult of
                    Nothing -> go acc rest
                    Just tr -> go (tr : acc) rest

-- ---------------------------------------------------------------------------
-- UTool results and commands
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

data UTool = UTool
  { utToolDef :: ToolDef,
    utToolExecute :: ToolContext -> Value -> IO ToolOutcome
  }

utTool :: Tool -> UTool
utTool tool =
  UTool
    { utToolDef = tool.toolDef,
      utToolExecute = \ctx val -> ToolReply <$> tool.toolExecute ctx val
    }

executeToolOutcome ::
  Machine ->
  ToolContext ->
  UTool ->
  ToolCall ->
  IO (Either GenerateError (ToolOutcome, Maybe ToolResult))
executeToolOutcome _machine ctx tool tc = do
  result <- try (toolExecuteLegacy ctx tc.tcArguments tool)
  case result of
    Left (e :: SomeException) ->
      let msg = "UTool error: " <> T.pack (show e)
       in pure (Right (ToolReply msg, Just (toolResult tc msg)))
    Right outcome ->
      let mTr = toolOutcomeToResult tc outcome
       in pure (Right (outcome, mTr))

toolOutcomeToResult :: ToolCall -> ToolOutcome -> Maybe ToolResult
toolOutcomeToResult tc = \case
  ToolReply txt -> Just (toolResult tc txt)
  ToolReplyAndCommand txt _ -> Just (toolResult tc txt)
  ToolCommand _ -> Nothing

toolExecuteLegacy :: ToolContext -> Value -> UTool -> IO ToolOutcome
toolExecuteLegacy ctx val tool = tool.utToolExecute ctx val

findTool :: [UTool] -> ToolCall -> Maybe UTool
findTool tools tc = go tools
  where
    go [] = Nothing
    go (t : rest) =
      case t.utToolDef of
        ToolDef {toolName} | toolName == tc.tcName -> Just t
        _ -> go rest

applyAgentCommand ::
  Machine ->
  AgentCommand ->
  IO (Either MachineError Machine)
applyAgentCommand machine cmd = applyAgentCommand' machine Nothing cmd

applyAgentCommand' ::
  Machine ->
  Maybe ToolCall ->
  AgentCommand ->
  IO (Either MachineError Machine)
applyAgentCommand' machine mTc cmd = case cmd of
  PushSubagent spec -> pushSubagentFrame machine mTc spec
  HandoffTo spec -> runHandoff machine spec
  PushSteps steps -> pushStepsFrame machine steps
  PopWith _summary -> pure (Right machine)
  RunDialogCommand spec -> pushDialogFrame machine spec
  FailCommand _err -> pure (Right machine)

summarizeForParent :: PopResult -> Text
summarizeForParent pop =
  case pop.prStructured of
    Nothing -> pop.prSummary
    Just v -> pop.prSummary <> "\n" <> T.pack (show v)

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
  { dgsText :: Text,
    dgsTranscript :: [Turn],
    dgsUsage :: Usage
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
runSubagent machine spec =
  case machineTopEnv machine of
    Nothing -> pure (Left GErrAborted)
    Just parentEnv -> do
      let overrides =
            EnvOverrides
              { eoAgent = Just spec.ssAgent,
                eoModels = Just spec.ssModels,
                eoRt = spec.ssRtOverrides
              }
          (store', childId) = forkEnv parentEnv overrides machine.mEnvStore
      emitOrchestrationEvent parentEnv.envRt (SubagentStarted (topFrame machine).frId spec)
      let childEnv = fromMaybe parentEnv (lookupEnv store' childId)
          genId = childEnv.envRt.rtGenerationId
          childStep = buildChatStep genId childId spec.ssInitialTurns [] mempty 0
          childMachine =
            (initialMachine childEnv machine.mUToolRegistry childStep machine.mConfig)
              { mEnvStore = store'
              }
      result <- runMachine childMachine
      let pop = popResultFromGenerate spec result
      emitOrchestrationEvent parentEnv.envRt (SubagentFinished (topFrame machine).frId pop)
      pure (Right pop)

runHandoff :: Machine -> HandoffSpec -> IO (Either MachineError Machine)
runHandoff machine spec =
  case machineTopEnv machine of
    Nothing -> pure (Left (EnvNotFound 0))
    Just parentEnv -> do
      emitOrchestrationEvent parentEnv.envRt (HandoffStarted (topFrame machine).frId spec)
      let target = spec.hsTarget
          turns = handoffTurns machine spec
          overrides =
            EnvOverrides
              { eoAgent = Just target.ssAgent,
                eoModels = Just target.ssModels,
                eoRt = target.ssRtOverrides
              }
          (store', childId) = forkEnv parentEnv overrides machine.mEnvStore
          childEnv = fromMaybe parentEnv (lookupEnv store' childId)
          genId = childEnv.envRt.rtGenerationId
          step = buildChatStep genId childId turns [] mempty 0
          frame =
            Frame
              { frId = machine.mNextFrameId,
                frEnvId = childId,
                frStep = step,
                frResume = Nothing
              }
          machine' =
            machine
              { mEnvStore = store',
                mNextFrameId = machine.mNextFrameId + 1
              }
      let result =
            if spec.hsReplaceStack
              then replaceTopFrame machine' frame
              else case pushFrame machine' frame of
                Left StackOverflow -> machine'
                Left _ -> machine'
                Right m -> m
      emitOrchestrationEvent parentEnv.envRt (HandoffFinished (topFrame machine).frId)
      pure (Right result)

runDialog :: Machine -> DialogSpec -> IO (Either GenerateError DialogSummary)
runDialog machine spec =
  case machineTopEnv machine of
    Nothing -> pure (Left GErrAborted)
    Just parentEnv -> do
      emitOrchestrationEvent parentEnv.envRt (DialogStarted (topFrame machine).frId spec)
      let (store0, envIdA) =
            forkEnv
              parentEnv
              EnvOverrides {eoAgent = Just spec.dsAgentA, eoModels = Just spec.dsModelsA, eoRt = Nothing}
              machine.mEnvStore
          envA = fromMaybe parentEnv (lookupEnv store0 envIdA)
          (store1, envIdB) =
            forkEnv
              envA
              EnvOverrides {eoAgent = Just spec.dsAgentB, eoModels = Just spec.dsModelsB, eoRt = Nothing}
              store0
          dialogMachine = machine {mEnvStore = store1}
          topicTurn = UserTurn spec.dsTopic
          initial = spec.dsSeedTurns ++ [topicTurn]
      (transcript, usage) <- dialogLoop dialogMachine envIdA envIdB spec.dsMaxRounds initial mempty 0
      let summaryText =
            case spec.dsSummarizer of
              Nothing -> lastAssistantText transcript
              Just _agent -> lastAssistantText transcript
      let summary =
            DialogSummary
              { dgsText = summaryText,
                dgsTranscript = transcript,
                dgsUsage = usage
              }
      emitOrchestrationEvent parentEnv.envRt (DialogFinished (topFrame machine).frId summary)
      pure (Right summary)

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
  | MergeCustom (forall m. (Monad m) => [WorkflowResult] -> m WorkflowResult)

data WorkflowResult = WorkflowResult
  { wrNodeId :: WorkflowNodeId,
    wrOutput :: Either GenerateErrorResult GenerateTextResult,
    wrUsage :: Usage
  }

data WorkflowContext = WorkflowContext
  { wcAbortSignal :: Maybe AbortSignal,
    wcUToolRegistry :: UToolRegistry,
    wcOnEvent :: EventObserver,
    wcHooks :: Hooks,
    wcLLMHooks :: LLMHooks,
    wcResults :: Map WorkflowNodeId WorkflowResult
  }

runWorkflow :: Workflow -> WorkflowContext -> IO WorkflowResult
runWorkflow wf ctx = runWorkflowNode wf ctx ""

runPipe :: [Workflow] -> WorkflowContext -> IO WorkflowResult
runPipe wfs ctx = runWorkflow (Seq wfs) ctx

runParallel :: [Workflow] -> MergePolicy -> WorkflowContext -> IO WorkflowResult
runParallel wfs policy ctx = do
  results <- mapM (`runWorkflow` ctx) wfs
  mergeWorkflowResults "" policy results ctx

compileWorkflowToSteps :: Workflow -> ChatStep
compileWorkflowToSteps wf = case wf of
  RunAgent AgentNodeInput {aniAgent, aniRt, aniInput} ->
    let turns = case aniInput of
          WInputTurns ts -> ts
          WInputText t -> [UserTurn t]
          WInputFromPrior _ -> []
     in buildAgentStep aniAgent aniRt turns [] mempty 0
  Seq wfs ->
    RunWorkflow (Seq wfs) (\wr -> Done wr.wrOutput)
  Par wfs pol ->
    RunWorkflow (Par wfs pol) (\wr -> Done wr.wrOutput)
  Dialog wd ->
    RunDialog wd.wdSpec (Done . Right . dialogSummaryToTextResult)
  Handoff wh ->
    RunWorkflow (Handoff wh) (\wr -> Done wr.wrOutput)
  Subagent ws ->
    RunWorkflow (Subagent ws) (\wr -> Done wr.wrOutput)

compileWorkflowToMachine :: Env -> UToolRegistry -> Workflow -> Machine
compileWorkflowToMachine env utoolRegistry wf =
  initialMachine env utoolRegistry (compileWorkflowToSteps wf) defaultMachineConfig

-- ---------------------------------------------------------------------------
-- Events (orchestration extensions)
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
  UToolRegistry ->
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
generateText utoolRegistry agent models rt initialTurns = do
  let env =
        mkEnv
          agent
          models
          rt
          (\a m r t -> generateTextWithFallbacks (createGenRequestU utoolRegistry a r t) m)
      env' = env {envId = 0}
      step = buildAgentStep agent rt initialTurns [] emptyUsage 0
      machine = initialMachine env' utoolRegistry step defaultMachineConfig
  emitEvent rt GenerationStarted
  generateTextMachine machine

streamText ::
  UToolRegistry ->
  (StreamChunk -> IO ()) ->
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
streamText utoolRegistry onChunk agent models rt initialTurns = do
  let env =
        mkEnv
          agent
          models
          rt
          (\a m r t -> streamTextWithFallbacks onChunk (createGenRequestU utoolRegistry a r t) m)
      env' = env {envId = 0}
      step = buildAgentStep agent rt initialTurns [] emptyUsage 0
      machine = initialMachine env' utoolRegistry step defaultMachineConfig
  emitEvent rt GenerationStarted
  generateTextMachine machine

generateTextWorkflow :: Workflow -> WorkflowContext -> IO WorkflowResult
generateTextWorkflow = runWorkflow

generateTextMachine ::
  Machine ->
  IO (Either GenerateErrorResult GenerateTextResult)
generateTextMachine machine =
  case lookupEnv machine.mEnvStore (NE.head machine.mStack).frEnvId of
    Nothing -> runMachine machine
    Just env -> do
      emitEvent env.envRt GenerationStarted
      runMachine machine

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

emitOrchestrationEvent :: RuntimeArgs -> OrchestrationEventDetail -> IO ()
emitOrchestrationEvent _rt _detail = pure ()

isAbortedForMachine :: Machine -> IO Bool
isAbortedForMachine machine =
  case machine.mConfig.mcSharedAbortSignal of
    Just sig -> isAbortedMaybe (Just sig)
    Nothing ->
      case machineTopEnv machine of
        Nothing -> pure False
        Just env -> isAbortedMaybe env.envRt.rtAbortSignal

machineTopEnv :: Machine -> Maybe Env
machineTopEnv machine =
  lookupEnv machine.mEnvStore (topFrame machine).frEnvId

setTopStep :: Machine -> ChatStep -> Machine
setTopStep machine step =
  let tf = topFrame machine
   in replaceTopFrame machine (tf {frStep = step})

updateTopResume :: Machine -> Maybe ResumePoint -> Machine
updateTopResume machine resume =
  let tf = topFrame machine
   in replaceTopFrame machine (tf {frResume = resume})

finishOrFail ::
  Machine ->
  Either GenerateErrorResult GenerateTextResult ->
  IO (Either GenerateErrorResult GenerateTextResult)
finishOrFail machine = \case
  Left err -> failGeneration machine err
  Right ok -> finishSuccess machine ok

handleChildDone ::
  Machine ->
  Either GenerateErrorResult GenerateTextResult ->
  IO (StepOutcome Machine)
handleChildDone machine result = case result of
  Left err -> do
    case popFrame machine (popResultFromError err) of
      Left _ -> pure (StepFinished (Left err))
      Right m' ->
        case m'.mStack of
          _ NE.:| [] -> pure (StepFinished (Left err))
          _ -> StepFinished <$> failGeneration m' err
  Right success -> do
    let pop = popResultFromSuccess TranscriptSummaryOnly success
    case popFrame machine pop of
      Left _ -> pure (StepFinished (Right success))
      Right m' ->
        case (topFrame m').frResume of
          Nothing -> pure (StepContinue m')
          Just rp -> do
            let nextStep = resumeParentStep m' pop rp
            pure (StepContinue (setTopStep m' nextStep))

resumeParentStep :: Machine -> PopResult -> ResumePoint -> ChatStep
resumeParentStep machine pop = \case
  ResumeAfterSubagent {rpToolCallId} ->
    let tc = ToolCall rpToolCallId "" AE.Null
        tr = toolResult tc (summarizeForParent pop)
     in case topFrame machine of
          Frame {frStep = ExecTools {csOnToolsResult, csCalls}} ->
            let results = map (\c -> if c.tcId == rpToolCallId then tr else toolResult c (summarizeForParent pop)) csCalls
             in csOnToolsResult (Right results)
          _ -> Done (Right (popToTextResult pop))
  ResumeAfterTools {rpLoopCount, rpCurrentTurns, rpAccTurns, rpUsage} ->
    let tf = topFrame machine
     in buildChatStep
          machine.mRootGenerationId
          tf.frEnvId
          rpCurrentTurns
          rpAccTurns
          rpUsage
          (rpLoopCount + 1)
  ResumeAfterDialog {rpMergeInto} ->
    let tr = toolResult rpMergeInto (summarizeForParent pop)
     in case topFrame machine of
          Frame {frStep = ExecTools {csOnToolsResult}} ->
            csOnToolsResult (Right [tr])
          _ -> Done (Right (popToTextResult pop))

popResultFromSuccess :: TranscriptPolicy -> GenerateTextResult -> PopResult
popResultFromSuccess policy success =
  PopResult
    { prSummary = success.gtrText,
      prStructured = Nothing,
      prUsage = success.gtrUsage,
      prNewTurns = case policy of
        TranscriptIsolated -> success.gtrNewMessages
        TranscriptShared -> success.gtrNewMessages
        TranscriptSummaryOnly -> []
    }

popResultFromError :: GenerateErrorResult -> PopResult
popResultFromError err =
  PopResult
    { prSummary = "",
      prStructured = Nothing,
      prUsage = err.gerUsage,
      prNewTurns = err.gerPartialNewMessages
    }

popResultFromGenerate :: SubagentSpec -> Either GenerateErrorResult GenerateTextResult -> PopResult
popResultFromGenerate spec = \case
  Left err -> popResultFromError err
  Right ok -> popResultFromSuccess spec.ssTranscriptPolicy ok

popToTextResult :: PopResult -> GenerateTextResult
popToTextResult pop =
  GenerateTextResult
    { gtrGenerationId = nilUuid,
      gtrNewMessages = pop.prNewTurns,
      gtrText = pop.prSummary,
      gtrUsage = pop.prUsage
    }

nilUuid :: UUID
nilUuid = read @UUID "00000000-0000-0000-0000-000000000000"

pushSubagentFrame ::
  Machine ->
  Maybe ToolCall ->
  SubagentSpec ->
  IO (Either MachineError Machine)
pushSubagentFrame machine mTc spec =
  case machineTopEnv machine of
    Nothing -> pure (Left (EnvNotFound 0))
    Just parentEnv -> do
      let tcId = maybe "" (.tcId) mTc
          parentFrame = topFrame machine
          resume =
            ResumeAfterSubagent
              { rpParentFrameId = parentFrame.frId,
                rpToolCallId = tcId
              }
          machineWithResume = updateTopResume machine (Just resume)
          overrides =
            EnvOverrides
              { eoAgent = Just spec.ssAgent,
                eoModels = Just spec.ssModels,
                eoRt = spec.ssRtOverrides
              }
          (store', childId) = forkEnv parentEnv overrides machineWithResume.mEnvStore
          childEnv = fromMaybe parentEnv (lookupEnv store' childId)
          genId = childEnv.envRt.rtGenerationId
          childStep = buildChatStep genId childId spec.ssInitialTurns [] mempty 0
          childFrame =
            Frame
              { frId = machineWithResume.mNextFrameId,
                frEnvId = childId,
                frStep = childStep,
                frResume = Nothing
              }
          machine' =
            machineWithResume
              { mEnvStore = store',
                mNextFrameId = machineWithResume.mNextFrameId + 1
              }
      emitOrchestrationEvent parentEnv.envRt (SubagentStarted parentFrame.frId spec)
      emitOrchestrationEvent parentEnv.envRt (FramePushed parentFrame.frId childFrame.frId)
      pure (pushFrame machine' childFrame)

pushStepsFrame :: Machine -> [ChatStep] -> IO (Either MachineError Machine)
pushStepsFrame machine [] = pure (Right machine)
pushStepsFrame machine (s : ss) = do
  case machineTopEnv machine of
    Nothing -> pure (Left (EnvNotFound 0))
    Just parentEnv -> do
      let step = case ss of
            [] -> s
            rest -> SeqSteps (s : rest)
          frame =
            Frame
              { frId = machine.mNextFrameId,
                frEnvId = parentEnv.envId,
                frStep = step,
                frResume = Nothing
              }
          machine' = machine {mNextFrameId = machine.mNextFrameId + 1}
      pure (pushFrame machine' frame)

pushDialogFrame :: Machine -> DialogSpec -> IO (Either MachineError Machine)
pushDialogFrame machine spec =
  let k summary = Done (Right (dialogSummaryToTextResult summary))
      step = RunDialog spec k
      frame =
        Frame
          { frId = machine.mNextFrameId,
            frEnvId = (topFrame machine).frEnvId,
            frStep = step,
            frResume = Nothing
          }
      machine' = machine {mNextFrameId = machine.mNextFrameId + 1}
   in pure (pushFrame machine' frame)

dialogSummaryToTextResult :: DialogSummary -> GenerateTextResult
dialogSummaryToTextResult dgs =
  GenerateTextResult
    { gtrGenerationId = nilUuid,
      gtrNewMessages = dgs.dgsTranscript,
      gtrText = dgs.dgsText,
      gtrUsage = dgs.dgsUsage
    }

handoffTurns :: Machine -> HandoffSpec -> [Turn]
handoffTurns machine spec =
  let base = spec.hsTarget
      parentTurns = currentTurnsFromMachine machine
   in case spec.hsContextMode of
        HandoffFullTranscript -> parentTurns ++ base.ssInitialTurns
        HandoffSummary summary -> UserTurn summary : base.ssInitialTurns
        HandoffWindow n ->
          let dropped = drop (max 0 (length parentTurns - n)) parentTurns
           in dropped ++ base.ssInitialTurns

currentTurnsFromMachine :: Machine -> [Turn]
currentTurnsFromMachine machine =
  case topFrame machine of
    Frame {frStep = CallModel {csCurrentTurns}} -> csCurrentTurns
    Frame {frStep = ExecTools {csCurrentTurns}} -> csCurrentTurns
    _ -> []

dialogLoop ::
  Machine ->
  EnvId ->
  EnvId ->
  Int ->
  [Turn] ->
  Usage ->
  Int ->
  IO ([Turn], Usage)
dialogLoop machine envIdA envIdB maxRounds transcript usage dialogRound
  | dialogRound >= maxRounds = pure (transcript, usage)
  | otherwise = do
      respA <- callModel machine envIdA transcript
      case respA of
        Left _err -> pure (transcript, usage)
        Right rA -> do
          let turnA = AssistantTurn rA.respText rA.respReasoning []
              uA = usage <> fromMaybe mempty rA.respUsage
              t1 = transcript ++ [turnA]
          emitOrchestrationEvent
            (fromMaybe (error "env") (machineTopEnv machine)).envRt
            (DialogTurn (topFrame machine).frId dialogRound turnA)
          respB <- callModel machine envIdB t1
          case respB of
            Left _ -> pure (t1, uA)
            Right rB -> do
              let turnB = AssistantTurn rB.respText rB.respReasoning []
                  uB = uA <> fromMaybe mempty rB.respUsage
                  t2 = t1 ++ [turnB]
              emitOrchestrationEvent
                (fromMaybe (error "env") (machineTopEnv machine)).envRt
                (DialogTurn (topFrame machine).frId dialogRound turnB)
              dialogLoop machine envIdA envIdB maxRounds t2 uB (dialogRound + 1)

lastAssistantText :: [Turn] -> Text
lastAssistantText =
  foldr
    ( \t acc -> case t of
        AssistantTurn txt _ _ -> if T.null acc then txt else acc
        _ -> acc
    )
    ""

workflowContextFromMachine :: Machine -> IO WorkflowContext
workflowContextFromMachine machine =
  case machineTopEnv machine of
    Nothing ->
      pure
        WorkflowContext
          { wcAbortSignal = Nothing,
            wcUToolRegistry = machine.mUToolRegistry,
            wcOnEvent = \_ -> pure (),
            wcHooks = noHooks,
            wcLLMHooks =
              LLMHooks
                { onLLMRequest = \_ _ -> pure (),
                  onLLMResponse = \_ _ -> pure (),
                  onLLMResponseError = \_ _ -> pure ()
                },
            wcResults = Map.empty
          }
    Just env ->
      pure
        WorkflowContext
          { wcAbortSignal = env.envRt.rtAbortSignal,
            wcUToolRegistry = machine.mUToolRegistry,
            wcOnEvent = env.envRt.rtOnEvent,
            wcHooks = env.envRt.rtHooks,
            wcLLMHooks = env.envRt.rtLLMHooks,
            wcResults = Map.empty
          }

runWorkflowNode ::
  Workflow ->
  WorkflowContext ->
  WorkflowNodeId ->
  IO WorkflowResult
runWorkflowNode wf ctx nodeId = do
  emitOrchestrationEventFromCtx ctx (WorkflowNodeStarted nodeId)
  result <- case wf of
    RunAgent inp -> runAgentNode inp ctx nodeId
    Seq wfs -> runSeqWorkflow wfs ctx nodeId
    Par wfs pol -> runParallel wfs pol ctx
    Dialog wd -> do
      machine <- dialogRootMachine ctx wd.wdSpec
      d <- runDialog machine wd.wdSpec
      case d of
        Left err ->
          pure
            WorkflowResult
              { wrNodeId = wd.wdNodeId,
                wrOutput = Left (GenerateErrorResult err [] mempty),
                wrUsage = mempty
              }
        Right summary ->
          pure
            WorkflowResult
              { wrNodeId = wd.wdNodeId,
                wrOutput = Right (dialogSummaryToTextResult summary),
                wrUsage = summary.dgsUsage
              }
    Handoff wh -> do
      machine <- handoffRootMachine ctx wh.whSpec
      _ <- runHandoff machine wh.whSpec
      runAgentNode (handoffToAgentNode wh) ctx wh.whNodeId
    Subagent ws -> do
      machine <- subagentRootMachine ctx ws.wsSpec
      sub <- runSubagent machine ws.wsSpec
      case sub of
        Left err ->
          pure
            WorkflowResult
              { wrNodeId = ws.wsNodeId,
                wrOutput = Left (GenerateErrorResult err [] mempty),
                wrUsage = mempty
              }
        Right pop ->
          pure
            WorkflowResult
              { wrNodeId = ws.wsNodeId,
                wrOutput = Right (popToTextResult pop),
                wrUsage = pop.prUsage
              }
  emitOrchestrationEventFromCtx ctx (WorkflowNodeFinished nodeId result)
  pure result

runSeqWorkflow ::
  [Workflow] ->
  WorkflowContext ->
  WorkflowNodeId ->
  IO WorkflowResult
runSeqWorkflow [] _ nodeId =
  pure
    WorkflowResult
      { wrNodeId = nodeId,
        wrOutput = Left (GenerateErrorResult GErrAborted [] mempty),
        wrUsage = mempty
      }
runSeqWorkflow wfs ctx _ =
  foldM
    ( \acc wf -> do
        let ctx' = ctx {wcResults = Map.insert acc.wrNodeId acc ctx.wcResults}
            wfNodeId = workflowNodeId wf
        runWorkflowNode wf ctx' wfNodeId
    )
    ( WorkflowResult nodeId (Left (GenerateErrorResult GErrAborted [] mempty)) mempty
    )
    wfs
  where
    nodeId = ""

runAgentNode ::
  AgentNodeInput ->
  WorkflowContext ->
  WorkflowNodeId ->
  IO WorkflowResult
runAgentNode inp ctx nodeId = do
  let turns =
        resolveWorkflowInput inp.aniInput ctx
          <> case inp.aniInput of
            WInputText t -> [UserTurn t]
            _ -> []
      rt =
        inp.aniRt
          { rtAbortSignal = inp.aniRt.rtAbortSignal <|> ctx.wcAbortSignal,
            rtOnEvent = ctx.wcOnEvent,
            rtHooks = ctx.wcHooks,
            rtLLMHooks = ctx.wcLLMHooks
          }
  out <- generateText ctx.wcUToolRegistry inp.aniAgent inp.aniModels rt turns
  pure
    WorkflowResult
      { wrNodeId = nodeId,
        wrOutput = out,
        wrUsage = either (.gerUsage) (.gtrUsage) out
      }

resolveWorkflowInput :: WorkflowInput -> WorkflowContext -> [Turn]
resolveWorkflowInput inp ctx = case inp of
  WInputTurns ts -> ts
  WInputText _ -> []
  WInputFromPrior nid ->
    case Map.lookup nid ctx.wcResults of
      Nothing -> []
      Just wr ->
        either
          (.gerPartialNewMessages)
          (.gtrNewMessages)
          wr.wrOutput

mergeWorkflowResults ::
  WorkflowNodeId ->
  MergePolicy ->
  [WorkflowResult] ->
  WorkflowContext ->
  IO WorkflowResult
mergeWorkflowResults nodeId policy results _ctx = do
  let mergedOutput = case policy of
        MergeConcat -> mergeConcat results
        MergeFirstSuccess -> mergeFirstSuccess results
        MergeWithAgent _agent -> mergeConcat results
        MergeCustom _f -> error "MergeCustom: supply merge via MergeConcat for now"
  pure
    WorkflowResult
      { wrNodeId = nodeId,
        wrOutput = mergedOutput,
        wrUsage = foldMap (.wrUsage) results
      }
  where
    mergeConcat rs =
      let texts = mapMaybe (eitherToMaybeText . (.wrOutput)) rs
       in if null texts
            then Left (GenerateErrorResult GErrAborted [] mempty)
            else
              Right
                GenerateTextResult
                  { gtrGenerationId = nilUuid,
                    gtrNewMessages = [],
                    gtrText = T.intercalate "\n" texts,
                    gtrUsage = foldMap (.wrUsage) rs
                  }
    mergeFirstSuccess rs =
      case mapMaybe (eitherToMaybe . (.wrOutput)) rs of
        [] -> Left (GenerateErrorResult GErrAborted [] mempty)
        (x : _) -> Right x

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

eitherToMaybeText :: Either GenerateErrorResult GenerateTextResult -> Maybe Text
eitherToMaybeText result = either (const Nothing) (\r -> Just r.gtrText) result

handoffToAgentNode :: WorkflowHandoff -> AgentNodeInput
handoffToAgentNode wh =
  let t = wh.whSpec.hsTarget
   in AgentNodeInput
        { aniAgent = t.ssAgent,
          aniModels = t.ssModels,
          aniRt = fromMaybe (error "handoff needs rt") t.ssRtOverrides,
          aniInput = WInputTurns t.ssInitialTurns
        }

workflowRuntime :: WorkflowContext -> UUID -> RuntimeArgs
workflowRuntime ctx genId =
  RuntimeArgs
    { rtGenerationId = genId,
      rtAbortSignal = ctx.wcAbortSignal,
      rtLLMHooks = ctx.wcLLMHooks,
      rtHooks = ctx.wcHooks,
      rtOnEvent = ctx.wcOnEvent,
      rtReadonly = True
    }

dialogRootMachine :: WorkflowContext -> DialogSpec -> IO Machine
dialogRootMachine ctx spec = do
  let rt = workflowRuntime ctx nilUuid
      env =
        ( mkEnv
            spec.dsAgentA
            spec.dsModelsA
            rt
            (\a m r t -> generateTextWithFallbacks (createGenRequestU ctx.wcUToolRegistry a r t) m)
        )
          { envId = 0
          }
      step = buildChatStep nilUuid 0 spec.dsSeedTurns [] mempty 0
  pure (initialMachine env ctx.wcUToolRegistry step defaultMachineConfig)

handoffRootMachine :: WorkflowContext -> HandoffSpec -> IO Machine
handoffRootMachine ctx spec = do
  let t = spec.hsTarget
      rt = fromMaybe (workflowRuntime ctx nilUuid) t.ssRtOverrides
      env =
        ( mkEnv
            t.ssAgent
            t.ssModels
            rt
            (\a m r t' -> generateTextWithFallbacks (createGenRequestU ctx.wcUToolRegistry a r t') m)
        )
          { envId = 0
          }
      step = buildChatStep rt.rtGenerationId 0 t.ssInitialTurns [] mempty 0
  pure (initialMachine env ctx.wcUToolRegistry step defaultMachineConfig)

subagentRootMachine :: WorkflowContext -> SubagentSpec -> IO Machine
subagentRootMachine ctx spec = do
  let rt = fromMaybe (workflowRuntime ctx nilUuid) spec.ssRtOverrides
      env =
        ( mkEnv
            spec.ssAgent
            spec.ssModels
            rt
            (\a m r t -> generateTextWithFallbacks (createGenRequestU ctx.wcUToolRegistry a r t) m)
        )
          { envId = 0
          }
      step = buildChatStep rt.rtGenerationId 0 spec.ssInitialTurns [] mempty 0
  pure (initialMachine env ctx.wcUToolRegistry step defaultMachineConfig)

workflowNodeId :: Workflow -> WorkflowNodeId
workflowNodeId = \case
  RunAgent _ -> "run-agent"
  Seq _ -> "seq"
  Par _ _ -> "par"
  Dialog wd -> wd.wdNodeId
  Handoff wh -> wh.whNodeId
  Subagent ws -> ws.wsNodeId

emitOrchestrationEventFromCtx :: WorkflowContext -> OrchestrationEventDetail -> IO ()
emitOrchestrationEventFromCtx ctx detail =
  emitOrchestrationEvent
    RuntimeArgs
      { rtGenerationId = nilUuid,
        rtAbortSignal = ctx.wcAbortSignal,
        rtLLMHooks = ctx.wcLLMHooks,
        rtHooks = ctx.wcHooks,
        rtOnEvent = ctx.wcOnEvent,
        rtReadonly = True
      }
    detail

failWithMachineError :: Machine -> IO (Either GenerateErrorResult GenerateTextResult)
failWithMachineError machine =
  failGeneration machine (GenerateErrorResult GErrAborted [] mempty)

type UToolRegistry = Map Text UTool

getResolvedUTools :: UToolRegistry -> Agent -> RuntimeArgs -> [UTool]
getResolvedUTools utoolRegistry agent rt =
  let readonly = rt.rtReadonly
      utools = mapMaybe (`Map.lookup` utoolRegistry) agent.agUTools
      readonlyUTools = if readonly then filter (\x -> x.utToolDef.toolReadonly) utools else utools
   in readonlyUTools

createGenRequestU :: UToolRegistry -> Agent -> RuntimeArgs -> [Turn] -> GenRequest
createGenRequestU utoolRegistry agent rt messages =
  let offset = windowOffset agent.agContextWindow messages
      tools = getResolvedTools agent rt
      utools = getResolvedUTools utoolRegistry agent rt
   in GenRequest
        { grSystemPrompt = agent.agSystemPrompt,
          grTools = map (\x -> x.toolDef) tools ++ map (\x -> x.utToolDef) utools,
          grMessages = drop offset messages,
          grAbortSignal = rt.rtAbortSignal,
          grLLMHooks = rt.rtLLMHooks,
          grHooks = rt.rtHooks
        }
