{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

-- | Staged stack-machine interpreter with @reduceStep@-based execution.
--
-- Each @reduceStep@ performs exactly one of:
-- (1) one LLM call, (2) one tool execution, (3) one stack transformation
-- (push\/pop\/replace\/fork), or (4) pure bookkeeping (abort check, loop exit
-- check, workflow advance).  @runStack@ loops until 'ReduceFinished' or
-- 'ReduceBlocked'.
module LLM.Agent.Generate5
  ( -- * Identifiers
    EnvId,
    FrameId,
    NodeId,
    ToolCallId,
    KontId,

    -- * Environment (stage 1+)
    Env (..),
    EnvStore (..),
    EnvOverrides (..),

    -- * Stack and configuration (stage 1+)
    Stack (..),
    StackConfig (..),
    defaultStackConfig,
    ReduceOutcome (..),
    StepKind (..),

    -- * Frame (all stages)
    Frame (..),
    FrameKind (..),
    ParentPause (..),
    ReturnKont (..),
    PendingChild (..),

    -- * Step / phase — program counter on the frame (stage 1+)
    Step (..),
    AgentPhase (..),

    -- * Agent local state (stage 1+)
    AgentLocalState (..),
    PendingToolRound (..),

    -- * Pop / transcript (stage 2+)
    PopResult (..),
    TranscriptPolicy (..),

    -- * Subagent (stage 2+)
    SubagentSpec (..),

    -- * Handoff (stage 3+)
    HandoffSpec (..),
    HandoffContext (..),

    -- * Workflow AST and runtime scheduler (stage 4+)
    Workflow (..),
    AgentNodeInput (..),
    WorkflowInput (..),
    WorkflowNodeResult (..),
    MergePolicy (..),
    WorkflowLocalState (..),
    WorkflowProg (..),
    WorkflowPc (..),
    SeqPhase (..),
    ParBranchState (..),
    ParBranchStatus (..),

    -- * Loop workflow (stage 4+)
    LoopSpec (..),
    LoopExit (..),
    LoopPredicate (..),
    LoopCarry (..),
    LoopFrameState (..),
    LoopPhase (..),
    LoopPc (..),
    WorkflowLoopView (..),
    MergeFn (..),
    defaultLoopSpec,

    -- * User gate (stage 5)
    UserGate (..),
    UserChoice (..),
    UserGateOutcome (..),

    -- * UTool and commands
    UTool (..),
    UToolRegistry,
    ToolOutcome (..),
    AgentCommand (..),
    ToolExecute,
    ToolStepResult (..),

    -- * Runtime
    StackRuntime (..),

    -- * Errors
    StackError (..),

    -- * Stack view (stepping UI)
    StackView (..),
    FrameView (..),
    NextAction (..),

    -- * Env operations
    mkEnv,
    forkEnv,
    lookupEnv,

    -- * Stack primitives
    initialStack,
    pushFrame,
    popFrame,
    replaceTopFrame,
    topFrame,
    setTopStep,
    updateTopAgent,
    topEnv,

    -- * Core interpreter
    reduceStep,
    runStack,
    resumeUser,

    -- * Workflow compilation
    compileWorkflow,

    -- * Inspection
    peekNextAction,

    -- * Public entry points
    generateText5,
    streamText5,
  )
where

import Control.Exception (SomeException, try)
import Data.Aeson (Value)
import Data.List.NonEmpty (NonEmpty (..))
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
    GenerateEventDetail (..),
    RuntimeArgs (..),
    Tool (..),
    ToolContext,
  )
import LLM.Core.Abort (AbortSignal, isAbortedMaybe)
import LLM.Core.Types
  ( ChatResponse (..),
    ToolCall (..),
    ToolDef (..),
    ToolResult (..),
    Turn (..),
  )
import LLM.Core.Usage (Usage, emptyUsage)
import LLM.Core.Utils (getToolCalls, toolResult)
import LLM.Generate.Generate
  ( generateTextWithFallbacks,
    streamTextWithFallbacks,
  )
import LLM.Generate.ModelConfig (ModelWithFallbacks)
import LLM.Generate.Types
  ( GenRequest (..),
    GenerateError (..),
    GenerateErrorResult (..),
    GenerateResult,
    GenerateTextResult (..),
    StreamChunk,
  )

-- ===========================================================================
-- Identifiers
-- ===========================================================================

type EnvId = Int

type FrameId = Int

type NodeId = Text

type ToolCallId = Text

type KontId = FrameId

-- ===========================================================================
-- UTool / commands
-- ===========================================================================

type UToolRegistry = Map Text UTool

data UTool = UTool
  { utToolDef :: ToolDef,
    utToolExec :: ToolContext -> Value -> IO ToolOutcome
  }

type ToolExecute = ToolContext -> Value -> IO ToolOutcome

data ToolOutcome
  = ToolReply Text
  | ToolCommand AgentCommand
  | ToolReplyAndCommand Text AgentCommand

data AgentCommand
  = CmdPushSubagent SubagentSpec
  | CmdHandoff HandoffSpec
  | CmdRunWorkflow Workflow
  | CmdPopWith Text
  | CmdFail GenerateError
  | CmdAwaitUser UserGate

data ToolStepResult
  = TSRReply ToolResult
  | TSRCommand AgentCommand
  | TSRReplyAndCommand ToolResult AgentCommand

-- ===========================================================================
-- StackRuntime — holds registries with function fields (not on Stack)
-- ===========================================================================

data StackRuntime = StackRuntime
  { srUTools :: UToolRegistry,
    srLoopPredicates :: Map Text LoopPredicate,
    srMergeFns :: Map Text MergeFn
  }

-- ===========================================================================
-- Environment (stage 1+)
-- ===========================================================================

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

-- ===========================================================================
-- Stack (stage 1+)
-- ===========================================================================

data Stack = Stack
  { stEnvStore :: EnvStore,
    stFrames :: NonEmpty Frame,
    stNextFrameId :: FrameId,
    stDepth :: Int,
    stGlobalUsage :: Usage,
    stRootGenerationId :: UUID,
    stConfig :: StackConfig,
    stNodeResults :: Map NodeId WorkflowNodeResult,
    stLastStep :: Maybe StepKind,
    stTotalSteps :: Int
  }

data StackConfig = StackConfig
  { scMaxStackDepth :: Int,
    scMaxToolRoundsPerFrame :: Maybe Int,
    scSharedAbortSignal :: Maybe AbortSignal,
    scMaxTotalSteps :: Maybe Int,
    scMaxTotalUsage :: Maybe Usage
  }

defaultStackConfig :: StackConfig
defaultStackConfig =
  StackConfig
    { scMaxStackDepth = 32,
      scMaxToolRoundsPerFrame = Nothing,
      scSharedAbortSignal = Nothing,
      scMaxTotalSteps = Nothing,
      scMaxTotalUsage = Nothing
    }

data StackError
  = SEStackOverflow
  | SEEmptyStack
  | SEEnvNotFound EnvId
  | SEFrameNotFound FrameId
  | SEToolNotFound Text
  deriving stock (Show, Eq)

-- | Result of one @reduceStep@ invocation.
data ReduceOutcome a
  = Stepped a
  | ReduceBlocked a UserGate
  | ReduceFinished (Either GenerateErrorResult GenerateTextResult)
  deriving stock (Show, Functor)

-- | Classification of the last reduction (for inspection).
data StepKind
  = StepKindPure Text
  | StepKindLLM FrameId
  | StepKindTool FrameId ToolCallId
  | StepKindStackOp Text
  | StepKindAwaitUser
  deriving stock (Show, Eq)

-- ===========================================================================
-- Frame (stages 1–5)
-- ===========================================================================

data Frame = Frame
  { frId :: FrameId,
    frKind :: FrameKind,
    frEnvId :: EnvId,
    frStep :: Step,
    frPause :: Maybe ParentPause,
    frReturn :: ReturnKont,
    frTranscriptPolicy :: TranscriptPolicy
  }

data FrameKind
  = FAgent AgentLocalState
  | FWorkflow WorkflowLocalState
  | FLoop LoopFrameState

data ParentPause
  = PausedForSubagent
      { ppsToolCallId :: ToolCallId,
        ppsRound :: PendingToolRound
      }
  | PausedForWorkflowNode
      { ppsNodeId :: NodeId,
        ppsPendingChild :: PendingChild
      }
  deriving stock (Show)

data ReturnKont
  = KNone
  | KResumeToolRound
      { kParentFrameId :: FrameId,
        kToolCallId :: ToolCallId
      }
  | KWorkflowNode
      { kSchedulerFrameId :: FrameId,
        kNodeId :: NodeId
      }
  | KLoopIteration
      { kLoopFrameId :: FrameId,
        kIteration :: Int
      }
  deriving stock (Show, Eq)

data PendingChild = PendingChild
  { pcParentFrameId :: FrameId,
    pcToolCallId :: Maybe ToolCallId,
    pcNodeId :: Maybe NodeId,
    pcKont :: ReturnKont
  }
  deriving stock (Show)

-- ===========================================================================
-- Step — stored on the frame (stage 1+)
-- ===========================================================================

data Step
  = -- Stage 1
    StepCheckAbort
  | StepCallModel
  | StepRunTool ToolCallId
  | StepApplyToolResults
  | -- Stage 2
    StepPushSubagent SubagentSpec
  | StepPopChild PopResult
  | -- Stage 3
    StepHandoff HandoffSpec
  | -- Stage 4 (workflow scheduler)
    StepWfStartNode NodeId
  | StepWfChildDone NodeId PopResult
  | StepWfMerge MergePolicy
  | StepWfAdvance
  | StepLoopCheck
  | StepLoopStartIteration
  | StepLoopEndIteration
  | -- Stage 5
    StepAwaitUser UserGate
  | StepFinish

data AgentPhase
  = PhaseModel
  | PhaseTool Int
  | PhaseApplyTools
  | PhaseDone
  | PhaseAwaitUser UserGate
  deriving stock (Show, Eq)

-- ===========================================================================
-- Agent local state (stage 1+)
-- ===========================================================================

data AgentLocalState = AgentLocalState
  { alsGenerationId :: UUID,
    alsCurrentTurns :: [Turn],
    alsAccTurns :: [Turn],
    alsUsage :: Usage,
    alsLoopCount :: Int,
    alsPendingTools :: Maybe PendingToolRound
  }
  deriving stock (Show)

data PendingToolRound = PendingToolRound
  { ptrAssistant :: Turn,
    ptrCalls :: [ToolCall],
    ptrNextIndex :: Int,
    ptrResults :: [ToolResult]
  }
  deriving stock (Show)

-- ===========================================================================
-- Pop / transcript policy (stage 2+)
-- ===========================================================================

data PopResult = PopResult
  { popSummary :: Text,
    popStructured :: Maybe Value,
    popUsage :: Usage,
    popNewTurns :: [Turn]
  }
  deriving stock (Show)

data TranscriptPolicy
  = TranscriptSummaryOnly
  | TranscriptIsolated
  | TranscriptShared
  deriving stock (Show, Eq)

-- ===========================================================================
-- Subagent (stage 2+)
-- ===========================================================================

data SubagentSpec = SubagentSpec
  { ssAgent :: Agent,
    ssModels :: ModelWithFallbacks,
    ssRtOverrides :: Maybe RuntimeArgs,
    ssInitialTurns :: [Turn],
    ssMaxToolRounds :: Maybe Int,
    ssTranscriptPolicy :: TranscriptPolicy,
    ssSummaryPrompt :: Maybe Text
  }

-- ===========================================================================
-- Handoff (stage 3+)
-- ===========================================================================

data HandoffSpec = HandoffSpec
  { hsTarget :: SubagentSpec,
    hsContext :: HandoffContext,
    hsReplaceStack :: Bool
  }

data HandoffContext
  = HandoffFullTranscript
  | HandoffSummary Text
  | HandoffWindow Int
  deriving stock (Show, Eq)

-- ===========================================================================
-- Workflow AST (stage 4+)
-- ===========================================================================

data Workflow
  = WRunAgent AgentNodeInput
  | WSeq [Workflow]
  | WPar [Workflow] MergePolicy
  | WLoop LoopSpec
  | WHandoff HandoffSpec NodeId
  | WSubagent SubagentSpec NodeId

data AgentNodeInput = AgentNodeInput
  { aniAgent :: Agent,
    aniModels :: ModelWithFallbacks,
    aniRt :: RuntimeArgs,
    aniInput :: WorkflowInput,
    aniNodeId :: NodeId
  }

data WorkflowInput
  = WInputTurns [Turn]
  | WInputText Text
  | WInputFromPrior NodeId
  deriving stock (Show, Eq)

data WorkflowNodeResult = WorkflowNodeResult
  { wnrNodeId :: NodeId,
    wnrOutput :: Either GenerateErrorResult GenerateTextResult,
    wnrUsage :: Usage
  }
  deriving stock (Show)

data MergePolicy
  = MergeConcat
  | MergeFirstSuccess
  | MergeWithAgent Agent
  | MergeCustomNamed Text

newtype MergeFn = MergeFn
  { runMergeFn :: [WorkflowNodeResult] -> IO WorkflowNodeResult
  }

-- ===========================================================================
-- Workflow scheduler state (stage 4+)
-- ===========================================================================

data WorkflowLocalState = WorkflowLocalState
  { wlsProg :: WorkflowProg,
    wlsPc :: WorkflowPc,
    wlsNodeResults :: Map NodeId WorkflowNodeResult,
    wlsFinalResult :: Maybe (Either GenerateErrorResult GenerateTextResult)
  }

data WorkflowProg
  = WProgSeq [(NodeId, Workflow)]
  | WProgPar [(NodeId, Workflow)] MergePolicy
  | WProgLoop LoopSpec

instance Show WorkflowProg where
  show (WProgSeq nodes) = "WProgSeq[" <> show (map fst nodes) <> "]"
  show (WProgPar nodes _) = "WProgPar[" <> show (map fst nodes) <> "]"
  show (WProgLoop spec) = "WProgLoop(" <> show spec <> ")"

data WorkflowPc
  = PcSeq
      { pcSeqIndex :: Int,
        pcSeqPhase :: SeqPhase
      }
  | PcPar ParBranchState
  | PcLoop LoopPc
  deriving stock (Show)

data SeqPhase
  = SeqRunning NodeId
  | SeqBetween
  deriving stock (Show, Eq)

data ParBranchState = ParBranchState
  { pbsNodeIds :: [NodeId],
    pbsCurrent :: Int,
    pbsResults :: Map NodeId WorkflowNodeResult,
    pbsStatuses :: Map NodeId ParBranchStatus
  }
  deriving stock (Show)

data ParBranchStatus
  = ParPending
  | ParRunning
  | ParDone
  deriving stock (Show, Eq)

-- ===========================================================================
-- Loop workflow — termination and carry (stage 4+)
-- ===========================================================================

data LoopSpec = LoopSpec
  { loopBody :: Workflow,
    loopNodeId :: NodeId,
    loopMaxIterations :: Int,
    loopExit :: LoopExit,
    loopCarry :: LoopCarry
  }

-- Manual Show: LoopSpec contains Workflow which contains Agent (functions).
instance Show LoopSpec where
  show ls = "LoopSpec{nodeId=" <> T.unpack ls.loopNodeId
    <> ",maxIter=" <> show ls.loopMaxIterations <> "}"

defaultLoopSpec :: Workflow -> NodeId -> LoopSpec
defaultLoopSpec body nodeId =
  LoopSpec
    { loopBody = body,
      loopNodeId = nodeId,
      loopMaxIterations = 3,
      loopExit = LoopExitOnMaxOnly,
      loopCarry = LoopCarryFromPrior nodeId
    }

data LoopExit
  = LoopExitOnMaxOnly
  | LoopExitOnSuccess
  | LoopExitOnError GenerateError
  | LoopExitWhenNamed Text
  deriving stock (Show, Eq)

newtype LoopPredicate = LoopPredicate
  { runLoopPredicate :: WorkflowLoopView -> Bool
  }

data LoopCarry
  = LoopCarryNone
  | LoopCarryFromPrior NodeId
  | LoopCarryAcc NodeId
  deriving stock (Show, Eq)

data LoopFrameState = LoopFrameState
  { lfsSpec :: LoopSpec,
    lfsIteration :: Int,
    lfsLastResult :: Maybe WorkflowNodeResult,
    lfsPhase :: LoopPhase,
    lfsFinalResult :: Maybe (Either GenerateErrorResult GenerateTextResult)
  }

data LoopPhase
  = LoopPhaseCheck
  | LoopPhaseRunningBody
  | LoopPhaseBetweenIterations
  | LoopPhaseDone
  deriving stock (Show, Eq)

data LoopPc = LoopPc
  { lpIteration :: Int,
    lpInner :: WorkflowPc,
    lpPhase :: LoopPhase
  }
  deriving stock (Show)

data WorkflowLoopView = WorkflowLoopView
  { wlvIteration :: Int,
    wlvMaxIterations :: Int,
    wlvLastResult :: Maybe WorkflowNodeResult,
    wlvNodeResults :: Map NodeId WorkflowNodeResult,
    wlvCarry :: LoopCarry
  }
  deriving stock (Show)

-- ===========================================================================
-- User confirmation (stage 5)
-- ===========================================================================

data UserGate = UserGate
  { ugPrompt :: Text,
    ugPayload :: Maybe Value,
    ugGateId :: Text
  }
  deriving stock (Show, Eq)

data UserChoice
  = UserContinue Text (Maybe Value)
  | UserDiscard
  deriving stock (Show, Eq)

data UserGateOutcome
  = GateContinue UserChoice
  | GateTimeout
  deriving stock (Show, Eq)

-- ===========================================================================
-- StackView (stepping UI)
-- ===========================================================================

data StackView = StackView
  { svDepth :: Int,
    svFrames :: [FrameView],
    svNext :: NextAction
  }
  deriving stock (Show)

data FrameView = FrameView
  { fvFrameId :: FrameId,
    fvKind :: Text,
    fvStep :: Text,
    fvEnvId :: EnvId
  }
  deriving stock (Show)

data NextAction
  = NextLLM
  | NextTool Text
  | NextPure Text
  | NextBlocked UserGate
  | NextDone
  deriving stock (Show)

-- ###########################################################################
-- ###########################################################################
--
--   IMPLEMENTATION
--
-- ###########################################################################
-- ###########################################################################

-- ===========================================================================
-- Env operations
-- ===========================================================================

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
lookupEnv store eid = Map.lookup eid store.esMap

-- ===========================================================================
-- Stack primitives
-- ===========================================================================

initialStack :: Env -> StackConfig -> [Turn] -> Stack
initialStack env config turns =
  let store =
        EnvStore
          { esNextId = env.envId + 1,
            esMap = Map.singleton env.envId env
          }
      als =
        AgentLocalState
          { alsGenerationId = env.envRt.rtGenerationId,
            alsCurrentTurns = turns,
            alsAccTurns = [],
            alsUsage = emptyUsage,
            alsLoopCount = 0,
            alsPendingTools = Nothing
          }
      frame0 =
        Frame
          { frId = 0,
            frKind = FAgent als,
            frEnvId = env.envId,
            frStep = StepCheckAbort,
            frPause = Nothing,
            frReturn = KNone,
            frTranscriptPolicy = TranscriptSummaryOnly
          }
   in Stack
        { stEnvStore = store,
          stFrames = frame0 :| [],
          stNextFrameId = 1,
          stDepth = 1,
          stGlobalUsage = mempty,
          stRootGenerationId = env.envRt.rtGenerationId,
          stConfig = config,
          stNodeResults = Map.empty,
          stLastStep = Nothing,
          stTotalSteps = 0
        }

pushFrame :: Stack -> Frame -> Either StackError Stack
pushFrame st frame
  | st.stDepth >= st.stConfig.scMaxStackDepth = Left SEStackOverflow
  | otherwise =
      Right
        st
          { stFrames = frame NE.<| st.stFrames,
            stDepth = st.stDepth + 1
          }

popFrame :: Stack -> PopResult -> Either StackError Stack
popFrame st pop =
  case NE.uncons st.stFrames of
    (_, Nothing) -> Left SEEmptyStack
    (_, Just rest) ->
      Right
        st
          { stFrames = rest,
            stDepth = st.stDepth - 1,
            stGlobalUsage = st.stGlobalUsage <> pop.popUsage
          }

replaceTopFrame :: Stack -> Frame -> Stack
replaceTopFrame st frame =
  case NE.uncons st.stFrames of
    (_, Nothing) -> st {stFrames = frame :| []}
    (_, Just rest) -> st {stFrames = frame NE.<| rest}

topFrame :: Stack -> Frame
topFrame st = NE.head st.stFrames

setTopStep :: Stack -> Step -> Stack
setTopStep st step =
  let fr = topFrame st
   in replaceTopFrame st (fr {frStep = step})

updateTopAgent :: Stack -> (AgentLocalState -> AgentLocalState) -> Stack
updateTopAgent st f =
  let fr = topFrame st
      kind' = case fr.frKind of
        FAgent als -> FAgent (f als)
        other -> other
   in replaceTopFrame st (fr {frKind = kind'})

updateTopWorkflow :: Stack -> (WorkflowLocalState -> WorkflowLocalState) -> Stack
updateTopWorkflow st f =
  let fr = topFrame st
      kind' = case fr.frKind of
        FWorkflow wls -> FWorkflow (f wls)
        other -> other
   in replaceTopFrame st (fr {frKind = kind'})

updateTopLoop :: Stack -> (LoopFrameState -> LoopFrameState) -> Stack
updateTopLoop st f =
  let fr = topFrame st
      kind' = case fr.frKind of
        FLoop lfs -> FLoop (f lfs)
        other -> other
   in replaceTopFrame st (fr {frKind = kind'})

topEnv :: Stack -> Maybe Env
topEnv st = lookupEnv st.stEnvStore (topFrame st).frEnvId

setTopPause :: Stack -> Maybe ParentPause -> Stack
setTopPause st pause =
  let fr = topFrame st
   in replaceTopFrame st (fr {frPause = pause})

-- ===========================================================================
-- Core interpreter
-- ===========================================================================

reduceStep :: StackRuntime -> Stack -> IO (ReduceOutcome Stack)
reduceStep rt st = do
  case tickGlobalLimits st of
    Left errResult -> pure (ReduceFinished (Left errResult))
    Right st1 -> do
      aborted <- isAbortedForStack st1
      if aborted
        then pure (ReduceFinished (Left (GenerateErrorResult GErrAborted [] mempty)))
        else do
          let fr = topFrame st1
              st2 = st1 {stTotalSteps = st1.stTotalSteps + 1}
          outcome <- reduceFrame rt st2 fr
          pure (fmap (\s -> s {stLastStep = Just (classifyStep fr)}) outcome)

runStack :: StackRuntime -> Stack -> IO (ReduceOutcome Stack)
runStack rt st0 = go st0
  where
    go st = do
      outcome <- reduceStep rt st
      case outcome of
        Stepped st' -> go st'
        blocked@(ReduceBlocked {}) -> pure blocked
        done@(ReduceFinished {}) -> pure done

resumeUser :: StackRuntime -> Stack -> UserGateOutcome -> IO (ReduceOutcome Stack)
resumeUser rt st outcome = case (topFrame st).frStep of
  StepAwaitUser _gate -> case outcome of
    GateContinue (UserContinue txt _val) ->
      let st' = updateTopAgent st (\als -> als {alsCurrentTurns = als.alsCurrentTurns ++ [UserTurn txt]})
          st'' = setTopStep st' StepCallModel
       in reduceStep rt st''
    GateContinue UserDiscard ->
      let st' = setTopStep st StepFinish
       in reduceStep rt st'
    GateTimeout ->
      pure (ReduceFinished (Left (GenerateErrorResult GErrAborted [] mempty)))
  _ -> reduceStep rt st

-- ===========================================================================
-- Frame dispatch
-- ===========================================================================

reduceFrame :: StackRuntime -> Stack -> Frame -> IO (ReduceOutcome Stack)
reduceFrame rt st fr = case fr.frKind of
  FAgent als -> reduceAgentStep rt st fr als
  FWorkflow wls -> reduceWorkflowStep rt st fr wls
  FLoop lfs -> reduceLoopStep rt st fr lfs

-- ===========================================================================
-- Agent step handlers (stage 1+)
-- ===========================================================================

reduceAgentStep :: StackRuntime -> Stack -> Frame -> AgentLocalState -> IO (ReduceOutcome Stack)
reduceAgentStep rt st fr als = case fr.frStep of
  StepCheckAbort -> stepCheckAbort st fr als
  StepCallModel -> stepCallModel rt st fr als
  StepRunTool tcId -> stepRunTool rt st fr als tcId
  StepApplyToolResults -> stepApplyToolResults st fr als
  StepPushSubagent spec -> stepPushSubagent rt st fr als spec
  StepPopChild pop -> stepPopChild rt st fr pop
  StepHandoff spec -> stepHandoff st fr als spec
  StepAwaitUser gate -> pure (ReduceBlocked st gate)
  StepFinish -> stepFinish st fr
  StepWfStartNode nid -> stepWfStartNode rt st fr nid
  StepWfChildDone nid pop -> stepWfChildDone st fr nid pop
  StepWfMerge pol -> stepWfMerge rt st fr pol
  StepWfAdvance -> stepWfAdvanceFromAgent st fr als
  StepLoopCheck -> stepLoopCheck rt st fr
  StepLoopStartIteration -> stepLoopStartIteration rt st fr
  StepLoopEndIteration -> stepLoopEndIteration st fr

-- ---------------------------------------------------------------------------
-- StepCheckAbort
-- ---------------------------------------------------------------------------

stepCheckAbort :: Stack -> Frame -> AgentLocalState -> IO (ReduceOutcome Stack)
stepCheckAbort st _fr als = do
  aborted <- isAbortedForStack st
  if aborted
    then failGeneration st als.alsAccTurns als.alsUsage GErrAborted
    else pure (Stepped (setTopStep st StepCallModel))

-- ---------------------------------------------------------------------------
-- StepCallModel
-- ---------------------------------------------------------------------------

stepCallModel :: StackRuntime -> Stack -> Frame -> AgentLocalState -> IO (ReduceOutcome Stack)
stepCallModel _rt st fr als = do
  checkMaxRounds st als >>= \case
    Just errResult -> pure (ReduceFinished (Left errResult))
    Nothing ->
      case lookupEnv st.stEnvStore fr.frEnvId of
        Nothing -> failGeneration st als.alsAccTurns als.alsUsage GErrAllModelsFailed
        Just env -> do
          result <- env.envCall env.envAgent env.envModels env.envRt als.alsCurrentTurns
          case result of
            Left err -> failGeneration st als.alsAccTurns als.alsUsage err
            Right resp -> do
              let toolCalls = getToolCalls resp
                  roundUsage = fromMaybe emptyUsage resp.respUsage
                  newUsage = als.alsUsage <> roundUsage
              case toolCalls of
                [] ->
                  let finalTurn = AssistantTurn resp.respText resp.respReasoning []
                      finalAcc = als.alsAccTurns ++ [finalTurn]
                      als' = als {alsAccTurns = finalAcc, alsUsage = newUsage}
                      st' = updateTopAgent st (const als')
                      st'' = setTopStep st' StepFinish
                   in pure (Stepped st'')
                _ ->
                  let assistantTurn = AssistantTurn resp.respText resp.respReasoning toolCalls
                      pending =
                        PendingToolRound
                          { ptrAssistant = assistantTurn,
                            ptrCalls = toolCalls,
                            ptrNextIndex = 0,
                            ptrResults = []
                          }
                      als' = als {alsPendingTools = Just pending, alsUsage = newUsage}
                      st' = updateTopAgent st (const als')
                   in do
                        emitEventForFrame st' fr (MessageCreated assistantTurn)
                        emitEventForFrame st' fr (ToolRoundStarted als.alsLoopCount)
                        let firstTcId = (head toolCalls).tcId
                        pure (Stepped (setTopStep st' (StepRunTool firstTcId)))

-- ---------------------------------------------------------------------------
-- StepRunTool — one tool per step
-- ---------------------------------------------------------------------------

stepRunTool :: StackRuntime -> Stack -> Frame -> AgentLocalState -> ToolCallId -> IO (ReduceOutcome Stack)
stepRunTool rt st fr als tcId =
  case als.alsPendingTools of
    Nothing -> failGeneration st als.alsAccTurns als.alsUsage GErrAborted
    Just pending ->
      case findCallById pending.ptrCalls tcId of
        Nothing -> failGeneration st als.alsAccTurns als.alsUsage GErrAborted
        Just tc -> do
          result <- executeOneTool rt st fr als tc
          case result of
            Left err -> failGeneration st als.alsAccTurns als.alsUsage err
            Right (TSRReply tr) ->
              let pending' =
                    pending
                      { ptrNextIndex = pending.ptrNextIndex + 1,
                        ptrResults = pending.ptrResults ++ [tr]
                      }
                  als' = als {alsPendingTools = Just pending'}
                  st' = updateTopAgent st (const als')
               in case nextToolCall pending' of
                    Just nextTc -> pure (Stepped (setTopStep st' (StepRunTool nextTc.tcId)))
                    Nothing -> pure (Stepped (setTopStep st' StepApplyToolResults))
            Right (TSRCommand cmd) -> applyAgentCommand rt st fr als pending cmd
            Right (TSRReplyAndCommand tr cmd) ->
              let pending' =
                    pending
                      { ptrNextIndex = pending.ptrNextIndex + 1,
                        ptrResults = pending.ptrResults ++ [tr]
                      }
                  als' = als {alsPendingTools = Just pending'}
                  st' = updateTopAgent st (const als')
               in applyAgentCommand rt st' fr als' pending' cmd

-- ---------------------------------------------------------------------------
-- StepApplyToolResults
-- ---------------------------------------------------------------------------

stepApplyToolResults :: Stack -> Frame -> AgentLocalState -> IO (ReduceOutcome Stack)
stepApplyToolResults st fr als =
  case als.alsPendingTools of
    Nothing -> pure (Stepped (setTopStep st StepCheckAbort))
    Just pending ->
      let assistantTurn = pending.ptrAssistant
          toolTurn = ToolTurn pending.ptrResults
          turnsToAdd = [assistantTurn, toolTurn]
          als' =
            als
              { alsCurrentTurns = als.alsCurrentTurns ++ turnsToAdd,
                alsAccTurns = als.alsAccTurns ++ turnsToAdd,
                alsLoopCount = als.alsLoopCount + 1,
                alsPendingTools = Nothing
              }
          st' = updateTopAgent st (const als')
       in do
            emitEventForFrame st' fr (MessageCreated toolTurn)
            emitEventForFrame st' fr (ToolRoundFinished als.alsLoopCount)
            pure (Stepped (setTopStep st' StepCheckAbort))

-- ---------------------------------------------------------------------------
-- StepFinish
-- ---------------------------------------------------------------------------

stepFinish :: Stack -> Frame -> IO (ReduceOutcome Stack)
stepFinish st fr
  | st.stDepth <= 1 = do
      let result = frameToResult fr
      case result of
        Left err -> do
          emitEventForFrame st fr (GenerationFailed err.gerError err)
          pure (ReduceFinished (Left err))
        Right success -> do
          let finalTurn = case reverse success.gtrNewMessages of
                (t : _) -> t
                [] -> AssistantTurn success.gtrText Nothing []
          emitEventForFrame st fr (MessageFinalized finalTurn)
          emitEventForFrame st fr (GenerationFinished success)
          pure (ReduceFinished (Right success))
  | otherwise =
      let result = frameToResult fr
          pop = buildPopResult fr.frTranscriptPolicy result
          st' = setTopStep st (StepPopChild pop)
       in pure (Stepped st')

-- ---------------------------------------------------------------------------
-- StepPopChild
-- ---------------------------------------------------------------------------

stepPopChild :: StackRuntime -> Stack -> Frame -> PopResult -> IO (ReduceOutcome Stack)
stepPopChild _rt st fr pop = do
  let returnKont = fr.frReturn
  case popFrame st pop of
    Left _err ->
      pure (ReduceFinished (Left (GenerateErrorResult GErrAborted [] mempty)))
    Right st' ->
      case returnKont of
        KNone ->
          pure (Stepped st')
        KResumeToolRound {kToolCallId} ->
          pure (Stepped (resumeParentToolRound st' pop kToolCallId))
        KWorkflowNode {kNodeId} ->
          pure (Stepped (setTopStep st' (StepWfChildDone kNodeId pop)))
        KLoopIteration {} -> do
          let wnr =
                WorkflowNodeResult
                  { wnrNodeId = loopNodeIdFromFrame st' (topFrame st'),
                    wnrOutput = popToOutput pop,
                    wnrUsage = pop.popUsage
                  }
              st'' = updateTopLoop st' (\lfs -> lfs {lfsLastResult = Just wnr})
              st''' = setTopStep st'' StepLoopEndIteration
          pure (Stepped st''')

-- ---------------------------------------------------------------------------
-- StepPushSubagent
-- ---------------------------------------------------------------------------

stepPushSubagent :: StackRuntime -> Stack -> Frame -> AgentLocalState -> SubagentSpec -> IO (ReduceOutcome Stack)
stepPushSubagent _rt st fr _als spec =
  case topEnv st of
    Nothing -> failGeneration st [] mempty GErrAborted
    Just parentEnv -> do
      let overrides =
            EnvOverrides
              { eoAgent = Just spec.ssAgent,
                eoModels = Just spec.ssModels,
                eoRt = spec.ssRtOverrides
              }
          (store', childId) = forkEnv parentEnv overrides st.stEnvStore
          childEnv = fromMaybe parentEnv (lookupEnv store' childId)
          genId = childEnv.envRt.rtGenerationId
          childAls =
            AgentLocalState
              { alsGenerationId = genId,
                alsCurrentTurns = spec.ssInitialTurns,
                alsAccTurns = [],
                alsUsage = emptyUsage,
                alsLoopCount = 0,
                alsPendingTools = Nothing
              }
          childFrame =
            Frame
              { frId = st.stNextFrameId,
                frKind = FAgent childAls,
                frEnvId = childId,
                frStep = StepCheckAbort,
                frPause = Nothing,
                frReturn =
                  KResumeToolRound
                    { kParentFrameId = fr.frId,
                      kToolCallId = maybe "" (.ppsToolCallId) fr.frPause
                    },
                frTranscriptPolicy = spec.ssTranscriptPolicy
              }
          st' = st {stEnvStore = store', stNextFrameId = st.stNextFrameId + 1}
      case pushFrame st' childFrame of
        Left _err -> failGeneration st [] mempty GErrAborted
        Right st'' -> pure (Stepped st'')

-- ---------------------------------------------------------------------------
-- StepHandoff
-- ---------------------------------------------------------------------------

stepHandoff :: Stack -> Frame -> AgentLocalState -> HandoffSpec -> IO (ReduceOutcome Stack)
stepHandoff st _fr als spec =
  case topEnv st of
    Nothing -> failGeneration st als.alsAccTurns als.alsUsage GErrAborted
    Just parentEnv -> do
      let target = spec.hsTarget
          turns = buildHandoffTurns als spec
          overrides =
            EnvOverrides
              { eoAgent = Just target.ssAgent,
                eoModels = Just target.ssModels,
                eoRt = target.ssRtOverrides
              }
          (store', childId) = forkEnv parentEnv overrides st.stEnvStore
          childEnv = fromMaybe parentEnv (lookupEnv store' childId)
          genId = childEnv.envRt.rtGenerationId
          childAls =
            AgentLocalState
              { alsGenerationId = genId,
                alsCurrentTurns = turns,
                alsAccTurns = [],
                alsUsage = emptyUsage,
                alsLoopCount = 0,
                alsPendingTools = Nothing
              }
          newFrame =
            Frame
              { frId = st.stNextFrameId,
                frKind = FAgent childAls,
                frEnvId = childId,
                frStep = StepCheckAbort,
                frPause = Nothing,
                frReturn = if spec.hsReplaceStack then (topFrame st).frReturn else KNone,
                frTranscriptPolicy = target.ssTranscriptPolicy
              }
          st' = st {stEnvStore = store', stNextFrameId = st.stNextFrameId + 1}
      if spec.hsReplaceStack
        then pure (Stepped (replaceTopFrame st' newFrame))
        else case pushFrame st' newFrame of
          Left _err -> failGeneration st als.alsAccTurns als.alsUsage GErrAborted
          Right st'' -> pure (Stepped st'')

-- Workflow-related steps forwarded from agent context should not happen, but
-- handle them defensively.
stepWfAdvanceFromAgent :: Stack -> Frame -> AgentLocalState -> IO (ReduceOutcome Stack)
stepWfAdvanceFromAgent st _fr als =
  failGeneration st als.alsAccTurns als.alsUsage GErrAborted

-- ===========================================================================
-- Workflow step handlers (stage 4)
-- ===========================================================================

reduceWorkflowStep :: StackRuntime -> Stack -> Frame -> WorkflowLocalState -> IO (ReduceOutcome Stack)
reduceWorkflowStep rt st fr wls = case fr.frStep of
  StepWfAdvance -> stepWfAdvance rt st fr wls
  StepWfStartNode nid -> stepWfStartNode rt st fr nid
  StepWfChildDone nid pop -> stepWfChildDone st fr nid pop
  StepWfMerge pol -> stepWfMerge rt st fr pol
  StepFinish -> stepFinish st fr
  StepPopChild pop -> stepPopChild rt st fr pop
  _ -> pure (Stepped st)

-- ---------------------------------------------------------------------------
-- StepWfAdvance
-- ---------------------------------------------------------------------------

stepWfAdvance :: StackRuntime -> Stack -> Frame -> WorkflowLocalState -> IO (ReduceOutcome Stack)
stepWfAdvance _rt st _fr wls = case wls.wlsPc of
  PcSeq {pcSeqIndex} -> case wls.wlsProg of
    WProgSeq nodes
      | pcSeqIndex < length nodes ->
          let (nid, _wf) = nodes !! pcSeqIndex
              wls' = wls {wlsPc = PcSeq pcSeqIndex (SeqRunning nid)}
              st' = updateTopWorkflow st (const wls')
           in pure (Stepped (setTopStep st' (StepWfStartNode nid)))
      | otherwise ->
          let lastResult = case nodes of
                [] -> Left (GenerateErrorResult GErrAborted [] mempty)
                _ ->
                  case Map.lookup (fst (last nodes)) wls.wlsNodeResults of
                    Just wnr -> wnr.wnrOutput
                    Nothing -> Left (GenerateErrorResult GErrAborted [] mempty)
              wls' = wls {wlsFinalResult = Just lastResult}
              st' = updateTopWorkflow st (const wls')
           in pure (Stepped (setTopStep st' StepFinish))
    _ -> pure (Stepped st)
  PcPar pbs ->
    if pbs.pbsCurrent < length pbs.pbsNodeIds
      then
        let nid = pbs.pbsNodeIds !! pbs.pbsCurrent
            pbs' = pbs {pbsStatuses = Map.insert nid ParRunning pbs.pbsStatuses}
            wls' = wls {wlsPc = PcPar pbs'}
            st' = updateTopWorkflow st (const wls')
         in pure (Stepped (setTopStep st' (StepWfStartNode nid)))
      else case wls.wlsProg of
        WProgPar _ pol ->
          pure (Stepped (setTopStep st (StepWfMerge pol)))
        _ -> pure (Stepped (setTopStep st StepFinish))
  PcLoop _lpc ->
    pure (Stepped st)

-- ---------------------------------------------------------------------------
-- StepWfStartNode — push child frame for a workflow node
-- ---------------------------------------------------------------------------

stepWfStartNode :: StackRuntime -> Stack -> Frame -> NodeId -> IO (ReduceOutcome Stack)
stepWfStartNode rt st fr nid = do
  let mWorkflow = lookupWorkflowNode fr nid
  case mWorkflow of
    Nothing -> failGeneration st [] mempty GErrAborted
    Just wf -> startWorkflowChild rt st fr nid wf

startWorkflowChild :: StackRuntime -> Stack -> Frame -> NodeId -> Workflow -> IO (ReduceOutcome Stack)
startWorkflowChild _rt st fr nid wf =
  case topEnv st of
    Nothing -> failGeneration st [] mempty GErrAborted
    Just parentEnv -> case wf of
      WRunAgent inp -> do
        let turns = resolveWorkflowInput st inp.aniInput
            overrides = EnvOverrides (Just inp.aniAgent) (Just inp.aniModels) (Just inp.aniRt)
            (store', childId) = forkEnv parentEnv overrides st.stEnvStore
            childEnv = fromMaybe parentEnv (lookupEnv store' childId)
            genId = childEnv.envRt.rtGenerationId
            childAls =
              AgentLocalState
                { alsGenerationId = genId,
                  alsCurrentTurns = turns,
                  alsAccTurns = [],
                  alsUsage = emptyUsage,
                  alsLoopCount = 0,
                  alsPendingTools = Nothing
                }
            childFrame =
              Frame
                { frId = st.stNextFrameId,
                  frKind = FAgent childAls,
                  frEnvId = childId,
                  frStep = StepCheckAbort,
                  frPause = Nothing,
                  frReturn = KWorkflowNode fr.frId nid,
                  frTranscriptPolicy = TranscriptSummaryOnly
                }
            pause = PausedForWorkflowNode nid (PendingChild fr.frId Nothing (Just nid) (KWorkflowNode fr.frId nid))
            st' =
              st
                { stEnvStore = store',
                  stNextFrameId = st.stNextFrameId + 1
                }
            st'' = setTopPause st' (Just pause)
        case pushFrame st'' childFrame of
          Left _err -> failGeneration st [] mempty GErrAborted
          Right st''' -> pure (Stepped st''')
      WSubagent spec _snid -> do
        let overrides = EnvOverrides (Just spec.ssAgent) (Just spec.ssModels) (spec.ssRtOverrides)
            (store', childId) = forkEnv parentEnv overrides st.stEnvStore
            childEnv = fromMaybe parentEnv (lookupEnv store' childId)
            genId = childEnv.envRt.rtGenerationId
            childAls =
              AgentLocalState
                { alsGenerationId = genId,
                  alsCurrentTurns = spec.ssInitialTurns,
                  alsAccTurns = [],
                  alsUsage = emptyUsage,
                  alsLoopCount = 0,
                  alsPendingTools = Nothing
                }
            childFrame =
              Frame
                { frId = st.stNextFrameId,
                  frKind = FAgent childAls,
                  frEnvId = childId,
                  frStep = StepCheckAbort,
                  frPause = Nothing,
                  frReturn = KWorkflowNode fr.frId nid,
                  frTranscriptPolicy = spec.ssTranscriptPolicy
                }
            pause = PausedForWorkflowNode nid (PendingChild fr.frId Nothing (Just nid) (KWorkflowNode fr.frId nid))
            st' = st {stEnvStore = store', stNextFrameId = st.stNextFrameId + 1}
            st'' = setTopPause st' (Just pause)
        case pushFrame st'' childFrame of
          Left _err -> failGeneration st [] mempty GErrAborted
          Right st''' -> pure (Stepped st''')
      WHandoff spec _hnid -> do
        let target = spec.hsTarget
            overrides = EnvOverrides (Just target.ssAgent) (Just target.ssModels) (target.ssRtOverrides)
            (store', childId) = forkEnv parentEnv overrides st.stEnvStore
            childEnv = fromMaybe parentEnv (lookupEnv store' childId)
            genId = childEnv.envRt.rtGenerationId
            childAls =
              AgentLocalState
                { alsGenerationId = genId,
                  alsCurrentTurns = target.ssInitialTurns,
                  alsAccTurns = [],
                  alsUsage = emptyUsage,
                  alsLoopCount = 0,
                  alsPendingTools = Nothing
                }
            childFrame =
              Frame
                { frId = st.stNextFrameId,
                  frKind = FAgent childAls,
                  frEnvId = childId,
                  frStep = StepCheckAbort,
                  frPause = Nothing,
                  frReturn = KWorkflowNode fr.frId nid,
                  frTranscriptPolicy = target.ssTranscriptPolicy
                }
            pause = PausedForWorkflowNode nid (PendingChild fr.frId Nothing (Just nid) (KWorkflowNode fr.frId nid))
            st' = st {stEnvStore = store', stNextFrameId = st.stNextFrameId + 1}
            st'' = setTopPause st' (Just pause)
        case pushFrame st'' childFrame of
          Left _err -> failGeneration st [] mempty GErrAborted
          Right st''' -> pure (Stepped st''')
      WSeq wfs -> do
        let prog = compileWorkflow (WSeq wfs)
            wls =
              WorkflowLocalState
                { wlsProg = prog,
                  wlsPc = initialPc prog,
                  wlsNodeResults = Map.empty,
                  wlsFinalResult = Nothing
                }
            childFrame =
              Frame
                { frId = st.stNextFrameId,
                  frKind = FWorkflow wls,
                  frEnvId = fr.frEnvId,
                  frStep = StepWfAdvance,
                  frPause = Nothing,
                  frReturn = KWorkflowNode fr.frId nid,
                  frTranscriptPolicy = TranscriptSummaryOnly
                }
            pause = PausedForWorkflowNode nid (PendingChild fr.frId Nothing (Just nid) (KWorkflowNode fr.frId nid))
            st' = st {stNextFrameId = st.stNextFrameId + 1}
            st'' = setTopPause st' (Just pause)
        case pushFrame st'' childFrame of
          Left _err -> failGeneration st [] mempty GErrAborted
          Right st''' -> pure (Stepped st''')
      WPar wfs pol -> do
        let prog = compileWorkflow (WPar wfs pol)
            wls =
              WorkflowLocalState
                { wlsProg = prog,
                  wlsPc = initialPc prog,
                  wlsNodeResults = Map.empty,
                  wlsFinalResult = Nothing
                }
            childFrame =
              Frame
                { frId = st.stNextFrameId,
                  frKind = FWorkflow wls,
                  frEnvId = fr.frEnvId,
                  frStep = StepWfAdvance,
                  frPause = Nothing,
                  frReturn = KWorkflowNode fr.frId nid,
                  frTranscriptPolicy = TranscriptSummaryOnly
                }
            pause = PausedForWorkflowNode nid (PendingChild fr.frId Nothing (Just nid) (KWorkflowNode fr.frId nid))
            st' = st {stNextFrameId = st.stNextFrameId + 1}
            st'' = setTopPause st' (Just pause)
        case pushFrame st'' childFrame of
          Left _err -> failGeneration st [] mempty GErrAborted
          Right st''' -> pure (Stepped st''')
      WLoop spec -> do
        let lfs =
              LoopFrameState
                { lfsSpec = spec,
                  lfsIteration = 0,
                  lfsLastResult = Nothing,
                  lfsPhase = LoopPhaseCheck,
                  lfsFinalResult = Nothing
                }
            childFrame =
              Frame
                { frId = st.stNextFrameId,
                  frKind = FLoop lfs,
                  frEnvId = fr.frEnvId,
                  frStep = StepLoopCheck,
                  frPause = Nothing,
                  frReturn = KWorkflowNode fr.frId nid,
                  frTranscriptPolicy = TranscriptSummaryOnly
                }
            pause = PausedForWorkflowNode nid (PendingChild fr.frId Nothing (Just nid) (KWorkflowNode fr.frId nid))
            st' = st {stNextFrameId = st.stNextFrameId + 1}
            st'' = setTopPause st' (Just pause)
        case pushFrame st'' childFrame of
          Left _err -> failGeneration st [] mempty GErrAborted
          Right st''' -> pure (Stepped st''')

-- ---------------------------------------------------------------------------
-- StepWfChildDone — store result, advance PC
-- ---------------------------------------------------------------------------

stepWfChildDone :: Stack -> Frame -> NodeId -> PopResult -> IO (ReduceOutcome Stack)
stepWfChildDone st _fr nid pop = do
  let wnr =
        WorkflowNodeResult
          { wnrNodeId = nid,
            wnrOutput = popToOutput pop,
            wnrUsage = pop.popUsage
          }
      st' =
        updateTopWorkflow st $ \wls ->
          wls {wlsNodeResults = Map.insert nid wnr wls.wlsNodeResults}
      st'' = st' {stNodeResults = Map.insert nid wnr st'.stNodeResults}
      st''' = setTopPause st'' Nothing
  case (topFrame st''').frKind of
    FWorkflow wls -> do
      let pc' = advancePc wls.wlsProg wls.wlsPc nid
          wls' = wls {wlsPc = pc', wlsNodeResults = Map.insert nid wnr wls.wlsNodeResults}
          st4 = updateTopWorkflow st''' (const wls')
      pure (Stepped (setTopStep st4 StepWfAdvance))
    _ -> pure (Stepped (setTopStep st''' StepWfAdvance))

-- ---------------------------------------------------------------------------
-- StepWfMerge — merge parallel branch results
-- ---------------------------------------------------------------------------

stepWfMerge :: StackRuntime -> Stack -> Frame -> MergePolicy -> IO (ReduceOutcome Stack)
stepWfMerge rt st _fr pol =
  case (topFrame st).frKind of
    FWorkflow wls -> do
      let results = Map.elems wls.wlsNodeResults
      merged <- mergeWorkflowResults rt pol results
      let wls' = wls {wlsFinalResult = Just merged.wnrOutput}
          st' = updateTopWorkflow st (const wls')
      pure (Stepped (setTopStep st' StepFinish))
    _ -> pure (Stepped (setTopStep st StepFinish))

-- ===========================================================================
-- Loop step handlers (stage 4)
-- ===========================================================================

reduceLoopStep :: StackRuntime -> Stack -> Frame -> LoopFrameState -> IO (ReduceOutcome Stack)
reduceLoopStep rt st fr _lfs = case fr.frStep of
  StepLoopCheck -> stepLoopCheck rt st fr
  StepLoopStartIteration -> stepLoopStartIteration rt st fr
  StepLoopEndIteration -> stepLoopEndIteration st fr
  StepFinish -> stepFinish st fr
  StepPopChild pop -> stepPopChild rt st fr pop
  _ -> pure (Stepped st)

-- ---------------------------------------------------------------------------
-- StepLoopCheck
-- ---------------------------------------------------------------------------

stepLoopCheck :: StackRuntime -> Stack -> Frame -> IO (ReduceOutcome Stack)
stepLoopCheck rt st _fr =
  case (topFrame st).frKind of
    FLoop lfs -> do
      let shouldExit = evalLoopExit rt lfs st
      if shouldExit
        then do
          let finalResult = case lfs.lfsLastResult of
                Just wnr -> wnr.wnrOutput
                Nothing -> Left (GenerateErrorResult GErrAborted [] mempty)
              lfs' = lfs {lfsPhase = LoopPhaseDone, lfsFinalResult = Just finalResult}
              st' = updateTopLoop st (const lfs')
          pure (Stepped (setTopStep st' StepFinish))
        else pure (Stepped (setTopStep st StepLoopStartIteration))
    _ -> pure (Stepped st)

-- ---------------------------------------------------------------------------
-- StepLoopStartIteration — push compiled body
-- ---------------------------------------------------------------------------

stepLoopStartIteration :: StackRuntime -> Stack -> Frame -> IO (ReduceOutcome Stack)
stepLoopStartIteration rt st fr =
  case (topFrame st).frKind of
    FLoop lfs -> do
      let spec = lfs.lfsSpec
          lfs' = lfs {lfsPhase = LoopPhaseRunningBody}
          st' = updateTopLoop st (const lfs')
      applyLoopCarry st' spec lfs
      startWorkflowChild rt st' fr spec.loopNodeId spec.loopBody
    _ -> pure (Stepped st)

-- ---------------------------------------------------------------------------
-- StepLoopEndIteration
-- ---------------------------------------------------------------------------

stepLoopEndIteration :: Stack -> Frame -> IO (ReduceOutcome Stack)
stepLoopEndIteration st _fr =
  case (topFrame st).frKind of
    FLoop lfs -> do
      let lfs' =
            lfs
              { lfsIteration = lfs.lfsIteration + 1,
                lfsPhase = LoopPhaseBetweenIterations
              }
          st' = updateTopLoop st (const lfs')
          st'' = setTopPause st' Nothing
      pure (Stepped (setTopStep st'' StepLoopCheck))
    _ -> pure (Stepped st)

-- ===========================================================================
-- Tool execution
-- ===========================================================================

executeOneTool :: StackRuntime -> Stack -> Frame -> AgentLocalState -> ToolCall -> IO (Either GenerateError ToolStepResult)
executeOneTool rt st fr als tc =
  case lookupEnv st.stEnvStore fr.frEnvId of
    Nothing -> pure (Left GErrAborted)
    Just env -> do
      let ctx = createToolContext env.envAgent als.alsCurrentTurns als.alsUsage env.envRt
      case findToolByName rt env tc.tcName of
        Nothing ->
          pure (Right (TSRReply (toolResult tc ("Unknown tool: " <> tc.tcName))))
        Just utool -> do
          result <- try (utool.utToolExec ctx tc.tcArguments)
          case result of
            Left (e :: SomeException) ->
              let msg = "UTool error: " <> T.pack (show e)
               in pure (Right (TSRReply (toolResult tc msg)))
            Right outcome -> case outcome of
              ToolReply txt -> pure (Right (TSRReply (toolResult tc txt)))
              ToolCommand cmd -> pure (Right (TSRCommand cmd))
              ToolReplyAndCommand txt cmd ->
                pure (Right (TSRReplyAndCommand (toolResult tc txt) cmd))

findToolByName :: StackRuntime -> Env -> Text -> Maybe UTool
findToolByName rt env name =
  let legacyTools = getResolvedTools env.envAgent env.envRt
      legacyMap = Map.fromList [(t.toolDef.toolName, utTool t) | t <- legacyTools]
      namedUTools = getResolvedUTools rt.srUTools env.envAgent env.envRt
      namedMap = Map.fromList [(t.utToolDef.toolName, t) | t <- namedUTools]
      allTools = Map.union namedMap legacyMap
   in Map.lookup name allTools

utTool :: Tool -> UTool
utTool tool =
  UTool
    { utToolDef = tool.toolDef,
      utToolExec = \ctx val -> ToolReply <$> tool.toolExecute ctx val
    }

getResolvedUTools :: UToolRegistry -> Agent -> RuntimeArgs -> [UTool]
getResolvedUTools registry agent rt =
  let utools = mapMaybe (`Map.lookup` registry) agent.agUTools
   in if rt.rtReadonly
        then filter (\x -> x.utToolDef.toolReadonly) utools
        else utools

-- ===========================================================================
-- Agent command application
-- ===========================================================================

applyAgentCommand :: StackRuntime -> Stack -> Frame -> AgentLocalState -> PendingToolRound -> AgentCommand -> IO (ReduceOutcome Stack)
applyAgentCommand _rt st fr als pending cmd = case cmd of
  CmdPushSubagent spec -> do
    let pause = PausedForSubagent (currentToolCallId pending) pending
        st' = setTopPause (updateTopAgent st (const als)) (Just pause)
    pure (Stepped (setTopStep st' (StepPushSubagent spec)))
  CmdHandoff spec ->
    pure (Stepped (setTopStep st (StepHandoff spec)))
  CmdRunWorkflow wf -> do
    let prog = compileWorkflow wf
        wls =
          WorkflowLocalState
            { wlsProg = prog,
              wlsPc = initialPc prog,
              wlsNodeResults = Map.empty,
              wlsFinalResult = Nothing
            }
        childFrame =
          Frame
            { frId = st.stNextFrameId,
              frKind = FWorkflow wls,
              frEnvId = fr.frEnvId,
              frStep = StepWfAdvance,
              frPause = Nothing,
              frReturn = KResumeToolRound fr.frId (currentToolCallId pending),
              frTranscriptPolicy = TranscriptSummaryOnly
            }
        pause = PausedForSubagent (currentToolCallId pending) pending
        st' = setTopPause (updateTopAgent st (const als)) (Just pause)
        st'' = st' {stNextFrameId = st'.stNextFrameId + 1}
    case pushFrame st'' childFrame of
      Left _err -> failGeneration st als.alsAccTurns als.alsUsage GErrAborted
      Right st''' -> pure (Stepped st''')
  CmdPopWith summary -> do
    let pop =
          PopResult
            { popSummary = summary,
              popStructured = Nothing,
              popUsage = als.alsUsage,
              popNewTurns = []
            }
    pure (Stepped (setTopStep st (StepPopChild pop)))
  CmdFail err ->
    failGeneration st als.alsAccTurns als.alsUsage err
  CmdAwaitUser gate ->
    pure (Stepped (setTopStep st (StepAwaitUser gate)))

-- ===========================================================================
-- Checks
-- ===========================================================================

isAbortedForStack :: Stack -> IO Bool
isAbortedForStack st = do
  configAborted <- isAbortedMaybe st.stConfig.scSharedAbortSignal
  if configAborted
    then pure True
    else case topEnv st of
      Nothing -> pure False
      Just env -> isAbortedMaybe env.envRt.rtAbortSignal

checkMaxRounds :: Stack -> AgentLocalState -> IO (Maybe GenerateErrorResult)
checkMaxRounds st als =
  case topEnv st of
    Nothing -> pure (Just (GenerateErrorResult GErrAllModelsFailed als.alsAccTurns als.alsUsage))
    Just env ->
      let limit = fromMaybe env.envAgent.agMaxToolRounds st.stConfig.scMaxToolRoundsPerFrame
       in if als.alsLoopCount >= limit
            then pure (Just (GenerateErrorResult GErrToolExceeded als.alsAccTurns als.alsUsage))
            else pure Nothing

tickGlobalLimits :: Stack -> Either GenerateErrorResult Stack
tickGlobalLimits st =
  case st.stConfig.scMaxTotalSteps of
    Just maxSteps
      | st.stTotalSteps >= maxSteps ->
          Left (GenerateErrorResult GErrToolExceeded [] st.stGlobalUsage)
    _ -> Right st

-- ===========================================================================
-- Result helpers
-- ===========================================================================

failGeneration :: Stack -> [Turn] -> Usage -> GenerateError -> IO (ReduceOutcome Stack)
failGeneration st acc usage err = do
  let errResult = GenerateErrorResult err acc usage
  case topEnv st of
    Just env -> emitEvent env.envRt (GenerationFailed err errResult)
    Nothing -> pure ()
  pure (ReduceFinished (Left errResult))

frameToResult :: Frame -> Either GenerateErrorResult GenerateTextResult
frameToResult fr = case fr.frKind of
  FAgent als ->
    Right
      GenerateTextResult
        { gtrGenerationId = als.alsGenerationId,
          gtrNewMessages = als.alsAccTurns,
          gtrText = extractFinalText als.alsAccTurns,
          gtrUsage = als.alsUsage
        }
  FWorkflow wls ->
    case wls.wlsFinalResult of
      Just result -> result
      Nothing -> Left (GenerateErrorResult GErrAborted [] mempty)
  FLoop lfs ->
    case lfs.lfsFinalResult of
      Just result -> result
      Nothing -> Left (GenerateErrorResult GErrAborted [] mempty)

extractFinalText :: [Turn] -> Text
extractFinalText turns = case reverse turns of
  (AssistantTurn txt _ _ : _) -> txt
  _ -> ""

buildPopResult :: TranscriptPolicy -> Either GenerateErrorResult GenerateTextResult -> PopResult
buildPopResult policy result = case result of
  Left err ->
    PopResult
      { popSummary = "",
        popStructured = Nothing,
        popUsage = err.gerUsage,
        popNewTurns = err.gerPartialNewMessages
      }
  Right success ->
    PopResult
      { popSummary = success.gtrText,
        popStructured = Nothing,
        popUsage = success.gtrUsage,
        popNewTurns = case policy of
          TranscriptShared -> success.gtrNewMessages
          TranscriptSummaryOnly -> []
          TranscriptIsolated -> []
      }

popToOutput :: PopResult -> Either GenerateErrorResult GenerateTextResult
popToOutput pop =
  Right
    GenerateTextResult
      { gtrGenerationId = nilUuid,
        gtrNewMessages = pop.popNewTurns,
        gtrText = pop.popSummary,
        gtrUsage = pop.popUsage
      }

-- ===========================================================================
-- Subagent resume
-- ===========================================================================

resumeParentToolRound :: Stack -> PopResult -> ToolCallId -> Stack
resumeParentToolRound st pop tcId =
  case (topFrame st).frPause of
    Just (PausedForSubagent _savedTcId savedRound) ->
      let syntheticResult = ToolResult {trCallId = tcId, trName = "subagent", trContent = pop.popSummary}
          round' =
            savedRound
              { ptrNextIndex = savedRound.ptrNextIndex + 1,
                ptrResults = savedRound.ptrResults ++ [syntheticResult]
              }
          st' =
            updateTopAgent st $ \als ->
              als {alsPendingTools = Just round'}
          st'' = setTopPause st' Nothing
       in case nextToolCall round' of
            Just nextTc -> setTopStep st'' (StepRunTool nextTc.tcId)
            Nothing -> setTopStep st'' StepApplyToolResults
    _ ->
      setTopStep st StepCheckAbort

-- ===========================================================================
-- Handoff helpers
-- ===========================================================================

buildHandoffTurns :: AgentLocalState -> HandoffSpec -> [Turn]
buildHandoffTurns als spec =
  let base = spec.hsTarget
      parentTurns = als.alsCurrentTurns
   in case spec.hsContext of
        HandoffFullTranscript -> parentTurns ++ base.ssInitialTurns
        HandoffSummary summary -> UserTurn summary : base.ssInitialTurns
        HandoffWindow n ->
          let dropped = drop (max 0 (length parentTurns - n)) parentTurns
           in dropped ++ base.ssInitialTurns

-- ===========================================================================
-- Workflow compilation
-- ===========================================================================

compileWorkflow :: Workflow -> WorkflowProg
compileWorkflow wf = case wf of
  WRunAgent inp -> WProgSeq [(inp.aniNodeId, wf)]
  WSeq wfs -> WProgSeq [(workflowNodeId w, w) | w <- wfs]
  WPar wfs pol -> WProgPar [(workflowNodeId w, w) | w <- wfs] pol
  WLoop spec -> WProgLoop spec
  WHandoff _ nid -> WProgSeq [(nid, wf)]
  WSubagent _ nid -> WProgSeq [(nid, wf)]

workflowNodeId :: Workflow -> NodeId
workflowNodeId = \case
  WRunAgent inp -> inp.aniNodeId
  WSeq _ -> "seq"
  WPar _ _ -> "par"
  WLoop spec -> spec.loopNodeId
  WHandoff _ nid -> nid
  WSubagent _ nid -> nid

initialPc :: WorkflowProg -> WorkflowPc
initialPc = \case
  WProgSeq _ -> PcSeq 0 SeqBetween
  WProgPar nodes _ ->
    let nids = map fst nodes
     in PcPar
          ParBranchState
            { pbsNodeIds = nids,
              pbsCurrent = 0,
              pbsResults = Map.empty,
              pbsStatuses = Map.fromList [(nid, ParPending) | nid <- nids]
            }
  WProgLoop _spec ->
    PcLoop
      LoopPc
        { lpIteration = 0,
          lpInner = PcSeq 0 SeqBetween,
          lpPhase = LoopPhaseCheck
        }

advancePc :: WorkflowProg -> WorkflowPc -> NodeId -> WorkflowPc
advancePc _prog pc nid = case pc of
  PcSeq {pcSeqIndex} ->
    PcSeq (pcSeqIndex + 1) SeqBetween
  PcPar pbs ->
    let pbs' =
          pbs
            { pbsCurrent = pbs.pbsCurrent + 1,
              pbsStatuses = Map.insert nid ParDone pbs.pbsStatuses
            }
     in PcPar pbs'
  PcLoop lpc ->
    PcLoop lpc {lpPhase = LoopPhaseBetweenIterations}

lookupWorkflowNode :: Frame -> NodeId -> Maybe Workflow
lookupWorkflowNode fr nid = case fr.frKind of
  FWorkflow wls -> lookupInProg wls.wlsProg nid
  FLoop lfs -> Just lfs.lfsSpec.loopBody
  FAgent _ -> Nothing

lookupInProg :: WorkflowProg -> NodeId -> Maybe Workflow
lookupInProg prog nid = case prog of
  WProgSeq nodes -> lookup nid nodes
  WProgPar nodes _ -> lookup nid nodes
  WProgLoop spec ->
    if spec.loopNodeId == nid
      then Just spec.loopBody
      else Nothing

-- ===========================================================================
-- Workflow input resolution
-- ===========================================================================

resolveWorkflowInput :: Stack -> WorkflowInput -> [Turn]
resolveWorkflowInput st inp = case inp of
  WInputTurns ts -> ts
  WInputText t -> [UserTurn t]
  WInputFromPrior nid ->
    case Map.lookup nid st.stNodeResults of
      Nothing -> []
      Just wnr ->
        either
          (.gerPartialNewMessages)
          (.gtrNewMessages)
          wnr.wnrOutput

-- ===========================================================================
-- Merge
-- ===========================================================================

mergeWorkflowResults :: StackRuntime -> MergePolicy -> [WorkflowNodeResult] -> IO WorkflowNodeResult
mergeWorkflowResults rt pol results = case pol of
  MergeConcat -> pure (mergeConcat results)
  MergeFirstSuccess -> pure (mergeFirstSuccess results)
  MergeWithAgent _agent -> pure (mergeConcat results)
  MergeCustomNamed key ->
    case Map.lookup key rt.srMergeFns of
      Just (MergeFn f) -> f results
      Nothing -> pure (mergeConcat results)

mergeConcat :: [WorkflowNodeResult] -> WorkflowNodeResult
mergeConcat results =
  let texts = mapMaybe (eitherToMaybeText . (.wnrOutput)) results
      output =
        if null texts
          then Left (GenerateErrorResult GErrAborted [] mempty)
          else
            Right
              GenerateTextResult
                { gtrGenerationId = nilUuid,
                  gtrNewMessages = [],
                  gtrText = T.intercalate "\n" texts,
                  gtrUsage = foldMap (.wnrUsage) results
                }
   in WorkflowNodeResult
        { wnrNodeId = "merged",
          wnrOutput = output,
          wnrUsage = foldMap (.wnrUsage) results
        }

mergeFirstSuccess :: [WorkflowNodeResult] -> WorkflowNodeResult
mergeFirstSuccess results =
  case mapMaybe (eitherToMaybe . (.wnrOutput)) results of
    (x : _) ->
      WorkflowNodeResult
        { wnrNodeId = "merged",
          wnrOutput = Right x,
          wnrUsage = foldMap (.wnrUsage) results
        }
    [] ->
      WorkflowNodeResult
        { wnrNodeId = "merged",
          wnrOutput = Left (GenerateErrorResult GErrAborted [] mempty),
          wnrUsage = foldMap (.wnrUsage) results
        }

-- ===========================================================================
-- Loop helpers
-- ===========================================================================

evalLoopExit :: StackRuntime -> LoopFrameState -> Stack -> Bool
evalLoopExit rt lfs st
  | lfs.lfsIteration >= lfs.lfsSpec.loopMaxIterations = True
  | otherwise = case lfs.lfsSpec.loopExit of
      LoopExitOnMaxOnly -> False
      LoopExitOnSuccess ->
        case lfs.lfsLastResult of
          Just wnr -> isRight wnr.wnrOutput
          Nothing -> False
      LoopExitOnError targetErr ->
        case lfs.lfsLastResult of
          Just wnr -> case wnr.wnrOutput of
            Left err -> err.gerError == targetErr
            Right _ -> False
          Nothing -> False
      LoopExitWhenNamed key ->
        case Map.lookup key rt.srLoopPredicates of
          Just (LoopPredicate p) -> p (workflowLoopView lfs st)
          Nothing -> False

workflowLoopView :: LoopFrameState -> Stack -> WorkflowLoopView
workflowLoopView lfs st =
  WorkflowLoopView
    { wlvIteration = lfs.lfsIteration,
      wlvMaxIterations = lfs.lfsSpec.loopMaxIterations,
      wlvLastResult = lfs.lfsLastResult,
      wlvNodeResults = st.stNodeResults,
      wlvCarry = lfs.lfsSpec.loopCarry
    }

applyLoopCarry :: Stack -> LoopSpec -> LoopFrameState -> IO ()
applyLoopCarry _st _spec _lfs = pure ()

-- ===========================================================================
-- Public API
-- ===========================================================================

generateText5 ::
  StackRuntime ->
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
generateText5 rt agent models runtimeArgs turns = do
  let env =
        mkEnv
          agent
          models
          runtimeArgs
          (\a m r t -> generateTextWithFallbacks (createGenRequestU rt.srUTools a r t) m)
      env' = env {envId = 0}
      st = initialStack env' defaultStackConfig turns
  emitEvent runtimeArgs GenerationStarted
  result <- runStack rt st
  case result of
    ReduceFinished r -> pure r
    ReduceBlocked _ _ -> pure (Left (GenerateErrorResult GErrAborted [] mempty))
    Stepped _ -> pure (Left (GenerateErrorResult GErrAborted [] mempty))

streamText5 ::
  StackRuntime ->
  (StreamChunk -> IO ()) ->
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
streamText5 rt onChunk agent models runtimeArgs turns = do
  let env =
        mkEnv
          agent
          models
          runtimeArgs
          (\a m r t -> streamTextWithFallbacks onChunk (createGenRequestU rt.srUTools a r t) m)
      env' = env {envId = 0}
      st = initialStack env' defaultStackConfig turns
  emitEvent runtimeArgs GenerationStarted
  result <- runStack rt st
  case result of
    ReduceFinished r -> pure r
    ReduceBlocked _ _ -> pure (Left (GenerateErrorResult GErrAborted [] mempty))
    Stepped _ -> pure (Left (GenerateErrorResult GErrAborted [] mempty))

-- ===========================================================================
-- Inspection
-- ===========================================================================

peekNextAction :: Stack -> StackView
peekNextAction st =
  StackView
    { svDepth = st.stDepth,
      svFrames = map viewFrame (NE.toList st.stFrames),
      svNext = nextActionFromFrame (topFrame st)
    }

viewFrame :: Frame -> FrameView
viewFrame fr =
  FrameView
    { fvFrameId = fr.frId,
      fvKind = case fr.frKind of
        FAgent _ -> "agent"
        FWorkflow _ -> "workflow"
        FLoop _ -> "loop",
      fvStep = stepDescription fr.frStep,
      fvEnvId = fr.frEnvId
    }

nextActionFromFrame :: Frame -> NextAction
nextActionFromFrame fr = case fr.frStep of
  StepCallModel -> NextLLM
  StepRunTool tcId -> NextTool tcId
  StepAwaitUser gate -> NextBlocked gate
  StepFinish -> NextDone
  StepCheckAbort -> NextPure "check-abort"
  StepApplyToolResults -> NextPure "apply-tool-results"
  StepPushSubagent _ -> NextPure "push-subagent"
  StepPopChild _ -> NextPure "pop-child"
  StepHandoff _ -> NextPure "handoff"
  StepWfAdvance -> NextPure "wf-advance"
  StepWfStartNode nid -> NextPure ("wf-start:" <> nid)
  StepWfChildDone nid _ -> NextPure ("wf-child-done:" <> nid)
  StepWfMerge _ -> NextPure "wf-merge"
  StepLoopCheck -> NextPure "loop-check"
  StepLoopStartIteration -> NextPure "loop-start-iteration"
  StepLoopEndIteration -> NextPure "loop-end-iteration"

stepDescription :: Step -> Text
stepDescription = \case
  StepCheckAbort -> "check-abort"
  StepCallModel -> "call-model"
  StepRunTool tcId -> "run-tool:" <> tcId
  StepApplyToolResults -> "apply-tool-results"
  StepPushSubagent _ -> "push-subagent"
  StepPopChild _ -> "pop-child"
  StepHandoff _ -> "handoff"
  StepWfStartNode nid -> "wf-start-node:" <> nid
  StepWfChildDone nid _ -> "wf-child-done:" <> nid
  StepWfMerge _ -> "wf-merge"
  StepWfAdvance -> "wf-advance"
  StepLoopCheck -> "loop-check"
  StepLoopStartIteration -> "loop-start-iteration"
  StepLoopEndIteration -> "loop-end-iteration"
  StepAwaitUser _ -> "await-user"
  StepFinish -> "finish"

-- ===========================================================================
-- Internal helpers
-- ===========================================================================

classifyStep :: Frame -> StepKind
classifyStep fr = case fr.frStep of
  StepCallModel -> StepKindLLM fr.frId
  StepRunTool tcId -> StepKindTool fr.frId tcId
  StepCheckAbort -> StepKindPure "check-abort"
  StepApplyToolResults -> StepKindPure "apply-tool-results"
  StepPushSubagent _ -> StepKindStackOp "push-subagent"
  StepPopChild _ -> StepKindStackOp "pop-child"
  StepHandoff _ -> StepKindStackOp "handoff"
  StepWfAdvance -> StepKindPure "wf-advance"
  StepWfStartNode _ -> StepKindStackOp "wf-start-node"
  StepWfChildDone _ _ -> StepKindStackOp "wf-child-done"
  StepWfMerge _ -> StepKindPure "wf-merge"
  StepLoopCheck -> StepKindPure "loop-check"
  StepLoopStartIteration -> StepKindStackOp "loop-start-iteration"
  StepLoopEndIteration -> StepKindPure "loop-end-iteration"
  StepAwaitUser _ -> StepKindAwaitUser
  StepFinish -> StepKindPure "finish"

findCallById :: [ToolCall] -> ToolCallId -> Maybe ToolCall
findCallById [] _ = Nothing
findCallById (tc : rest) tcId
  | tc.tcId == tcId = Just tc
  | otherwise = findCallById rest tcId

nextToolCall :: PendingToolRound -> Maybe ToolCall
nextToolCall ptr
  | ptr.ptrNextIndex < length ptr.ptrCalls = Just (ptr.ptrCalls !! ptr.ptrNextIndex)
  | otherwise = Nothing

currentToolCallId :: PendingToolRound -> ToolCallId
currentToolCallId ptr
  | ptr.ptrNextIndex < length ptr.ptrCalls = (ptr.ptrCalls !! ptr.ptrNextIndex).tcId
  | otherwise = ""

loopNodeIdFromFrame :: Stack -> Frame -> NodeId
loopNodeIdFromFrame _st fr = case fr.frKind of
  FLoop lfs -> lfs.lfsSpec.loopNodeId
  _ -> ""

emitEventForFrame :: Stack -> Frame -> GenerateEventDetail -> IO ()
emitEventForFrame st fr detail =
  case lookupEnv st.stEnvStore fr.frEnvId of
    Just env -> emitEvent env.envRt detail
    Nothing -> pure ()

createGenRequestU :: UToolRegistry -> Agent -> RuntimeArgs -> [Turn] -> GenRequest
createGenRequestU registry agent rt messages =
  let offset = windowOffset agent.agContextWindow messages
      tools = getResolvedTools agent rt
      utools = getResolvedUTools registry agent rt
   in GenRequest
        { grSystemPrompt = agent.agSystemPrompt,
          grTools = map (\x -> x.toolDef) tools ++ map (\x -> x.utToolDef) utools,
          grMessages = drop offset messages,
          grAbortSignal = rt.rtAbortSignal,
          grLLMHooks = rt.rtLLMHooks,
          grHooks = rt.rtHooks
        }

nilUuid :: UUID
nilUuid = read @UUID "00000000-0000-0000-0000-000000000000"

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

eitherToMaybeText :: Either GenerateErrorResult GenerateTextResult -> Maybe Text
eitherToMaybeText = either (const Nothing) (\r -> Just r.gtrText)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False
