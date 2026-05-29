{-# LANGUAGE DerivingStrategies #-}

-- | Staged stack-machine types for a future @reduceStep@-based interpreter.
--
-- Stages (each layer extends the previous):
--
-- 1. Basic agent loop — one LLM or one tool per reduction.
-- 2. Subagent — tool pushes child frame; pop resumes parent tool round.
-- 3. Handoff — replace top agent; conversation carried via 'HandoffContext'.
-- 4. Workflows — 'WSeq', 'WPar', 'WLoop' (dialog ≈ @WLoop (WSeq [A,B])@).
-- 5. User confirmation — 'StepAwaitUser' / 'ReduceBlocked'.
--
-- This module defines types only; no interpreter yet.
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
  )
where

import Data.Aeson (Value)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.UUID.Types (UUID)
import LLM.Agent.Types (Agent, RuntimeArgs)
import LLM.Core.Abort (AbortSignal)
import LLM.Core.Types (ToolCall, ToolResult, Turn)
import LLM.Core.Usage (Usage)
import LLM.Generate.ModelConfig (ModelWithFallbacks)
import LLM.Generate.Types
  ( GenerateError,
    GenerateErrorResult,
    GenerateTextResult,
  )

-- ---------------------------------------------------------------------------
-- Identifiers
-- ---------------------------------------------------------------------------

type EnvId = Int

type FrameId = Int

type NodeId = Text

type ToolCallId = Text

type KontId = FrameId

-- ---------------------------------------------------------------------------
-- Environment (stage 1+)
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
      IO (Either GenerateError GenerateTextResult)
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

-- ---------------------------------------------------------------------------
-- Stack (stage 1+)
-- ---------------------------------------------------------------------------

data Stack = Stack
  { stEnvStore :: EnvStore,
    stFrames :: NonEmpty Frame,
    stNextFrameId :: FrameId,
    stDepth :: Int,
    stGlobalUsage :: Usage,
    stRootGenerationId :: UUID,
    stConfig :: StackConfig,
    -- | Completed workflow node outputs (stage 4+).
    stNodeResults :: Map NodeId WorkflowNodeResult,
    -- | Optional trace of the last reduction (debugging / stepping UI).
    stLastStep :: Maybe StepKind
  }

data StackConfig = StackConfig
  { scMaxStackDepth :: Int,
    scMaxToolRoundsPerFrame :: Maybe Int,
    scSharedAbortSignal :: Maybe AbortSignal,
    -- | Global safety net across all frames and nested loops (stage 4+).
    scMaxTotalSteps :: Maybe Int,
  -- | Global token budget for the whole run (optional).
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

-- ---------------------------------------------------------------------------
-- Frame (stages 1–5)
-- ---------------------------------------------------------------------------

data Frame = Frame
  { frId :: FrameId,
    frKind :: FrameKind,
    frEnvId :: EnvId,
    -- | What to run on the next reduction (program counter).
    frStep :: Step,
    -- | Parent pause while a child runs (stage 2+).
    frPause :: Maybe ParentPause,
    -- | Return address on child frames (stage 2+; optional if parent holds pause).
    frReturn :: ReturnKont
  }

data FrameKind
  = -- | Standard model ↔ tools loop (stage 1+).
    FAgent AgentLocalState
  | -- | Workflow scheduler (stage 4+).
    FWorkflow WorkflowLocalState
  | -- | Dedicated loop scheduler state (stage 4+; may also live inside 'FWorkflow').
    FLoop LoopFrameState

-- | Parent suspended until child completes (stage 2+).
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

-- | Where control returns after a child finishes (stage 2+).
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

-- | Bookkeeping before pushing a child (stage 2+ / 4+).
data PendingChild = PendingChild
  { pcParentFrameId :: FrameId,
    pcToolCallId :: Maybe ToolCallId,
    pcNodeId :: Maybe NodeId,
    pcKont :: ReturnKont
  }
  deriving stock (Show)

-- ---------------------------------------------------------------------------
-- Step — stored on the frame (stage 1+)
-- ---------------------------------------------------------------------------

data Step
  = -- | Stage 1
    StepCheckAbort
  | StepCallModel
  | StepRunTool ToolCallId
  | StepApplyToolResults
  | -- | Stage 2
    StepPushSubagent SubagentSpec
  | StepPopChild PopResult
  | -- | Stage 3
    StepHandoff HandoffSpec
  | -- | Stage 4 (workflow scheduler)
    StepWfStartNode NodeId
  | StepWfChildDone NodeId PopResult
  | StepWfMerge MergePolicy
  | StepWfAdvance
  | StepLoopCheck
  | StepLoopStartIteration
  | StepLoopEndIteration
  | -- | Stage 5
    StepAwaitUser UserGate
  | StepFinish

-- | Agent-only phase view (alternative to 'Step' when deriving next action).
data AgentPhase
  = PhaseModel
  | PhaseTool Int
  | PhaseApplyTools
  | PhaseDone
  | PhaseAwaitUser UserGate
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Agent local state (stage 1+)
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- Pop / transcript policy (stage 2+)
-- ---------------------------------------------------------------------------

data PopResult = PopResult
  { popSummary :: Text,
    popStructured :: Maybe Value,
    popUsage :: Usage,
    popNewTurns :: [Turn]
  }
  deriving stock (Show)

-- | How a child agent's transcript is exposed to its parent on pop.
data TranscriptPolicy
  = -- | Parent sees summary text only; child turns stay private.
    TranscriptSummaryOnly
  | -- | Child turns are not merged into parent (same prNewTurns as SummaryOnly in G4).
    TranscriptIsolated
  | -- | Parent may merge child messages into its transcript.
    TranscriptShared
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Subagent (stage 2+)
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

-- ---------------------------------------------------------------------------
-- Handoff (stage 3+)
-- ---------------------------------------------------------------------------

data HandoffSpec = HandoffSpec
  { hsTarget :: SubagentSpec,
    hsContext :: HandoffContext,
    -- | When 'True', replace the top frame (typical handoff). When 'False', push.
    hsReplaceStack :: Bool
  }

data HandoffContext
  = HandoffFullTranscript
  | HandoffSummary Text
  | HandoffWindow Int
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Workflow AST (stage 4+)
-- ---------------------------------------------------------------------------

data Workflow
  = WRunAgent AgentNodeInput
  | WSeq [Workflow]
  | WPar [Workflow] MergePolicy
  | WLoop LoopSpec
  | WHandoff HandoffSpec
  | WSubagent SubagentSpec

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

-- | How parallel workflow branches are combined (stage 4+).
data MergePolicy
  = MergeConcat
  | MergeFirstSuccess
  | -- | Reducer agent (one extra LLM step in the interpreter).
    MergeWithAgent Agent
  | -- | Registry key; interpreter resolves to 'MergeFn'.
    MergeCustomNamed Text

-- | Resolved custom merge (attached by the interpreter).
newtype MergeFn = MergeFn
  { runMergeFn :: [WorkflowNodeResult] -> IO WorkflowNodeResult
  }

-- ---------------------------------------------------------------------------
-- Workflow scheduler state (stage 4+)
-- ---------------------------------------------------------------------------

data WorkflowLocalState = WorkflowLocalState
  { wlsProg :: WorkflowProg,
    wlsPc :: WorkflowPc,
    wlsNodeResults :: Map NodeId WorkflowNodeResult
  }

-- | Compiled workflow program (runtime).
data WorkflowProg
  = WProgSeq [NodeId]
  | WProgPar [NodeId] MergePolicy
  | WProgLoop LoopSpec
  | WProgNode Workflow

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

-- ---------------------------------------------------------------------------
-- Loop workflow — termination and carry (stage 4+)
-- ---------------------------------------------------------------------------

data LoopSpec = LoopSpec
  { loopBody :: Workflow,
    loopNodeId :: NodeId,
    -- | Required hard cap; the engine must always be able to stop.
    loopMaxIterations :: Int,
    loopExit :: LoopExit,
    loopCarry :: LoopCarry
  }

defaultLoopSpec :: Workflow -> NodeId -> LoopSpec
defaultLoopSpec body nodeId =
  LoopSpec
    { loopBody = body,
      loopNodeId = nodeId,
      loopMaxIterations = 3,
      loopExit = LoopExitOnMaxOnly,
      loopCarry = LoopCarryFromPrior nodeId
    }

-- | Semantic exit condition (always combined with 'loopMaxIterations').
data LoopExit
  = -- | Run until iteration count reaches 'loopMaxIterations'.
    LoopExitOnMaxOnly
  | -- | Stop early when the body finishes with 'Right'.
    LoopExitOnSuccess
  | -- | Stop early on a specific error.
    LoopExitOnError GenerateError
  | -- | Registry key; interpreter resolves to 'LoopPredicate'.
    LoopExitWhenNamed Text
  deriving stock (Show, Eq)

-- | Pure predicate over 'WorkflowLoopView' (attached by the interpreter).
newtype LoopPredicate = LoopPredicate
  { runLoopPredicate :: WorkflowLoopView -> Bool
  }

-- | How iteration N+1 receives input from iteration N.
data LoopCarry
  = -- | Each iteration uses the same configured input.
    LoopCarryNone
  | -- | Body reads prior iteration via 'WInputFromPrior'.
    LoopCarryFromPrior NodeId
  | -- | Accumulate text under a synthetic node id (e.g. for refine loops).
    LoopCarryAcc NodeId
  deriving stock (Show, Eq)

-- | Runtime loop frame (on stack as 'FLoop' or inside 'PcLoop').
data LoopFrameState = LoopFrameState
  { lfsSpec :: LoopSpec,
    lfsIteration :: Int,
    lfsLastResult :: Maybe WorkflowNodeResult,
    lfsPhase :: LoopPhase
  }

data LoopPhase
  = LoopPhaseCheck
  | LoopPhaseRunningBody
  | LoopPhaseBetweenIterations
  | LoopPhaseDone
  deriving stock (Show, Eq)

-- | Program counter while executing a loop inside a workflow frame.
data LoopPc = LoopPc
  { lpIteration :: Int,
    lpInner :: WorkflowPc,
    lpPhase :: LoopPhase
  }
  deriving stock (Show)

-- | Pure view for 'LoopExitWhen' predicates and stepping UI.
data WorkflowLoopView = WorkflowLoopView
  { wlvIteration :: Int,
    wlvMaxIterations :: Int,
    wlvLastResult :: Maybe WorkflowNodeResult,
    wlvNodeResults :: Map NodeId WorkflowNodeResult,
    wlvCarry :: LoopCarry
  }
  deriving stock (Show)

-- ---------------------------------------------------------------------------
-- User confirmation (stage 5)
-- ---------------------------------------------------------------------------

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

-- | Host supplies this when resuming from 'ReduceBlocked'.
data UserGateOutcome
  = GateContinue UserChoice
  | GateTimeout
  deriving stock (Show, Eq)
