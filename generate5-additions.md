-- generate5-additions.hs
--
-- Analysis of Generate5.hs types against generate5-impl.md.
-- Lists issues, missing types, required changes, and open questions.

-- ============================================================================
-- BLOCKING ISSUE 1: envCall returns the wrong type
-- ============================================================================
--
-- Current (Generate5.hs):
--
--   envCall :: Agent -> ModelWithFallbacks -> RuntimeArgs -> [Turn]
--            -> IO (Either GenerateError GenerateTextResult)
--
-- Generate4.hs has:
--
--   envCall :: Agent -> ModelWithFallbacks -> RuntimeArgs -> [Turn]
--            -> IO (GenerateResult ChatResponse)
--          -- i.e. IO (Either GenerateError ChatResponse)
--
-- GenerateTextResult is the *final* result of an entire agent loop
-- (processed turns, final text, accumulated usage). It does NOT expose
-- the raw ContentBlock list from ChatResponse, which is where individual
-- ToolCall values live. The impl doc §6.2 ("After StepCallModel") requires
-- inspecting respContent for ToolCallBlock entries to build PendingToolRound.
-- The impl doc §6.4 confirms: callModel should return ChatResponse.
--
-- Without ChatResponse, the one-LLM-call-per-reduceStep model cannot work.
--
-- FIX: Change envCall to return ChatResponse, matching Generate4:

-- data Env = Env
--   { ...
--     envCall ::
--       Agent ->
--       ModelWithFallbacks ->
--       RuntimeArgs ->
--       [Turn] ->
--       IO (Either GenerateError ChatResponse)     -- ← ChatResponse, not GenerateTextResult
--   }


-- ============================================================================
-- BLOCKING ISSUE 2: No UTool / ToolOutcome / AgentCommand types
-- ============================================================================
--
-- Generate4 defines UTool, ToolOutcome, AgentCommand, and ToolExecute.
-- These are the bridge between tool execution and stack commands (push
-- subagent, handoff, pop, run workflow, fail). Without them, §6.3
-- (StepRunTool) and §11 (Command.hs) cannot be implemented.
--
-- Generate5 needs either to import them from Generate4 (coupling) or
-- redefine them. Since Generate5 replaces Generate4, redefining is cleaner.
--
-- Required types:

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import Data.Text (Text)

-- Placeholder imports for types that exist elsewhere
import LLM.Core.Types (ToolCall, ToolResult, ToolDef, Turn)
import LLM.Agent.Types (Agent, ToolContext, RuntimeArgs)
import LLM.Generate.Types (GenerateError)

type UToolRegistry = Map Text UTool

data UTool = UTool
  { utToolDef  :: ToolDef
  , utToolExec :: ToolContext -> Value -> IO ToolOutcome
  }

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
  | CmdAwaitUser UserGate          -- stage 5: wire tool → StepAwaitUser


-- ============================================================================
-- BLOCKING ISSUE 3: No StackRuntime record
-- ============================================================================
--
-- The impl doc §1 proposes StackRuntime to hold registries. Without it,
-- reduceStep has no access to UTools, loop predicates, or merge functions.
-- These should NOT live on Stack (Stack should be serializable/snapshotable).

data StackRuntime = StackRuntime
  { srUTools         :: UToolRegistry
  , srLoopPredicates :: Map Text LoopPredicate
  , srMergeFns       :: Map Text MergeFn
  }


-- ============================================================================
-- ISSUE 4: Missing NodeId on WSubagent and WHandoff
-- ============================================================================
--
-- WRunAgent has aniNodeId. WLoop has loopNodeId. But:
--
--   | WHandoff HandoffSpec
--   | WSubagent SubagentSpec
--
-- Neither carries a NodeId. The workflow scheduler stores results in
-- stNodeResults keyed by NodeId. Without it, the scheduler can't record
-- the result and downstream WInputFromPrior can't reference it.
--
-- In Generate4, these are wrapped: WorkflowHandoff { whNodeId },
-- WorkflowSubagent { wsNodeId }.
--
-- FIX: Either wrap in a record (like G4) or add NodeId:

-- | WHandoff HandoffSpec NodeId
-- | WSubagent SubagentSpec NodeId

-- Similarly, WSeq and WPar are composite nodes. If nested inside another
-- workflow, their aggregate result also needs a NodeId. The impl doc
-- suggests generating synthetic ids ("seq-0", "par-1") at compile time,
-- but the AST doesn't carry them. compileWorkflow will need a stateful
-- pass to assign them, or the AST should carry optional NodeIds:

-- | WSeq NodeId [Workflow]
-- | WPar NodeId [Workflow] MergePolicy


-- ============================================================================
-- ISSUE 5: No StackError type
-- ============================================================================
--
-- The impl doc §18 calls for it; Generate4 has MachineError.
-- pushFrame, popFrame, replaceTopFrame need to signal failures.

data StackError
  = SEStackOverflow
  | SEEmptyStack
  | SEEnvNotFound EnvId
  | SEFrameNotFound FrameId
  | SEToolNotFound Text
  deriving stock (Show, Eq)


-- ============================================================================
-- ISSUE 6: No StackView / NextAction / FrameView for peekNextAction
-- ============================================================================
--
-- The impl doc §13 describes these for the stepping UI.
-- They're pure types, easy to add, but needed before peekNextAction
-- can be implemented.

data StackView = StackView
  { svDepth  :: Int
  , svFrames :: [FrameView]
  , svNext   :: NextAction
  }

data FrameView = FrameView
  { fvFrameId :: FrameId
  , fvKind    :: Text           -- "agent" / "workflow" / "loop"
  , fvStep    :: Text           -- human-readable current step
  , fvEnvId   :: EnvId
  }

data NextAction
  = NextLLM
  | NextTool Text               -- tool name
  | NextPure Text               -- description of pure/stack step
  | NextBlocked UserGate
  | NextDone


-- ============================================================================
-- ISSUE 7: No ToolStepResult type
-- ============================================================================
--
-- The impl doc §6.4 references:
--   executeOneTool :: StackRuntime -> Stack -> ToolCall
--                  -> IO (Either GenerateError ToolStepResult)
--
-- ToolStepResult doesn't exist. It needs to capture the tool outcome
-- plus the optional ToolResult (for ToolReply) so the caller can decide
-- whether to store a result or apply a command.

data ToolStepResult
  = TSRReply ToolResult
  | TSRCommand AgentCommand
  | TSRReplyAndCommand ToolResult AgentCommand


-- ============================================================================
-- ISSUE 8: LoopPc.lpInner assumes body is always a workflow
-- ============================================================================
--
-- LoopPc = LoopPc { lpIteration, lpInner :: WorkflowPc, lpPhase }
--
-- But loopBody :: Workflow can be a single WRunAgent, not just WSeq/WPar.
-- If the body is WRunAgent, lpInner would need to be something like
-- PcSeq { pcSeqIndex = 0, pcSeqPhase = SeqRunning nodeId } — a degenerate
-- single-element seq. This works but is surprising.
--
-- Alternative: compile all loop bodies into WProgSeq [nodeId] even for
-- single agents. Document this as an invariant in compileWorkflow.
-- No type change needed, but worth noting.


-- ============================================================================
-- ISSUE 9: TranscriptSummaryOnly vs TranscriptIsolated — unclear semantics
-- ============================================================================
--
-- data TranscriptPolicy
--   = TranscriptSummaryOnly  -- "Parent sees summary text only; child turns stay private."
--   | TranscriptIsolated     -- "Child turns are not merged into parent"
--   | TranscriptShared       -- "Parent may merge child messages into its transcript."
--
-- The comments for SummaryOnly and Isolated describe the same behavior:
-- child turns not visible to parent, parent gets summary. The comment on
-- Isolated even references "(same prNewTurns as SummaryOnly in G4)".
--
-- If they produce the same PopResult, one of them is redundant.
-- If there IS a semantic difference (e.g. SummaryOnly generates a summary
-- via LLM while Isolated uses the raw final text), that distinction needs
-- to be documented and the impl needs to handle it.
--
-- DECISION NEEDED: Clarify or merge these two variants.


-- ============================================================================
-- ISSUE 10: PendingToolRound resume path needs more thought
-- ============================================================================
--
-- When a subagent is spawned mid-tool-round, ParentPause stores:
--   PausedForSubagent { ppsToolCallId, ppsRound :: PendingToolRound }
--
-- On child completion, the parent's PendingToolRound must be updated:
--   1. Find the tool call matching ppsToolCallId in ppsRound.ptrCalls
--   2. Insert the child's PopResult.popSummary as a ToolResult
--   3. Advance ptrNextIndex past the subagent call
--   4. Continue with remaining calls (next StepRunTool)
--
-- The current PendingToolRound stores ptrResults :: [ToolResult] as a flat
-- list. The resume logic needs to INSERT a result at the right position.
-- This works if ptrResults is built in order (appended as tools complete),
-- which it is (ptrNextIndex tracks progress).
--
-- However: a ToolCommand (like PushSubagent) in Generate4 produces NO
-- ToolResult (toolOutcomeToResult returns Nothing for ToolCommand). So the
-- subagent call doesn't contribute a ToolResult to the round at all —
-- instead the child's summary is injected as a synthetic ToolResult when
-- the parent resumes.
--
-- This means resumeParentToolRound must construct a ToolResult from the
-- PopResult and the saved ToolCallId, then insert it. The types support
-- this but the flow is non-obvious. Worth a helper:

-- resumeParentToolRound :: Stack -> PopResult -> ParentPause -> Stack
-- Should:
--   1. Build ToolResult { trCallId = ppsToolCallId, trName = "subagent", trContent = popSummary }
--   2. Insert into ppsRound.ptrResults
--   3. Advance ppsRound.ptrNextIndex
--   4. Set parent frStep = StepRunTool nextCallId (or StepApplyToolResults if done)
--   5. Clear frPause


-- ============================================================================
-- ISSUE 11: No Show instances for debugging-critical types
-- ============================================================================
--
-- These types have no Show instance and contain no function fields:
--   Frame, SubagentSpec, HandoffSpec, HandoffContext (already has Show),
--   Workflow, AgentNodeInput, WorkflowInput (has Show), LoopSpec,
--   LoopFrameState, WorkflowLocalState, WorkflowProg
--
-- Types that CANNOT derive Show (function fields):
--   Env (envCall), MergeFn (runMergeFn), LoopPredicate (runLoopPredicate)
--
-- Types that have Show but contain non-Show children:
--   FrameKind — contains AgentLocalState (Show) but also WorkflowLocalState
--   (no Show) and LoopFrameState (no Show, contains LoopSpec with no Show).
--
-- For the stepping UI and debugging, at minimum FrameKind needs Show.
-- That requires Show on WorkflowLocalState, LoopFrameState, LoopSpec,
-- WorkflowProg, MergePolicy (contains Agent, no Show), Workflow.
--
-- FIX: Either add Show where possible (may require Show on Agent) or
-- implement custom Show instances that elide function/opaque fields.
-- Alternatively, use the StackView / peekNextAction approach for UI and
-- skip Show entirely. But for development/debugging, Show is valuable.


-- ============================================================================
-- ISSUE 12: MergePolicy contains Agent — impacts Show, Eq, serialization
-- ============================================================================
--
-- data MergePolicy = ... | MergeWithAgent Agent | MergeCustomNamed Text
--
-- Agent contains [Tool], and Tool contains function fields (toolExecute).
-- This means MergePolicy can't derive Show or Eq.
-- In Generate4, MergeCustom uses a RankN function field.
--
-- If MergePolicy needs Show/Eq (for testing, logging, serialization),
-- consider replacing MergeWithAgent with MergeWithAgentNamed Text and
-- resolving via a registry, similar to MergeCustomNamed.


-- ============================================================================
-- ISSUE 13: WorkflowProg.WProgNode wraps raw Workflow — unclear purpose
-- ============================================================================
--
-- data WorkflowProg = ... | WProgNode Workflow
--
-- This "compiled" program variant wraps an uncompiled AST node. It's
-- presumably the catch-all for atomic nodes (WRunAgent, WHandoff,
-- WSubagent), but then the scheduler handler for WProgNode must
-- pattern-match the inner Workflow again, defeating the purpose of
-- compilation.
--
-- Consider replacing WProgNode with explicit atomic variants:
--   | WProgAgent AgentNodeInput
--   | WProgHandoff HandoffSpec NodeId
--   | WProgSubagent SubagentSpec NodeId


-- ============================================================================
-- ISSUE 14: Missing helper function signatures (needed in types module
-- or companion modules)
-- ============================================================================
--
-- The impl doc references these functions that need type definitions:

-- Stack primitives (§4):
-- initialStack    :: Env -> StackConfig -> AgentLocalState -> Stack
-- pushFrame       :: Stack -> Frame -> Either StackError Stack
-- popFrame        :: Stack -> PopResult -> Either StackError Stack
-- replaceTopFrame :: Stack -> Frame -> Stack
-- setTopStep      :: Stack -> Step -> Stack
-- updateTopAgent  :: Stack -> (AgentLocalState -> AgentLocalState) -> Stack
-- topFrame        :: Stack -> Frame
-- topEnv          :: Stack -> Maybe Env

-- Env operations (§5):
-- mkEnv     :: Agent -> ModelWithFallbacks -> RuntimeArgs -> (...) -> Env
-- forkEnv   :: Env -> EnvOverrides -> EnvStore -> (EnvStore, EnvId)
-- lookupEnv :: EnvStore -> EnvId -> Maybe Env

-- Core interpreter (§3):
-- reduceStep :: StackRuntime -> Stack -> IO (ReduceOutcome Stack)
-- runStack   :: StackRuntime -> Stack -> IO (ReduceOutcome Stack)
-- resumeUser :: StackRuntime -> Stack -> UserGateOutcome -> IO (ReduceOutcome Stack)

-- Agent steps (§6):
-- checkAbort       :: Stack -> [Turn] -> Usage -> IO (Maybe GenerateErrorResult)
-- checkMaxRounds   :: Stack -> AgentLocalState -> IO (Maybe GenerateErrorResult)
-- callModel        :: Stack -> EnvId -> [Turn] -> IO (Either GenerateError ChatResponse)
-- executeOneTool   :: StackRuntime -> Stack -> ToolCall -> IO (Either GenerateError ToolStepResult)
-- applyToolResults :: AgentLocalState -> PendingToolRound -> AgentLocalState
-- failGeneration   :: Stack -> GenerateErrorResult -> IO (ReduceOutcome Stack)
-- finishSuccess    :: Stack -> GenerateTextResult -> IO (ReduceOutcome Stack)

-- Workflow (§9):
-- compileWorkflow       :: Workflow -> WorkflowProg
-- resolveWorkflowInput  :: Stack -> WorkflowInput -> [Turn]
-- startWorkflowFrame    :: Stack -> WorkflowProg -> Stack
-- mergeWorkflowResults  :: MergePolicy -> [WorkflowNodeResult] -> IO WorkflowNodeResult
-- evalLoopExit          :: StackRuntime -> LoopSpec -> LoopFrameState -> Stack -> Bool
-- workflowLoopView      :: LoopFrameState -> Stack -> WorkflowLoopView

-- Public API (§1):
-- generateText5 :: ... -> IO (ReduceOutcome Stack)
-- streamText5   :: ... -> IO (ReduceOutcome Stack)
-- peekNextAction :: Stack -> StackView


-- ============================================================================
-- ISSUE 15: ReduceOutcome derives Functor — is fmap over Stack useful?
-- ============================================================================
--
-- ReduceOutcome derives Functor. In practice a = Stack. Functor lets you
-- fmap a pure transformation over the stack in all three cases. This could
-- be useful (e.g. for attaching stLastStep after each reduction), so it's
-- probably fine to keep. Low priority, just noting it.


-- ============================================================================
-- QUESTION 1: Should Stack carry a global step counter?
-- ============================================================================
--
-- The impl doc §3.3 shows runStack maintaining a local step counter `n`
-- against scMaxTotalSteps. But if execution is interrupted (ReduceBlocked)
-- and resumed later, the local counter resets. Consider adding a
-- stTotalSteps :: Int field to Stack so the counter persists across
-- block/resume cycles.


-- ============================================================================
-- QUESTION 2: Event emission — where does it happen?
-- ============================================================================
--
-- The impl doc §12 says to call emitEvent from the env of the frame that
-- owns the action. But reduceStep is IO, so events can be emitted inline.
-- Should events be emitted inside reduceStep, or should ReduceOutcome carry
-- pending events and let the caller emit? Inline is simpler and matches
-- Generate4. But for testing (pure stepping), carrying events would be
-- useful. This is an architectural decision, not a type issue.


-- ============================================================================
-- QUESTION 3: Abort signal — per-frame or global?
-- ============================================================================
--
-- StackConfig has scSharedAbortSignal :: Maybe AbortSignal.
-- RuntimeArgs (on Env) has rtAbortSignal :: Maybe AbortSignal.
-- When checking abort, which one takes priority? Presumably either/or
-- (any abort = abort). But if a child frame overrides Env (via forkEnv
-- with different RuntimeArgs), the child's env might have a different
-- abort signal. The impl should check BOTH the global config signal and
-- the current frame's env signal.


-- ============================================================================
-- SUMMARY OF REQUIRED TYPE CHANGES
-- ============================================================================
--
-- MUST change (blocking):
--   1. Env.envCall return type: GenerateTextResult → ChatResponse
--   2. Add UTool, ToolOutcome, AgentCommand, UToolRegistry types
--   3. Add StackRuntime record
--   4. Add NodeId to WSubagent and WHandoff
--
-- SHOULD change (important for correctness):
--   5. Add StackError type
--   6. Add ToolStepResult type
--   7. Add stTotalSteps :: Int to Stack
--   8. Replace WProgNode with explicit atomic variants
--
-- NICE to have (debugging/UI):
--   9. Add StackView, NextAction, FrameView types
--  10. Add Show instances where possible (or custom ones)
--  11. Clarify TranscriptSummaryOnly vs TranscriptIsolated
--  12. Consider MergeWithAgentNamed instead of MergeWithAgent
