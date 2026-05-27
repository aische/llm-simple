# Agent orchestration — proposed architecture

Design notes for evolving `Generate3` toward a stack-based machine with dynamic tool commands, plus static workflow orchestration (pipes, parallel, dialog, handoff).

**Layers (bottom to top):**

1. **Agent machine** — one agent loop (`ChatStep` / `AgentStep`, interpreted per frame).
2. **Dynamic orchestration** — stack, tool commands, subagent / handoff at runtime.
3. **Static orchestration** — `Workflow` AST (seq, par, dialog) compiled or interpreted without model-chosen topology.

Policy (abort, logging, events, hooks) stays in the **interpreter**, not in pure steps.

---

## Identifiers and environment store

```haskell
type EnvId = Int
type FrameId = Int
type WorkflowNodeId = Text
```

```haskell
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
```

```haskell
mkEnv ::
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  (Agent -> ModelWithFallbacks -> RuntimeArgs -> [Turn] -> IO (GenerateResult ChatResponse)) ->
  Env

forkEnv :: Env -> EnvOverrides -> EnvStore -> (EnvStore, EnvId)

lookupEnv :: EnvStore -> EnvId -> Maybe Env
```

```haskell
data EnvOverrides = EnvOverrides
  { eoAgent :: Maybe Agent,
    eoModels :: Maybe ModelWithFallbacks,
    eoRt :: Maybe RuntimeArgs
  }
```

---

## Frame and machine

A **frame** is one stack entry: which env to use and which step program to run.

```haskell
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
```

```haskell
data Machine = Machine
  { mEnvStore :: EnvStore,
    mStack :: NonEmpty Frame,
    mNextFrameId :: FrameId,
    mDepth :: Int,
    mMaxDepth :: Int,
    mGlobalUsage :: Usage,
    mRootGenerationId :: UUID
  }
```

```haskell
initialMachine ::
  Env ->
  ChatStep ->
  MachineConfig ->
  Machine

pushFrame :: Machine -> Frame -> Either MachineError Machine

popFrame :: Machine -> PopResult -> Either MachineError Machine

replaceTopFrame :: Machine -> Frame -> Machine

topFrame :: Machine -> Frame

machineDepth :: Machine -> Int
```

```haskell
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
```

---

## ChatStep (pure agent program, CPS)

Continuation-passing steps: **no IO, no abort checks, no logging**.  
(Current `Generate3` uses `AgentStep` with the same shape; `ChatStep` is the target name.)

```haskell
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
```

Optional **compound** steps (orchestration as a single frame instruction):

```haskell
  | RunDialog DialogSpec (DialogSummary -> ChatStep)
  | RunWorkflow Workflow (WorkflowResult -> ChatStep)
```

```haskell
buildChatStep ::
  EnvId ->
  [Turn] ->
  [Turn] ->
  Usage ->
  Int ->
  ChatStep

buildAgentStep ::
  Agent ->
  RuntimeArgs ->
  [Turn] ->
  [Turn] ->
  Usage ->
  Int ->
  ChatStep
```

---

## Interpreter-only steps (optional)

If abort / log / throttle appear as **step constructors**, the interpreter must implement them; they do not belong in the pure `buildChatStep` program.

```haskell
data InterpreterStep
  = CheckAbort (Bool -> InterpreterStep)
  | Log LogLevel Text InterpreterStep
  | Throttle Int InterpreterStep
  | RunChat ChatStep
```

```haskell
runInterpreterStep :: Machine -> InterpreterStep -> IO (Either GenerateErrorResult GenerateTextResult)
```

Preferred split: keep `ChatStep` pure; implement abort / events / hooks only in `runMachine` / `runStep`.

---

## Step interpreter (machine driver)

```haskell
runMachine :: Machine -> IO (Either GenerateErrorResult GenerateTextResult)

runStep :: Machine -> ChatStep -> IO (StepOutcome Machine)

runFrame :: Machine -> Frame -> IO (StepOutcome Machine)

data StepOutcome m
  = StepContinue m
  | StepFinished (Either GenerateErrorResult GenerateTextResult)
  | StepPush Frame m
  | StepHandoff Frame m
```

```haskell
checkAbort :: Machine -> [Turn] -> Usage -> IO (Either GenerateErrorResult ())

checkMaxRounds :: Machine -> Int -> [Turn] -> Usage -> IO (Either GenerateErrorResult ())

failGeneration ::
  Machine ->
  GenerateErrorResult ->
  IO (Either GenerateErrorResult GenerateTextResult)

finishSuccess ::
  Machine ->
  GenerateTextResult ->
  IO (Either GenerateErrorResult GenerateTextResult)
```

```haskell
callModel ::
  Machine ->
  EnvId ->
  [Turn] ->
  IO (GenerateResult ChatResponse)

execTools ::
  Machine ->
  EnvId ->
  ToolContext ->
  [Tool] ->
  [ToolCall] ->
  IO (GenerateResult [ToolResult])

runToolsWithAbortChecks ::
  Machine ->
  [Turn] ->
  Usage ->
  [Tool] ->
  ToolContext ->
  [ToolCall] ->
  IO (GenerateResult [ToolResult])
```

---

## Tool results and commands

Tools return either normal text (for the model) or a command for the interpreter.

```haskell
data ToolOutcome
  = ToolReply Text
  | ToolCommand AgentCommand
  | ToolReplyAndCommand Text AgentCommand
```

```haskell
data AgentCommand
  = PushSubagent SubagentSpec
  | HandoffTo HandoffSpec
  | PushSteps [ChatStep]
  | PopWith Text
  | RunDialogCommand DialogSpec
  | FailCommand GenerateError
```

```haskell
executeToolOutcome ::
  Machine ->
  ToolContext ->
  Tool ->
  ToolCall ->
  IO (Either GenerateError (ToolOutcome, Maybe ToolResult))

applyAgentCommand ::
  Machine ->
  AgentCommand ->
  IO (Either MachineError Machine)

summarizeForParent :: PopResult -> Text
```

```haskell
type ToolExecute = ToolContext -> Value -> IO ToolOutcome

-- Migration from current API:
toolExecuteLegacy :: ToolContext -> Value -> IO Text
```

---

## Subagent, handoff, dialog specs

```haskell
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
```

```haskell
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
```

```haskell
data TranscriptPolicy
  = TranscriptIsolated
  | TranscriptShared
  | TranscriptSummaryOnly
```

```haskell
runSubagent :: Machine -> SubagentSpec -> IO (Either GenerateError PopResult)

runHandoff :: Machine -> HandoffSpec -> IO (Either MachineError Machine)

runDialog :: Machine -> DialogSpec -> IO (Either GenerateError DialogSummary)
```

```haskell
data PopResult = PopResult
  { prSummary :: Text,
    prStructured :: Maybe Value,
    prUsage :: Usage,
    prNewTurns :: [Turn]
  }
```

---

## Static workflow orchestration

Graph known at compile / config time. May compile to `Machine` pushes or call `runWorkflow` directly.

```haskell
data Workflow
  = RunAgent AgentNodeInput
  | Seq [Workflow]
  | Par [Workflow] MergePolicy
  | Dialog WorkflowDialog
  | Handoff WorkflowHandoff
  | Subagent WorkflowSubagent
```

```haskell
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
```

```haskell
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
```

```haskell
data MergePolicy
  = MergeConcat
  | MergeFirstSuccess
  | MergeWithAgent Agent
  | MergeCustom (forall m. Monad m => [WorkflowResult] -> m WorkflowResult)
```

```haskell
data WorkflowResult = WorkflowResult
  { wrNodeId :: WorkflowNodeId,
    wrOutput :: Either GenerateErrorResult GenerateTextResult,
    wrUsage :: Usage
  }
```

```haskell
runWorkflow :: Workflow -> WorkflowContext -> IO WorkflowResult

runPipe :: [Workflow] -> WorkflowContext -> IO WorkflowResult

runParallel :: [Workflow] -> MergePolicy -> WorkflowContext -> IO WorkflowResult
```

```haskell
data WorkflowContext = WorkflowContext
  { wcAbortSignal :: Maybe AbortSignal,
    wcOnEvent :: EventObserver,
    wcHooks :: Hooks,
    wcLLMHooks :: LLMHooks
  }

compileWorkflowToSteps :: Workflow -> ChatStep

compileWorkflowToMachine :: Env -> Workflow -> Machine
```

---

## Events (extensions)

```haskell
data GenerateEventDetail
  = -- ... existing ...
  | SubagentStarted FrameId SubagentSpec
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
```

---

## Public entry points (target API)

```haskell
generateText ::
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)

streamText ::
  (StreamChunk -> IO ()) ->
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)

generateTextWorkflow :: Workflow -> WorkflowContext -> IO WorkflowResult

generateTextMachine :: Machine -> IO (Either GenerateErrorResult GenerateTextResult)
```

---

## Generate2 (free monad variant, reference)

```haskell
data AgentF next
  = CallModel { ... :: GenerateResult ChatResponse -> next }
  | RunTools { ... :: GenerateResult [ToolResult] -> next }

data Free f a = Pure a | Free (f (Free f a))

buildProgram :: ... -> Free AgentF (Either GenerateErrorResult GenerateTextResult)

interpret :: Env -> Free AgentF a -> IO a
```

---

## Implementation order (suggested)

1. `EnvStore` + `Frame` + `Machine` + `runMachine` (single frame, same behavior as `Generate3`).
2. `ToolOutcome` / `AgentCommand` + `PushSubagent` / `PopWith`.
3. `HandoffSpec` + `replaceTopFrame`.
4. `DialogSpec` + `runDialog` (static and command-driven).
5. `Workflow` + `runPipe` / `runParallel`.
6. Optional `InterpreterStep` layer only if you want explicit `Log` / `Throttle` in the step language.

---

## Mapping from current code

| Current (`Generate3`) | Proposed |
|----------------------|----------|
| `AgentStep` | `ChatStep` (rename; add `csEnvId` on `ExecTools`) |
| `Env` (single, outside step) | `EnvStore` + `Frame.frEnvId` |
| `runStep env step` | `runMachine` / `runFrame` |
| `toolExecute :: ... -> IO Text` | `toolExecute :: ... -> IO ToolOutcome` |
| `buildAgentStep` | `buildChatStep` with env id, or `buildAgentStep` wrapper |

---

## Changes needed for parallel tool calls and parallel (sub)agents

Today, tool calls in a single model round run **sequentially** (`executeToolsWithAbort` / `runToolsWithAbortChecks` process one `ToolCall` at a time, with abort checks between them). Subagents are not implemented; the stack model assumes **one active frame**. Parallelism requires explicit execution policy, ordering, cancellation, and merge semantics.

### Parallel tool calls (within one `ExecTools` step)

**Current behavior:** `[ToolCall]` → sequential `executeTool` → `[ToolResult]` in call order (reversed then fixed in the sequential loop).

**What to add**

```haskell
data ToolExecutionPolicy
  = ToolExecSequential
  | ToolExecParallel
  | ToolExecParallelBounded Int   -- max concurrent tools per round
  | ToolExecGrouped [(Text, ToolExecutionPolicy)]  -- by tool name
```

```haskell
executeToolsConcurrent ::
  Machine ->
  ToolExecutionPolicy ->
  ToolContext ->
  [Tool] ->
  [ToolCall] ->
  IO (GenerateResult [ToolResult])

waitToolRound :: AsyncGroup -> IO (Either GenerateError [ToolResult])
```

**Interpreter (`ExecTools` / `execTools`)**

- Branch on `ToolExecutionPolicy` (from `Agent`, `RuntimeArgs`, or `MachineConfig`).
- For parallel: spawn one `IO` action per `ToolCall` (e.g. `async`, `mapConcurrently` with a limiter).
- **Preserve result order** in the final `[ToolResult]` to match `asCalls` / `ToolCall` ids so the `ToolTurn` seen by the model is stable (reorder by `tcId` after join, not completion order).
- **Abort:** register all async handles in an `AsyncGroup`; on abort signal, cancel in-flight work and return `GErrAborted` without starting new tools. Sequential abort-between-tools is insufficient.
- **Hooks / events:** define whether `onToolCall` / `onToolResult` order is completion order or call order; document for observers.

**Tool implementations**

- `ToolContext` and shared resources (filesystem, env vars, in-memory state) may need **per-call isolation** or documented “parallel-safe” tools.
- Readonly mode already filters tools; add optional `toolParallelSafe :: Bool` on `ToolDef` to forbid parallel execution for mutating tools unless explicitly allowed.

**`ToolOutcome` / commands**

- If a tool returns `ToolCommand` (e.g. `PushSubagent`), parallel rounds can produce **multiple commands**. Policy required:
  - `CommandMergeFirst` — first completed command wins; cancel siblings.
  - `CommandMergeFail` — treat as error if more than one command.
  - `CommandQueue` — serialize command application on the interpreter thread after the round joins.
- Do not let two parallel tools `HandoffTo` or `PushSubagent` without a merge policy (undefined stack otherwise).

**`ChatStep`**

- Optional field on `ExecTools`: `csToolPolicy :: ToolExecutionPolicy` (default sequential until configured).

**Types / config (summary)**

| Area | Change |
|------|--------|
| `Agent` or `RuntimeArgs` | `toolExecutionPolicy` |
| `MachineConfig` | `mcMaxConcurrentTools` |
| `ToolDef` | optional `toolParallelSafe` |
| `ToolUtils` | `executeToolsConcurrent`, `AsyncGroup`, ordered join |
| Interpreter | parallel branch in `execTools`, cooperative cancel |
| Events | optional `ToolCallStarted` / `ToolCallFinished` per call with correlation id |

---

### Parallel (sub)agents

**Current gap:** one `Machine` stack, one `runMachine` loop, no fork/join of frames. `Workflow Par` is static-only in the doc and still needs a runtime story.

**What to add**

```haskell
data ParallelPolicy
  = ParWaitAll
  | ParWaitFirst
  | ParWaitAnyN Int
```

```haskell
data Branch = Branch
  { brId :: BranchId,
    brMachine :: Machine,
    brAsync :: Maybe (Async (Either GenerateError PopResult))
  }

data ParallelGroup = ParallelGroup
  { pgBranches :: [Branch],
    pgPolicy :: ParallelPolicy,
    pgMerge :: MergePolicy,
    pgParentFrameId :: FrameId
  }
```

```haskell
forkSubagent :: Machine -> SubagentSpec -> IO (BranchId, Async (Either GenerateError PopResult))

forkWorkflowBranch :: WorkflowContext -> Workflow -> IO (BranchId, Async WorkflowResult)

joinParallelGroup :: ParallelGroup -> IO (Either GenerateError JoinResult)

data JoinResult = JoinResult
  { jrResults :: [PopResult],
    jrMerged :: Text,
    jrUsage :: Usage
  }
```

**Machine / stack**

- **Option A — multiple machines:** each branch has its own `Machine` (own stack, env store, usage). Parent waits on `joinParallelGroup`. No shared stack during execution; merge on join. Simplest for isolation.
- **Option B — forest on one machine:** `Machine` gains `mChildren :: Map BranchId Machine` and parent pauses until children finish. Harder to reason about; prefer Option A first.

**`AgentCommand` / `Workflow`**

- `PushSubagent` stays sequential (one child) unless wrapped:
  - `Par [PushSubagent spec1, PushSubagent spec2]` in static `Workflow`.
  - New command: `ParSubagents [SubagentSpec] MergePolicy` for dynamic fan-out.
- Static `Workflow Par` compiles to `forkWorkflowBranch` + `joinParallelGroup` with `MergePolicy`.

**Interpreter**

- Parent frame **suspends** (push `WaitJoin ParallelGroup` frame or block inside `RunWorkflow` step) until branches complete.
- **Abort:** one shared `AbortSignal` cancels all branch asyncs; or per-branch signals linked to parent.
- **Budgets:** `mcMaxStackDepth` per branch; add `mcMaxParallelBranches` and global token/usage cap (`mGlobalUsage` already on `Machine` — roll up on join).
- **Events:** `SubagentStarted` / `SubagentFinished` per `BranchId`; `ParallelGroupStarted` / `ParallelGroupFinished`.

**Transcript / parent model**

- Parent must not interleave child transcripts blindly. On join, apply `MergePolicy`:
  - `MergeConcat` — summaries concatenated (cheap, noisy).
  - `MergeWithAgent` — reducer agent produces one `ToolResult` / user-visible block.
- Each branch uses `TranscriptPolicy` (`TranscriptIsolated` recommended for parallel).

**Dialog vs parallel**

- **Dialog** is sequential alternation (A→B→A→B); not parallel.
- **Parallel** is multiple independent agent runs; do not implement dialog as `Par` of two agents.

**Concurrency primitives**

- Use explicit `async` + `cancel` (or `withAsync`) per branch, not nested `runMachine` on the same stack without isolation.
- Cap concurrency at OS/thread pool level (`mcMaxParallelBranches`, `mcMaxConcurrentTools`) to avoid rate-limit storms on LLM APIs.

**Implementation order (parallel-specific)**

1. Parallel **tools** within one round (ordered join, abort group, sequential default).
2. `forkSubagent` + `join` for **two** branches with `MergeConcat` and isolated machines.
3. `Workflow Par` wired to the same join path.
4. `ToolOutcome` command merge policy when tools can push subagents.
5. Reducer merge agent and global usage/abort policies.

**Risks to address in design**

| Risk | Mitigation |
|------|------------|
| Nondeterministic `ToolTurn` order | Join by `ToolCall` id, not completion time |
| Double handoff / double push | Single command queue or fail on conflict |
| Token / rate spikes | Bounded concurrency + per-branch models |
| FS races from parallel tools | `toolParallelSafe` + default sequential for mutators |
| Debugging | `BranchId` on all events and logs |
