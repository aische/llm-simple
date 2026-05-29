# Implementing `LLM.Agent.Generate5`

High-level implementation guide for the stack-machine interpreter: `reduceStep`, supporting functions, and staged rollout. Types live in `src/LLM/Agent/Generate5.hs`; behavior should be ported from `Generate4.hs` where possible, not re-invented.

**Reference code:** `Generate4.hs` (stack, tools, subagent, workflow), `Generate3.hs` / `Generate.hs` (single-frame agent loop), `ToolUtils.hs`, `Events.hs`.

**Principle:** One `reduceStep` performs **exactly one** of: (1) one LLM call, (2) one tool execution, (3) one stack transformation (push/pop/replace/fork), or (4) pure bookkeeping (abort check, loop exit check, workflow advance). `runStack` loops until `ReduceFinished` or `ReduceBlocked`.

---

## 1. Target public API

Add to `Generate5` exports when each phase is ready:

| Symbol | Role |
|--------|------|
| `reduceStep :: Stack -> IO (ReduceOutcome Stack)` | Single reduction on **top frame** |
| `runStack :: Stack -> IO (ReduceOutcome Stack)` | Loop `reduceStep` until finished/blocked |
| `resumeUser :: Stack -> UserGateOutcome -> IO (ReduceOutcome Stack)` | Continue after `ReduceBlocked` |
| `initialStack :: Env -> StackConfig -> AgentLocalState -> Stack` | Root agent, depth 1 |
| `peekNextAction :: Stack -> StackView` | Pure preview for stepping UI (no IO) |
| `compileWorkflow :: Workflow -> WorkflowProg` | AST → scheduler program |
| `mkEnv`, `forkEnv`, `lookupEnv` | Same semantics as Generate4 |
| `pushFrame`, `popFrame`, `replaceTopFrame` | Pure stack ops + depth/usage |
| `generateText5`, `streamText5` | Build `Env` + `initialStack` + `runStack` |

Keep **registries** on `Stack` or a separate `StackRuntime` record:

```haskell
data StackRuntime = StackRuntime
  { srUTools :: UToolRegistry          -- from Generate4 pattern
  , srLoopPredicates :: Map Text LoopPredicate
  , srMergeFns :: Map Text MergeFn
  }
```

Either embed in `Stack` or pass `StackRuntime` into `reduceStep` (cleaner for pure `Stack` snapshots).

---

## 2. Module layout (recommended)

Avoid a 2000-line `Generate5.hs`. Suggested split:

```
src/LLM/Agent/Generate5.hs          -- types (current file)
src/LLM/Agent/Generate5/Env.hs      -- EnvStore, forkEnv, lookupEnv
src/LLM/Agent/Generate5/Stack.hs    -- push/pop/replace, topFrame, lenses
src/LLM/Agent/Generate5/Agent.hs    -- agent step transitions, callModel, tools
src/LLM/Agent/Generate5/Command.hs  -- ToolOutcome → stack commands (subagent, etc.)
src/LLM/Agent/Generate5/Workflow.hs -- compile, scheduler steps, merge, loop
src/LLM/Agent/Generate5/Reduce.hs   -- reduceStep dispatch + runStack
```

Re-export from `LLM.Agent.Generate5` for a stable import path. Types can stay in one file until the interpreter stabilizes.

---

## 3. Core dispatch: `reduceStep`

### 3.1 Top-level shape

```haskell
reduceStep :: StackRuntime -> Stack -> IO (ReduceOutcome Stack)
reduceStep rt st = do
  st' <- tickGlobalLimits rt st          -- optional pure+IO: total steps, usage, abort
  case st' of
    Left finished -> pure finished
    Right st1 -> do
      let fr = topFrame st1
      outcome <- reduceFrame rt st1 fr
      pure (attachStepKind outcome)
```

`reduceFrame` branches on `frKind` **and** `frStep` (program counter on frame):

```haskell
reduceFrame rt st fr = case fr.frKind of
  FAgent als -> reduceAgentStep rt st fr als
  FWorkflow wls -> reduceWorkflowStep rt st fr wls
  FLoop lfs -> reduceLoopStep rt st fr lfs
```

**Invariant:** Only the **top** frame runs IO (LLM/tools). Lower frames are suspended (`frPause` / waiting for child).

### 3.2 Outcomes

| `ReduceOutcome` | When |
|-----------------|------|
| `Stepped st` | State advanced; call `reduceStep` again |
| `ReduceBlocked st gate` | Top `frStep == StepAwaitUser gate`; no change until `resumeUser` |
| `ReduceFinished errOrOk` | Root agent `StepFinish` with final result, or unrecoverable error |

Do **not** conflate `StepFinish` on a **child** frame with global finish — that should transition to `StepPopChild` or workflow `StepWfChildDone` first.

### 3.3 `runStack`

```haskell
runStack rt st = go 0 st
  where
    go n st
      | Just limit <- rt.stConfig.scMaxTotalSteps, n >= limit =
          pure (ReduceFinished (Left (toolExceededPartial st)))
      | otherwise = reduceStep rt st >>= \case
          Stepped st' -> go (n + 1) st'
          blocked@(ReduceBlocked {}) -> pure blocked
          done@(ReduceFinished {}) -> pure done
```

---

## 4. Stack primitives (implement first)

Port from `Generate4` `initialMachine` / `pushFrame` / `popFrame` / `replaceTopFrame`, renaming `Machine` → `Stack`.

### `initialStack`

- Root `Env` in `stEnvStore.esMap` at `env.envId`.
- Single `Frame` with `FAgent als`, `frStep = StepCheckAbort` (or `StepCallModel` if you skip separate abort step).
- `stNextFrameId = 1`, `stDepth = 1`, `stGlobalUsage = mempty`, `stNodeResults = mempty`.

### `pushFrame`

- Fail with `StackOverflow` analogue if `stDepth >= scMaxStackDepth`.
- Prepend frame, increment `stDepth`, allocate `frId = stNextFrameId` and bump.

### `popFrame`

- Input: `PopResult` from child.
- Pop top; add `popUsage` to `stGlobalUsage`.
- **Do not** auto-resume parent — set parent's `frStep` explicitly in `StepPopChild` / workflow handlers.

### `replaceTopFrame`

- Handoff: swap top frame, keep rest of stack and depth unchanged.

### Helpers

```haskell
setTopStep :: Stack -> Step -> Stack
updateTopAgent :: Stack -> (AgentLocalState -> AgentLocalState) -> Stack
topEnv :: Stack -> Maybe Env
```

---

## 5. Environment (`Env.hs`)

Same as Generate4 Phase 1:

- `mkEnv` — `envId = 0` until inserted.
- `forkEnv parent overrides store -> (store', childId)` — `fromMaybe` for overrides, `insert` child, bump `esNextId`.
- `lookupEnv` — `Map.lookup`.

`envCall` type in Generate5 is `IO (Either GenerateError GenerateTextResult)` (slightly simpler than G4's `GenerateResult ChatResponse`). Adapter when porting:

```haskell
callModelFromEnv env turns = do
  res <- env.envCall env.envAgent env.envModels env.envRt turns
  -- map to ChatResponse-like fields inside Agent module if needed
```

Prefer one internal `ChatResponse` view in `Agent.hs` so tool-round logic matches G4's `buildChatStep`.

---

## 6. Stage 1 — Agent loop (`reduceAgentStep`)

### 6.1 Step transition table

| `frStep` | IO? | Action | Next `frStep` |
|----------|-----|--------|----------------|
| `StepCheckAbort` | IO | `isAborted` → fail partial or continue | `StepCallModel` |
| `StepCallModel` | **LLM** | `callModel`; no tools → build final; tools → pending round | `StepRunTool firstId` or `StepFinish` |
| `StepRunTool tcId` | **Tool** | `executeOneTool`; reply → next index; command → see §7 | `StepRunTool next` / `StepPushSubagent` / … |
| `StepApplyToolResults` | pure | Append assistant+tool turns, `alsLoopCount + 1` | `StepCheckAbort` or `StepCallModel` |
| `StepFinish` | pure/events | If depth==1 → `ReduceFinished`; else → `StepPopChild` | — |

### 6.2 After `StepCallModel` (port `buildChatStep` logic)

On `Right` response:

1. Merge usage into `alsUsage`.
2. If no tool calls: set `alsAccTurns`, `StepFinish`, emit `MessageFinalized` / `GenerationFinished` when root.
3. If tool calls: create `PendingToolRound` with `ptrNextIndex = 0`, assistant turn saved, `frStep = StepRunTool (head calls).tcId`.

On `Left`: `GenerateErrorResult` → root `ReduceFinished` or child pop path.

### 6.3 `StepRunTool` (one tool per step)

Port `runToolsWithAbortChecks` **one iteration**:

1. `checkAbort` / `checkMaxRounds` (use `alsLoopCount`, `scMaxToolRoundsPerFrame`, agent `agMaxToolRounds`).
2. Resolve UTool by name (registry + legacy tools from G4).
3. `executeToolOutcome` → `ToolOutcome`:
   - `ToolReply` → store `ToolResult`, increment index; if more calls → `StepRunTool next`; else → `StepApplyToolResults`.
   - `ToolCommand cmd` → **do not** run another tool in same step; set `frStep` to command step or apply command as **next** reduction (see §7).

### 6.4 Functions to implement (Stage 1)

| Function | Notes |
|----------|--------|
| `checkAbort :: Stack -> [Turn] -> Usage -> IO (Maybe GenerateErrorResult)` | Shared abort signal from config + env |
| `checkMaxRounds :: Stack -> AgentLocalState -> IO (Maybe GenerateErrorResult)` | `GErrToolExceeded` |
| `callModel :: Stack -> EnvId -> [Turn] -> IO (Either GenerateError ChatResponse)` | Wrap `envCall` |
| `executeOneTool :: StackRuntime -> Stack -> ToolCall -> IO (Either GenerateError ToolStepResult)` | |
| `applyToolResults :: AgentLocalState -> PendingToolRound -> AgentLocalState` | Clear pending, extend turns |
| `failGeneration :: Stack -> GenerateErrorResult -> IO (ReduceOutcome Stack)` | Emit `GenerationFailed` |
| `finishSuccess :: Stack -> GenerateTextResult -> IO (ReduceOutcome Stack)` | Emit `GenerationFinished` |
| `agentLocalFromFinish :: AgentLocalState -> Either GenerateErrorResult GenerateTextResult` | For `StepFinish` |

### 6.5 `AgentPhase` vs `Step`

Optional: `agentPhase :: AgentLocalState -> Step -> AgentPhase` for `peekNextAction`. Runtime should use `frStep` as source of truth.

---

## 7. Stage 2 — Subagent

### 7.1 Flow

1. During `StepRunTool`, tool returns `ToolCommand (PushSubagent spec)`.
2. **One stack step** (`StepPushSubagent spec` or inline in tool handler):
   - Save `ParentPause (PausedForSubagent tcId pendingRound)` on **parent** frame.
   - `forkEnv` with overrides from `spec`.
   - `pushFrame` child: `FAgent` with `buildInitialAgentState spec`, `frStep = StepCheckAbort`, `frReturn = KNone`.
3. Child runs until `StepFinish` → parent not finished globally:
   - Set top to `StepPopChild pop` where `pop = popResultFromChild spec result`.
4. **One stack step** `StepPopChild pop`:
   - `popFrame` child usage into stack.
   - Parent becomes top: merge `ToolResult` for `ppsToolCallId`, continue `StepRunTool` for remaining calls.

### 7.2 `popResultFromChild`

Port `popResultFromSuccess` / `popResultFromError` from G4 with `TranscriptPolicy`:

| Policy | `popSummary` | `popNewTurns` |
|--------|--------------|---------------|
| `TranscriptSummaryOnly` / `Isolated` | child final text | `[]` |
| `TranscriptShared` | child final text | child messages |

### 7.3 Functions

| Function | Notes |
|----------|--------|
| `pushSubagentFrame :: StackRuntime -> Stack -> ToolCallId -> SubagentSpec -> IO Stack` | Sets pause + push |
| `resumeParentToolRound :: Stack -> PopResult -> ParentPause -> Stack` | Inject result, advance tool index |
| `popResultFromChild :: SubagentSpec -> Either ... -> PopResult` | |

**Stack search:** Default rule — only the **top** frame may complete. No `FrameId` search on happy path.

---

## 8. Stage 3 — Handoff

### 8.1 Semantics

- **Not** a pop-back: old top agent is discarded (when `hsReplaceStack = True`).
- **One reduction:** `StepHandoff spec` → pure stack + env:
  - `turns = handoffTurns st spec` (port G4 `handoffTurns`: full / summary / window over parent's `alsCurrentTurns`).
  - `forkEnv` for `hsTarget`.
  - `replaceTopFrame` with new agent frame, fresh `AgentLocalState`, merged turns, `frPause = Nothing`, `frStep = StepCheckAbort`.

### 8.2 `handoffTurns`

```text
HandoffFullTranscript  → parent current turns ++ ssInitialTurns
HandoffSummary t       → UserTurn t : ssInitialTurns
HandoffWindow n        → drop (len-n) parent turns ++ ssInitialTurns
```

### 8.3 Functions

| Function | Notes |
|----------|--------|
| `handoffTurns :: Stack -> HandoffSpec -> [Turn]` | Read parent `alsCurrentTurns` from frame below or from replaced frame before replace |
| `applyHandoff :: Stack -> HandoffSpec -> Stack` | |

When `hsReplaceStack = False`, use `pushFrame` instead (buried parent); document as non-default.

---

## 9. Stage 4 — Workflows

**Critical:** Do **not** call `generateText` / new `initialStack` per workflow node (G4 `runAgentNode` anti-pattern). Run each node as **child agent frame(s)** on the **same** stack, with a **workflow scheduler** frame underneath.

### 9.1 Compile AST → `WorkflowProg`

```haskell
compileWorkflow :: Workflow -> WorkflowProg
-- WSeq ws   -> WProgSeq (map nodeId ws)
-- WPar ws p -> WProgPar (map nodeId ws) p
-- WLoop s   -> WProgLoop s
-- WRunAgent n -> WProgNode (WRunAgent n)
-- etc.
```

Assign stable `NodeId` from `aniNodeId` / `loopNodeId` / generated ids for anonymous inner nodes (`"seq-0"`, `"par-1"`).

### 9.2 Scheduler frame on stack

While workflow runs, stack often looks like:

```text
[ FAgent (node B running)     ]  ← top: reduceStep does LLM/tools here
[ FWorkflow scheduler state   ]
[ FAgent (root caller)        ]  ← optional, if workflow started from tool
```

### 9.3 Scheduler steps

| `frStep` | Kind | Action |
|----------|------|--------|
| `StepWfAdvance` | pure | Move `WorkflowPc`, decide next node or merge/complete |
| `StepWfStartNode nid` | stack | Resolve input from `stNodeResults`, `pushFrame` agent child |
| `StepWfChildDone nid pop` | stack | `popFrame`, insert `WorkflowNodeResult` into `wlsNodeResults` + `stNodeResults` |
| `StepWfMerge pol` | pure/IO | Combine branch results (`MergeConcat`, etc.) |

### 9.4 `WSeq`

`WorkflowPc = PcSeq { pcSeqIndex, pcSeqPhase }`:

- `SeqBetween` → pick `nodeIds !! index`, `StepWfStartNode`, set `SeqRunning`.
- Child completes → `StepWfChildDone` → increment index or finish seq.
- Last node done → pop workflow frame or set `ReduceFinished` if root workflow.

### 9.5 `WPar` (phase 4a: sequential)

For stepping/debug, run branches **one at a time** on the same stack:

- `ParBranchState { pbsCurrent, pbsResults, ... }`
- Start branch i → push agent → on done store in `pbsResults` → i+1.
- When all done → `StepWfMerge`.

True concurrent Par (multiple children) is phase 4b: requires `FrameId`/`NodeId` on completion and is **not** “pop once = parent”.

### 9.6 `WLoop` — termination

Loop frame: `FLoop LoopFrameState` or `PcLoop` inside `FWorkflow`.

**Per iteration (after body completes):**

1. `StepLoopCheck` (pure): build `WorkflowLoopView`.
2. Stop if `lfsIteration >= loopMaxIterations` (**always** enforced).
3. Else evaluate `loopExit`:
   - `LoopExitOnMaxOnly` → continue only if below max (already handled).
   - `LoopExitOnSuccess` → stop if `lfsLastResult` is `Right _`.
   - `LoopExitOnError e` → stop if child error matches.
   - `LoopExitWhenNamed key` → `srLoopPredicates[key] view`.
4. If continuing: `StepLoopStartIteration` → apply `loopCarry`:
   - `LoopCarryNone` — same input.
   - `LoopCarryFromPrior nid` — next body reads `WInputFromPrior nid` from `stNodeResults`.
   - `LoopCarryAcc accNid` — merge prior text into `stNodeResults` under `accNid`.
5. Push compiled body again; increment `lfsIteration`.

**Dialog pattern:** `WLoop (defaultLoopSpec (WSeq [agentA, agentB]) "dialog")` with `loopMaxIterations = dsMaxRounds`.

### 9.7 Functions

| Function | Notes |
|----------|--------|
| `compileWorkflow :: Workflow -> WorkflowProg` | |
| `resolveWorkflowInput :: Stack -> WorkflowInput -> [Turn]` | Port G4 `resolveWorkflowInput` |
| `startWorkflowFrame :: Stack -> WorkflowProg -> Stack` | Push `FWorkflow` |
| `mergeWorkflowResults :: MergePolicy -> [WorkflowNodeResult] -> IO WorkflowNodeResult` | Port G4 merge helpers |
| `evalLoopExit :: StackRuntime -> LoopSpec -> LoopFrameState -> Stack -> Bool` | Pure except named predicate map |
| `workflowLoopView :: LoopFrameState -> Stack -> WorkflowLoopView` | |

### 9.8 Merge policies

| Policy | Behavior |
|--------|----------|
| `MergeConcat` | Join successful branch texts (G4 `mergeConcat`) |
| `MergeFirstSuccess` | First `Right` wins |
| `MergeWithAgent` | Push reducer agent frame with prompt listing branch outputs; one `StepCallModel`; treat as extra node |
| `MergeCustomNamed k` | `srMergeFns[k]` |

---

## 10. Stage 5 — User confirmation

### 10.1 Blocking

When top `frStep = StepAwaitUser gate`:

```haskell
reduceStep _ st
  | StepAwaitUser gate <- topStep st =
      pure (ReduceBlocked st gate)
```

No state change; host shows UI.

### 10.2 `resumeUser`

```haskell
resumeUser rt st outcome = case (topStep st, outcome) of
  (StepAwaitUser gate, GateContinue (UserContinue txt _)) ->
    -- apply: e.g. append UserTurn txt, StepCallModel
    reduceStep rt (appliedStack st txt)
  (StepAwaitUser _, GateContinue UserDiscard) ->
    reduceStep rt (discardStack st)
  (StepAwaitUser _, GateTimeout) ->
    failGeneration st GErrAborted ...
```

Wire gates from tools: `ToolCommand (AwaitUser ...)` → set `StepAwaitUser` on next step.

Optional: workflow node `WConfirm UserGate` in AST (future).

---

## 11. Tool commands (`Command.hs`)

Port `AgentCommand` handling from G4 `applyAgentCommand'`:

| Command | Stage | `reduceStep` effect |
|---------|-------|---------------------|
| `PushSubagent spec` | 2 | `StepPushSubagent` / push child |
| `HandoffTo spec` | 3 | `StepHandoff` |
| `PushSteps` | 4 | Push frame with compiled steps (or expand to agent) |
| `RunWorkflowCommand wf` | 4 | `startWorkflowFrame (compileWorkflow wf)` |
| `PopWith summary` | 2+ | Child initiates pop with text |
| `RunDialogCommand` | 4 | Prefer `WLoop` instead; or compile dialog to loop |
| `FailCommand err` | 1 | `failGeneration` |

Map tool outcomes in **one tool step**; command application may be the **following** pure/stack step if you want strict “one effect per `reduceStep`”.

---

## 12. Events and hooks

Port from G4 / `Events.hs`:

- `GenerationStarted` / `GenerationFinished` / `GenerationFailed` on root only.
- `MessageCreated` on assistant (before tools) and tool turns.
- `ToolRoundStarted` / `ToolRoundFinished` per `alsLoopCount`.
- Orchestration events: extend `GenerateEventDetail` or use `emitOrchestrationEvent` stub until wired.

Call `emitEvent` from `env.envRt.rtOnEvent` on the **env of the frame that owns the action** (top frame's `frEnvId`).

---

## 13. `peekNextAction` (stepping UI)

Pure function:

```haskell
data StackView = StackView
  { svDepth :: Int
  , svFrames :: [FrameView]
  , svNext :: NextAction
  }

data NextAction
  = NextLLM | NextTool Text | NextPure Text | NextBlocked UserGate | NextDone
```

Derive from top `frStep` + `frKind` without IO.

---

## 14. Implementation phases (order)

| Phase | Deliverable | Test |
|-------|-------------|------|
| **G5-0** | Stack/Env primitives, `initialStack`, `setTopStep` | unit: push/pop depth |
| **G5-1** | `reduceAgentStep` stages 1 (model, one tool, apply, finish) | port `ChatSpec` tool loop tests |
| **G5-2** | Subagent push/pop + tool resume | UTool1-style subagent |
| **G5-3** | Handoff replace + context modes | handoff turns count |
| **G5-4a** | `compileWorkflow`, `WSeq`, `stNodeResults` | two-agent seq |
| **G5-4b** | `WPar` sequential + `MergeConcat` | UTool2-like par |
| **G5-4c** | `WLoop` + `evalLoopExit` + carry | max iter + early success |
| **G5-5** | `StepAwaitUser` + `resumeUser` | blocked → continue |
| **G5-6** | `generateText5` / `streamText5` | parity with `generateText` |

Do not start G5-4 until G5-2 pop/resume is correct.

---

## 15. `reduceStep` dispatch cheat sheet

```text
top.frKind:
  FAgent als →
    StepCheckAbort     → pure/IO guard → StepCallModel
    StepCallModel      → LLM
    StepRunTool id     → one tool
    StepApplyToolResults → pure
    StepPushSubagent   → stack + fork
    StepPopChild pop   → pop + resume parent
    StepHandoff spec   → replace + fork
    StepAwaitUser      → ReduceBlocked
    StepFinish         → finished or pop-child setup

  FWorkflow wls →
    StepWfAdvance / StepWfStartNode / StepWfChildDone / StepWfMerge

  FLoop lfs →
    StepLoopCheck / StepLoopStartIteration / StepLoopEndIteration
    (or delegate to inner Wf steps while LoopPhaseRunningBody)
```

---

## 16. Known gaps / differences from Generate4

| G4 behavior | G5 plan |
|-------------|---------|
| CPS `ChatStep` continuations | Explicit `frStep` + `AgentLocalState` |
| `ExecTools` runs all tools in one `runStep` | One tool per `reduceStep` |
| `runWorkflow` spawns new machine | Same stack, scheduler frame |
| `RunDialog` + `dialogLoop` | `WLoop (WSeq [A,B])` |
| `frResume` on parent | `ParentPause` + `ReturnKont` (equivalent) |
| `rpParentFrameId` unused for lookup | Keep for validation/logging only |
| `MergeWithAgent` stubbed | Optional reducer agent frame |

---

## 17. Acceptance criteria

1. **Stepping:** REPL can call `reduceStep` N times, printing `peekNextAction` / stack after each call.
2. **Granularity:** Each LLM or tool invocation is exactly one `reduceStep` (commands may add one stack-only step).
3. **Single stack:** UTool2 `Par [Seq [...], Loop ...]` visible as scheduler + child frames, not nested `generateText`.
4. **Loop safety:** `loopMaxIterations` always stops; `scMaxTotalSteps` optional backstop.
5. **Handoff:** Top agent replaced; transcript per `HandoffContext`.
6. **Subagent:** Parent receives one `ToolResult` per spawning call; other tool calls in the same round unaffected.

---

## 18. File checklist for implementer

- [ ] Remove `undefined` from `reduceStep` in `Generate5.hs`; move to `Generate5/Reduce.hs`
- [ ] Add `UToolRegistry` type alias or import from G4
- [ ] Add `StackError` type (`StackOverflow`, `EmptyStack`, `EnvNotFound`)
- [ ] Port tests from `test/LLM/ChatSpec.hs` against `generateText5`
- [ ] Document stepping REPL in `app/` (optional `StepMain.hs`)

This document is the source of truth for completing the Generate5 interpreter; update it when types in `Generate5.hs` change.
