# Implementing `LLM.Agent.Generate4`

Step-by-step guide for replacing every `undefined` in `Generate4.hs`.  
**Reference implementation:** `Generate3.hs` (single-env interpreter), `Generate.hs` (events), `ToolUtils.hs` (tools).

**Principle:** Pure `ChatStep` encodes control flow; `runMachine` / `runStep` own IO, abort, max rounds, hooks, and events. Stack + commands come in later phases.

---

## Implementation phases (order)

| Phase | Goal | Functions / types |
|-------|------|-------------------|
| **0** | Helpers & event bridge | `emitOrchestrationEvent` (new, internal), empty `EnvStore` helpers |
| **1** | Env store | `mkEnv`, `lookupEnv`, `forkEnv` |
| **2** | Machine stack (pure) | `initialMachine`, `topFrame`, `machineDepth`, `pushFrame`, `popFrame`, `replaceTopFrame` |
| **3** | Pure program | `buildChatStep`, `buildAgentStep` |
| **4** | Core interpreter (1 frame) | `checkAbort`, `checkMaxRounds`, `failGeneration`, `finishSuccess`, `callModel`, `runToolsWithAbortChecks`, `execTools`, `runStep`, `runFrame`, `runMachine` |
| **5** | Public API (parity with Generate3) | `generateText`, `streamText`, `generateTextMachine` |
| **6** | Legacy tools | `toolExecuteLegacy`, `executeToolOutcome` |
| **7** | Commands & stack | `applyAgentCommand`, `summarizeForParent`, extend `runStep` for `StepPush` / `StepHandoff` |
| **8** | Subagent & handoff | `runSubagent`, `runHandoff` |
| **9** | Dialog | `runDialog`, `ChatStep` `RunDialog` in `runStep` |
| **10** | Workflows | `runWorkflow`, `runPipe`, `runParallel` (sequential stub for `Par` first), `compileWorkflowToSteps`, `compileWorkflowToMachine`, `RunWorkflow` in `runStep` |
| **11** | Optional | `runInterpreterStep`, `InterpreterStep` layer |

Do not start phase 7 until phase 4 tests pass against `Generate3` behavior.

---

## Phase 0 — Internal helpers

### `emitOrchestrationEvent` (add to `Generate4`, not exported initially)

```haskell
emitOrchestrationEvent :: RuntimeArgs -> OrchestrationEventDetail -> IO ()
```

**How:** Mirror `LLM.Agent.Events.emitEvent`: wrap detail in a new `GenerateEvent` variant **or** map orchestration events to existing `GenerateEventDetail` where possible. Until `Types.hs` is extended, either:

- Add `GenerateEventDetail` constructors in `LLM.Agent.Types`, **or**
- Store orchestration-only events in a separate observer field on `RuntimeArgs` (requires type change).

**Recommendation:** Extend `GenerateEventDetail` in `Types.hs` with the `OrchestrationEventDetail` cases (or a single `OrchestrationEvent OrchestrationEventDetail` wrapper) so one `rtOnEvent` stream suffices.

---

## Phase 1 — Environment store

### `mkEnv`

**Input:** `agent`, `models`, `rt`, `call` (LLM invocation).

**Output:** `Env` with `envId = 0` (caller assigns real id when inserting into store).

**Logic:**

```haskell
mkEnv agent models rt call =
  Env { envId = 0, envAgent = agent, envModels = models, envRt = rt, envCall = call }
```

`envCall` is the only difference between `generateText` and `streamText` (fallbacks vs stream).

### `lookupEnv`

**Logic:** `Map.lookup envId esMap` on `EnvStore`.

### `forkEnv`

**Input:** parent `Env`, `EnvOverrides`, current `EnvStore`.

**Output:** `(store', newEnvId)` where `newEnvId = esNextId store`, and `store'` maps `newEnvId` to child env.

**Logic:**

1. `newId = esNextId store`
2. Build child: copy parent fields, apply `eoAgent`, `eoModels`, `eoRt` with `fromMaybe` parent values
3. Set `envId = newId` on child
4. `insert newId child (esMap store)`, increment `esNextId`

---

## Phase 2 — Machine stack (pure)

### `initialMachine`

**Input:** root `Env` (with `envId` already assigned), initial `ChatStep`, `MachineConfig`.

**Logic:**

1. `envStore = EnvStore { esNextId = envId + 1, esMap = insert envId env empty }` — if root `env.envId` is 0, store starts at 1 for next fork
2. `frame0 = Frame { frId = 0, frEnvId = env.envId, frStep = step, frResume = Nothing }`
3. `Machine { mEnvStore = envStore, mStack = frame0 :| [], mNextFrameId = 1, mDepth = 1, mMaxDepth = mcMaxStackDepth config, mGlobalUsage = mempty, mRootGenerationId = env.envRt.rtGenerationId }`

### `topFrame`

**Logic:** `NonEmpty.head mStack`.

### `machineDepth`

**Logic:** `mDepth` (keep in sync on push/pop) or `length mStack`.

### `pushFrame`

**Logic:**

1. If `mDepth >= mMaxDepth` → `Left StackOverflow`
2. Else push frame onto stack, `mDepth + 1`, `mNextFrameId` bumped if `frId` assigned from machine (allocate `frId = mNextFrameId` when creating child frame in interpreter, not here)

**Note:** Caller usually builds `Frame` with id `mNextFrameId` before calling `pushFrame`.

### `popFrame`

**Input:** `machine`, `PopResult` (child’s result for parent).

**Logic:**

1. If stack has only one frame → `Left EmptyStack`
2. Pop top; **parent** is now `head` of remaining stack
3. Do **not** apply `PopResult` here — interpreter applies it when resuming parent (see phase 7). This function only shrinks stack and decrements `mDepth`.
4. Add child `prUsage` to `mGlobalUsage` with `<>`

**Type issue:** `PopResult` in `popFrame` is unused in a minimal design. Consider changing signature to `popFrame :: Machine -> Either MachineError Machine` and passing `PopResult` only to resume helpers. See [Issues](#issues-and-recommended-type-changes).

### `replaceTopFrame`

**Logic:** `mStack` with head replaced by new frame (same `frId` or new id for handoff — use new id only if you treat handoff as new frame).

---

## Phase 3 — Pure `ChatStep` builders

Copy logic from `Generate3.buildAgentStep`, with these deltas:

### `buildChatStep`

**Parameters:** `envId`, `currentTurns`, `accTurns`, `usage`, `loopCount`.

**Problem:** `Done` success needs `rtGenerationId` but `ChatStep` has no `RuntimeArgs` on `CallModel`.

**Fix (pick one):**

- Add `csGenerationId :: UUID` to `CallModel` / `ExecTools`, **or**
- Pass `UUID` as extra argument to `buildChatStep`, **or**
- Only build `Done` in `buildAgentStep` (wrapper).

**Recommended:** `buildAgentStep` calls `buildChatStep` after resolving env:

```haskell
buildAgentStep agent rt current acc usage n =
  buildChatStep (envIdForAgent rt) ...  -- see buildAgentStep below
```

For `ExecTools`, set `csEnvId = envId` (no agent/rt on step — interpreter loads from store).

**`afterModel` / `afterTools`:** Same as Generate3: `getToolCalls`, usage monoid, `AssistantTurn` / `ToolTurn`, tail-call `buildChatStep` on tool success.

### `buildAgentStep`

**Logic:**

1. `envId` is **not** in store yet at build time for top-level API — use `0` and ensure `initialMachine` inserts env at `0`, **or** build step **after** `initialMachine` with known id.
2. Practical approach: `generateText` does **not** use `buildAgentStep` alone; it uses:

   ```haskell
   env = mkEnv ...; env' = env { envId = 0 }
   step = buildChatStep 0 turns [] mempty 0  -- needs UUID for Done: add rt to wrapper
   ```

3. Wrap `buildChatStep` with `rt` for `gtrGenerationId` in `Done`:

   ```haskell
   buildAgentStep agent rt cur acc usage n =
     patchGenerationId rt.rtGenerationId $
       buildChatStep 0 cur acc usage n
   ```

   where `patchGenerationId` walks `ChatStep` and sets id in `Done` / or pass `UUID` into `buildChatStep`.

**Exact algorithm for `afterModel`:** Identical to `Generate3` lines 128–182, replacing `ExecTools` agent/rt with `csEnvId`.

---

## Phase 4 — Core interpreter

### `checkAbort`

**Logic:** Read abort from **top frame’s env**: `lookupEnv` → `envRt.rtAbortSignal` → `isAbortedMaybe`. On abort: `Left (GenerateErrorResult GErrAborted acc usage)`.

Use `mcSharedAbortSignal` from config if you add `MachineConfig` to machine; else per-env signal.

### `checkMaxRounds`

**Logic:**

- `limit = fromMaybe (envAgent.agMaxToolRounds) mcMaxToolRoundsPerFrame` from machine config + top env’s agent
- If `loopCount >= limit` → `Left (GenerateErrorResult GErrToolExceeded acc usage)`

### `failGeneration`

**Logic:** `emitEvent` `GenerationFailed` on **top frame** `envRt`; return `Left errResult`. Do not pop stack.

### `finishSuccess`

**Logic:** Compute `finalTurn` from `gtrNewMessages` (same as Generate3); emit `MessageFinalized`, `GenerationFinished`; return `Right`.

### `callModel`

**Logic:** `lookupEnv mEnvStore envId`; on `Nothing` treat as `Left GErrAllModelsFailed` or add `MachineError` path; else `envCall env envAgent envModels envRt turns`.

### `runToolsWithAbortChecks`

**Logic:** Port from `Generate3.runToolsWithAbortChecks` (sequential). Use top-frame env’s `rtHooks`. Between tools: `checkAbort machine partialAcc usage`. Call `executeToolOutcome` once phase 6 exists; until then `executeTool` + wrap as `ToolReply`.

### `execTools`

**Logic:** `lookupEnv` → `createToolContext`, `getResolvedTools` → `runToolsWithAbortChecks`. On each result, if phase 7: handle `ToolCommand` via `applyAgentCommand` before continuing.

### `runStep`

**Type:** `IO (StepOutcome Machine)` — drives one **constructor** then may recurse via `runMachine`.

| Constructor | Behavior |
|-------------|----------|
| `Done r` | If stack depth 1: `finishSuccess` or `failGeneration` → `StepFinished`. If depth > 1: pop child result, resume parent (phase 7). |
| `CallModel{..}` | `checkAbort` → `checkMaxRounds` → `callModel` → `StepContinue machine { top frStep = csOnModelResult result }` **or** inline `runStep machine (csOnModelResult result)` |
| `ExecTools{..}` | Abort check; emit `MessageCreated` assistant; `ToolRoundStarted`; `execTools`; on success emit tool turn + `ToolRoundFinished`; `runStep` with `csOnToolsResult` |
| `RunDialog` | Delegate `runDialog` then continue (phase 9) |
| `RunWorkflow` | `runWorkflow` then continue (phase 10) |

**Phase 4 simplification:** Only stack depth 1; `Done` → `StepFinished`. No `StepPush` yet.

### `runFrame`

**Logic:** `runStep machine (frStep frame)`.

### `runMachine`

**Logic:** Loop until `StepFinished`:

```text
loop machine:
  outcome <- runFrame machine (topFrame machine)
  case outcome of
    StepFinished r -> pure r
    StepContinue m' -> loop m'
    StepPush fr m' -> pushFrame m' fr >>= loop
    StepHandoff fr m' -> replaceTopFrame m' fr >> loop  -- or pop+push per policy
```

Emit `GenerationStarted` once at entry (`generateTextMachine` / `generateText`).

---

## Phase 5 — Public entry points

### `generateText`

1. `env = mkEnv agent models rt (\a m r t -> generateTextWithFallbacks (createGenRequest a r t) m)`
2. `env' = env { envId = 0 }`
3. `step = buildAgentStep agent rt turns [] emptyUsage 0`
4. `machine = initialMachine env' step defaultMachineConfig`
5. `generateTextMachine machine`

### `streamText`

Same with `streamTextWithFallbacks onChunk ...` in `envCall`.

### `generateTextMachine`

1. `emitEvent` on root env `GenerationStarted` (lookup env 0 from store)
2. `runMachine machine`

### `defaultMachineConfig`

Add locally: `mcMaxStackDepth = 32`, `mcMaxToolRoundsPerFrame = Nothing`, `mcSharedAbortSignal = Nothing`.

---

## Phase 6 — Tool outcomes

### `toolExecuteLegacy`

**Logic:** `Tool.toolExecute ctx val` (existing `IO Text`) → `ToolReply text`.

### `executeToolOutcome`

**Logic:**

1. Run legacy execute (or new `toolExecute` when `Tool` type changes)
2. Map to `ToolOutcome`
3. Build `ToolResult` via `toolResult tc` when outcome is `ToolReply` or `ToolReplyAndCommand` (model-visible text)
4. On exception: `Left` or reply with error text (match `ToolUtils.executeTool`)

**Command-only:** `ToolCommand cmd` → no `ToolResult` yet (`Nothing`) until command applied; interpreter must apply before feeding model.

---

## Phase 7 — Commands and stack dynamics

### `summarizeForParent`

**Logic:** Default `prSummary`; optional append structured JSON from `prStructured`.

### `applyAgentCommand`

| Command | Effect |
|---------|--------|
| `PushSubagent spec` | `forkEnv` child env from spec; `buildChatStep` for `ssInitialTurns`; new `Frame` with child step; `pushFrame`; emit `SubagentStarted` |
| `HandoffTo spec` | `runHandoff` |
| `PushSteps steps` | Push frame whose step is first step (encode list as `RunChat` internal or fold into seq) |
| `PopWith summary` | Build `PopResult`; `popFrame`; resume parent |
| `RunDialogCommand spec` | Push `RunDialog spec (...)` frame |
| `FailCommand err` | `failGeneration` with error |

Return `Right machine'` or `Left StackOverflow` / `EnvNotFound`.

### Extend `runStep` / `runMachine` for multi-frame

**On `Done` with depth > 1:**

1. Build `PopResult` from success (summary = `gtrText`, usage, new turns per policy)
2. `popFrame machine popResult`
3. Load parent `ResumePoint`:
   - `ResumeAfterSubagent`: inject `ToolResult` for `rpToolCallId`, continue parent `ExecTools` continuation or rebuild tool turn
   - `ResumeAfterTools`: continue with tool results already applied

**On `ExecTools` with command from tool:** `StepPush` instead of immediate `csOnToolsResult`.

Set `frResume` when pushing child from `ExecTools` (`ResumeAfterSubagent`).

---

## Phase 8 — Subagent and handoff

### `runSubagent`

**Logic:**

1. `forkEnv` from current top env + `EnvOverrides` from spec (`eoAgent = Just ssAgent`, etc.)
2. `childMachine = initialMachine childEnv (buildChatStep newId ssInitialTurns [] mempty 0) config`
3. Emit `SubagentStarted`
4. `runMachine childMachine`
5. On success: `PopResult { prSummary = gtrText, prUsage = gtrUsage, prNewTurns = per ssTranscriptPolicy, ... }`
6. Emit `SubagentFinished`

**TranscriptPolicy:**

- `TranscriptIsolated`: `prNewTurns` = child new messages only
- `TranscriptShared`: include in parent conversation
- `TranscriptSummaryOnly`: `prNewTurns = []`, summary in `prSummary` only

### `runHandoff`

**Logic:**

1. Build target env from `hsTarget` + `hsContextMode` (copy/window/summary turns into `ssInitialTurns`)
2. If `hsReplaceStack`: `replaceTopFrame` with new frame, new agent step, clear resume
3. Else: `pushFrame` (suspend parent)
4. Emit `HandoffStarted` / on complete `HandoffFinished`

---

## Phase 9 — Dialog

### `runDialog`

**Logic:**

1. Emit `DialogStarted`
2. State: `transcript = dsSeedTurns`, `usage = mempty`, `round = 0`
3. Loop `round < dsMaxRounds`:
   - Call model for A with topic + transcript → assistant turn
   - Emit `DialogTurn frameId round turnA`
   - Call model for B with B’s system + transcript + turnA → turnB
   - Emit `DialogTurn`
   - Append to transcript
4. If `dsSummarizer`: one more `CallModel` to summarize; else concat last turns
5. `DialogSummary { dsText, dsTranscript, dsUsage }`
6. Emit `DialogFinished`

Use separate `Env` values for A and B (`forkEnv` with different agents/models).

### `runStep` `RunDialog spec k`

1. `runDialog machine spec`
2. On `Right summary`: `runStep machine (k summary)`
3. On `Left err`: `StepFinished (Left ...)`

---

## Phase 10 — Static workflows

### `runWorkflow`

Recursive on `Workflow`:

| Node | Action |
|------|--------|
| `RunAgent inp` | Resolve `aniInput` → turns; `generateText` with ani fields; wrap `WorkflowResult` |
| `Seq wfs` | `foldM` runWorkflow, pass prior output as `WInputFromPrior` via context map |
| `Par wfs pol` | **v1:** `runParallel` sequential + merge (no concurrency) |
| `Dialog wd` | `runDialog` with `wdSpec` |
| `Handoff wh` | `runHandoff` on empty/single-frame machine built from context |
| `Subagent ws` | `runSubagent` |

Emit `WorkflowNodeStarted` / `WorkflowNodeFinished`.

**Context:** Add `Map WorkflowNodeId WorkflowResult` to `WorkflowContext` (type change) for `WInputFromPrior`.

### `runPipe`

**Logic:** `runWorkflow (Seq wfs) ctx`.

### `runParallel`

**v1:** `mapM (runWorkflow) wfs` then `mergeResults pol`.

**Merge:**

- `MergeConcat`: concatenate successful `gtrText` with newlines
- `MergeFirstSuccess`: first `Right` in list
- `MergeWithAgent`: run reducer agent with merged input (new `generateText` call)
- `MergeCustom f`: `f results`

### `compileWorkflowToSteps`

**v1:** Only `RunAgent` → `buildAgentStep`; `Seq` → embed as nested `RunWorkflow` or left-to-right `RunWorkflow` chain. `Par` / `Dialog` / `Handoff` → `RunWorkflow node` compound step.

### `compileWorkflowToMachine`

**Logic:** `initialMachine env (compileWorkflowToSteps wf) config` — requires fixed `env` for root `RunAgent`.

### `generateTextWorkflow`

**Logic:** `runWorkflow wf ctx` (no stack unless workflow uses subagent).

---

## Phase 11 — `InterpreterStep` (optional)

### `runInterpreterStep`

| Step | Action |
|------|--------|
| `CheckAbort k` | `isAbortedMaybe` on top env → `k aborted` → recurse |
| `Log lvl msg rest` | If hooks support log level, emit; else no-op → recurse |
| `Throttle ms rest` | `threadDelay ms` → recurse |
| `RunChat chat` | `runMachine machine { top step = chat }` |

Prefer **not** using this in production; keep policy in `runStep` directly (as Generate3).

---

## Function checklist (alphabetical)

| Function | Phase | Depends on |
|----------|-------|------------|
| `applyAgentCommand` | 7 | `pushFrame`, `forkEnv`, `runHandoff`, `runDialog` |
| `buildAgentStep` | 3 | `buildChatStep` |
| `buildChatStep` | 3 | — |
| `callModel` | 4 | `lookupEnv` |
| `checkAbort` | 4 | `lookupEnv`, `isAbortedMaybe` |
| `checkMaxRounds` | 4 | `lookupEnv` |
| `compileWorkflowToMachine` | 10 | `compileWorkflowToSteps`, `initialMachine` |
| `compileWorkflowToSteps` | 10 | `buildAgentStep` |
| `execTools` | 4/6 | `runToolsWithAbortChecks`, `executeToolOutcome` |
| `executeToolOutcome` | 6 | `toolExecuteLegacy` |
| `failGeneration` | 4 | `emitEvent` |
| `finishSuccess` | 4 | `emitEvent` |
| `forkEnv` | 1 | — |
| `generateText` | 5 | `generateTextMachine`, `buildAgentStep`, `initialMachine` |
| `generateTextMachine` | 5 | `runMachine` |
| `generateTextWorkflow` | 10 | `runWorkflow` |
| `initialMachine` | 2 | — |
| `lookupEnv` | 1 | — |
| `machineDepth` | 2 | — |
| `mkEnv` | 1 | — |
| `popFrame` | 2/7 | — |
| `pushFrame` | 2 | — |
| `replaceTopFrame` | 2 | — |
| `runDialog` | 9 | `callModel`, events |
| `runFrame` | 4 | `runStep` |
| `runHandoff` | 8 | `forkEnv`, `replaceTopFrame` |
| `runInterpreterStep` | 11 | `runMachine` |
| `runMachine` | 4 | `runFrame`, `pushFrame` |
| `runParallel` | 10 | `runWorkflow` |
| `runPipe` | 10 | `runWorkflow` |
| `runStep` | 4+ | all step constructors |
| `runSubagent` | 8 | `runMachine`, `forkEnv` |
| `runToolsWithAbortChecks` | 4 | `executeToolOutcome` |
| `runWorkflow` | 10 | `generateText`, `runDialog`, … |
| `streamText` | 5 | same as `generateText` |
| `summarizeForParent` | 7 | — |
| `toolExecuteLegacy` | 6 | `Tool.toolExecute` |
| `topFrame` | 2 | — |

---

## Testing strategy

1. **Golden parity:** `generateText` Generate4 vs Generate3 on mock/no-tool conversation.
2. **Abort:** signal during `CallModel` and between tools.
3. **Max rounds:** exceed `agMaxToolRounds`.
4. **Stack:** tool returns `PushSubagent`, child completes, parent gets one `ToolTurn`.
5. **Workflow:** `Seq` of two `RunAgent` nodes.

---

## Issues and recommended type changes

### 1. `popFrame :: Machine -> PopResult -> ...`

`PopResult` is the **child output**, not an input to popping. **Change to:**

```haskell
popFrame :: Machine -> Either MachineError Machine
resumeParent :: Machine -> PopResult -> ResumePoint -> ChatStep
```

Interpreter calls `popFrame` then `resumeParent`.

### 2. `buildChatStep` lacks `RuntimeArgs` / `UUID` for `Done`

`GenerateTextResult` needs `gtrGenerationId`. **Add parameter:**

```haskell
buildChatStep :: UUID -> EnvId -> [Turn] -> [Turn] -> Usage -> Int -> ChatStep
```

### 3. `Env.envId` in `mkEnv`

Always set when inserting into store; document that `mkEnv` leaves `envId = 0` as placeholder.

### 4. `DialogSummary` vs `DialogSpec` field names

Both use `ds*` prefix (`dsText` vs `dsAgentA`). Legal in Haskell but confusing. **Rename** `DialogSummary` fields to `dgsText`, `dgsTranscript`, `dgsUsage`.

### 5. `Tool` type still `IO Text`

`ToolExecute` exists only in Generate4. **Change `LLM.Agent.Types`:**

```haskell
toolExecute :: ToolContext -> Value -> IO ToolOutcome
```

Provide `toolFromLegacy :: (ToolContext -> Value -> IO Text) -> Tool` adapter. Update `ToolUtils.executeTool` to handle `ToolOutcome` and commands (or call `executeToolOutcome`).

### 6. `OrchestrationEventDetail` vs `GenerateEventDetail`

Observers only know `GenerateEventDetail`. **Merge** orchestration constructors into `Types.hs` **or** add:

```haskell
| Orchestration OrchestrationEventDetail
```

and map in `emitEvent`.

### 7. `WorkflowContext` missing node results

`WInputFromPrior` needs `Map WorkflowNodeId WorkflowResult`. **Add:**

```haskell
wcResults :: Map WorkflowNodeId WorkflowResult
```

Update `runWorkflow` to `insert` after each node.

### 8. `StepOutcome` vs `IO (Either ...)`

`generateTextMachine` could return `StepFinished` only; ensure `runMachine` never returns `StepPush` to caller without handling. **Document:** public API collapses to `Either GenerateErrorResult GenerateTextResult`.

### 9. `ResumePoint` unused in phase 4

Fine for MVP; required before subagent pop. **Set `frResume`** in `applyAgentCommand` `PushSubagent`.

### 10. `ExecTools` without agent in step

Interpreter must `lookupEnv csEnvId` for agent/rt. **On missing env:** `StepFinished (Left ...)` with `GErrAllModelsFailed` or `MachineError`.

### 11. `MergeCustom` RankN

Hard to serialize and test. **Add** `MergeWithIO ([WorkflowResult] -> IO WorkflowResult)` for production; keep `MergeCustom` for embedded DSLs.

### 12. `MachineConfig.mcSharedAbortSignal`

If set, override per-env `rtAbortSignal` in `checkAbort` for all frames.

### 13. `HandoffSpec.hsReplaceStack`

If `True`, parent frame is discarded — parent `ResumePoint` never used. Document UX: user thread continues as new agent only.

### 14. `compileWorkflowToSteps` and `Par`

Compiling `Par` to a single `ChatStep` requires `RunWorkflow` or a new `RunParallel` constructor. **Add:**

```haskell
| RunParallel [Workflow] MergePolicy (WorkflowResult -> ChatStep)
```

### 15. Global usage

Roll up child `gtrUsage` into `mGlobalUsage` on `popFrame` or `StepFinished` at depth 1.

### 16. `executeToolOutcome` return type

`Either GenerateError (...)` vs tool errors as text in `ToolResult`. **Align with** `ToolUtils`: exceptions → error text in result; only command failures use `Left`.

### 17. `PushSteps [ChatStep]`

Running a list needs a small sequencer:

```haskell
data ChatStep = ... | SeqSteps [ChatStep] (() -> ChatStep)  -- or fold to nested RunChat
```

Or push one frame per step — stack depth cost.

---

## Minimal first milestone

Ship when this works:

- [ ] `generateText` / `streamText` match Generate3
- [ ] `EnvStore` + single frame
- [ ] `buildAgentStep` = Generate3 logic
- [ ] Events: `GenerationStarted`, `Failed`, `Finished`, tool rounds, messages

Defer: `Workflow`, `Dialog`, `Handoff`, `ToolCommand`, `InterpreterStep`, `Par` concurrency.

---

## File touch list

| File | Changes |
|------|---------|
| `Generate4.hs` | Implement all functions |
| `LLM.Agent.Types` | `ToolOutcome` import or move; extend `GenerateEventDetail`; optional `Tool` signature |
| `LLM.Agent.ToolUtils` | `executeToolOutcome` wrapper; command handling hook |
| `LLM.Agent.Events` | Optional `emitOrchestrationEvent` |
| Tests | New `Generate4Spec.hs` parity with Generate3 |
