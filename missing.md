# Missing / Partial in `Generate4`

This is a snapshot of what is currently missing, stubbed, or only partially implemented in `src/LLM/Agent/Generate4.hs`.

---

## 1) Event system gaps

- **`emitOrchestrationEvent` is a no-op**
  - Current implementation: `emitOrchestrationEvent _rt _detail = pure ()`.
  - Effect: all orchestration lifecycle events (`SubagentStarted`, `DialogTurn`, etc.) are silently dropped.

- **`OrchestrationEventDetail` is disconnected from `GenerateEventDetail`**
  - You define orchestration event types, but they are not emitted through the existing `rtOnEvent` pipeline.

---

## 2) Command handling is incomplete

- **`AgentCommand` lacks workflow command support**
  - `RunWorkflowCommand Workflow` is not present yet.
  - Current commands: `PushSubagent`, `HandoffTo`, `PushSteps`, `PopWith`, `RunDialogCommand`, `FailCommand`.

- **`PopWith` is stubbed**
  - `PopWith _summary -> pure (Right machine)` does nothing.

- **`FailCommand` is stubbed**
  - `FailCommand _err -> pure (Right machine)` does nothing.

---

## 3) Subagent/workflow resume semantics are partial

- **No dedicated resume for workflow command**
  - Resume points only include `ResumeAfterSubagent`, `ResumeAfterTools`, `ResumeAfterDialog`.
  - If workflow commands are added, resume protocol still needs definition.

- **`resumeParentStep` logic is simplistic**
  - `ResumeAfterSubagent` maps all non-target calls to `toolResult c (summarizeForParent pop)`, which is likely wrong (it overwrites unrelated call results with the same summary text).

---

## 4) `PopResult` / transcript policy behavior is only partly modeled

- **`popResultFromSuccess` does not differentiate `TranscriptIsolated` vs `TranscriptShared`**
  - Both currently return full `success.gtrNewMessages`.
  - If shared vs isolated semantics should differ, this is incomplete.

- **`popToTextResult` uses `nilUuid`**
  - Synthetic result UUID handling is placeholder-like; may be fine for internal transforms but not ideal for user-visible lineage.

---

## 5) Workflow engine has placeholder behavior

- **`MergeCustom` is not implemented**
  - It raises: `error "MergeCustom: supply merge via MergeConcat for now"`.

- **`MergeWithAgent` is not implemented as true reducer**
  - It currently falls back to `mergeConcat` (ignores reducer agent intent).

- **`runSeqWorkflow` seed value is artificial**
  - Starts fold with an aborted dummy result and empty node id (`""`), which can pollute context and node tracking.

- **`workflowNodeId` is coarse for `RunAgent/Seq/Par`**
  - Static ids like `"run-agent"`, `"seq"`, `"par"` are reused, so contexts/maps may collide for multi-node workflows.

---

## 6) Dialog implementation is basic / provisional

- **`dsSummarizer` is ignored**
  - Branch for `Just _agent` currently returns same fallback behavior as `Nothing`.

- **`dialogLoop` uses unsafe fallback**
  - `fromMaybe (error "env") (machineTopEnv machine)` in event emission path.
  - This can crash instead of producing a structured error.

---

## 7) Error modeling still generic in several places

- **Unknown UTool names are not explicit errors**
  - Current behavior in tool execution path can yield `"Unknown tool: ..."` content, but there is no dedicated `GenerateError` variant for UTool resolution failures.

- **Several failures collapse to `GErrAborted`**
  - Used as fallback for non-abort cases (e.g., machine-level fallback paths), reducing diagnosability.

---

## 8) UTool registration/exposure mismatch risks

- You now have:
  - `UToolRegistry`
  - `getResolvedUTools`
  - `createGenRequestU` including UTool defs
  - runtime merged UTool execution in `ExecTools`.
- Still worth validating:
  - Unknown `agUTools` names are currently ignored by `getResolvedUTools` (lookup via `mapMaybe`).
  - If you want fail-hard behavior, this is not implemented yet.

---

## 9) `runInterpreterStep` remains mostly utility-level

- `Log` step currently ignores logger (`Log _lvl _msg rest -> ...`).
- It works as control flow shim but not as full policy/event layer.

---

## 10) API / architecture consistency debt

- `generateText` and `generateTextMachine` both emit `GenerationStarted`, risking duplicate start events depending on entrypoint path.
- `WorkflowContext.wcResults` is reset in `workflowContextFromMachine`; only per-run updates in workflow code are retained.

---

## Specifically on your example (`popResultFromSuccess`)

Yes, your suspicion is valid:

- `TranscriptIsolated` and `TranscriptShared` currently behave identically.
- If those policies are meant to differ semantically, `popResultFromSuccess` is not fully implemented.

---

## Suggested implementation order (from this missing list)

1. Wire orchestration events (`emitOrchestrationEvent`) into real observer pipeline.
2. Implement `RunWorkflowCommand` and command-specific resume semantics.
3. Implement `PopWith` and `FailCommand`.
4. Fix `resumeParentStep` result merging for multi-tool-call rounds.
5. Implement proper `MergeWithAgent` and `MergeCustom`.
6. Finalize transcript policy behavior in `popResultFromSuccess`.
7. Replace `error` fallbacks with typed error paths.

