# Using UTools With `agUTools`

Goal: an agent declares command-capable tools by name in `agUTools`, and those tools can return workflow commands (`RunWorkflow`) that the `Generate4` interpreter executes.

This document describes the implementation plan, integration points, and open decisions.

---

## Current status

What is already in place:

- `Agent` has `agUTools :: [Text]` in `LLM.Agent.Types`.
- `Generate4` defines:
    - `UTool`
    - `ToolOutcome` (`ToolReply`, `ToolCommand`, `ToolReplyAndCommand`)
    - `AgentCommand` (`PushSubagent`, `PushSteps`, etc.)
- `Generate4` runs tool calls through `runToolsWithAbortChecks` and applies commands via `applyAgentCommand'`.

Key gap:

- In `runStep` (`ExecTools` branch), only legacy tools are used:
    - `tools = getResolvedTools ...`
    - `utools = map utTool tools`
- So `agUTools` names are not resolved yet.

---

## Target behavior

1. Register UTools in a runtime registry: `name -> UTool` (or builder).
2. Agent declares allowed UTools by name in `agUTools`.
3. At tool round execution, interpreter resolves those names from registry and merges with legacy tools.
4. Model can call those tools by name.
5. A UTool can return a command that runs a workflow:
    - either `ToolCommand (RunWorkflowCommand wf)`
    - or `ToolCommand (PushSteps [RunWorkflow wf k])`

---

## Implementation plan

## 1) Add a UTool registry type

Define in `Generate4`:

```haskell
type UToolRegistry = Map Text UTool
```

If you need late binding:

```haskell
type UToolBuilder = BuildCtx -> UTool
type UToolBuilderRegistry = Map Text UToolBuilder
```

`BuildCtx` can contain `ModelWithFallbacks`, available agents, and app-specific deps.

---

## 2) Thread registry through top-level API

Add API variants (keep existing ones for backward compatibility):

```haskell
generateTextWithUTools ::
  UToolRegistry ->
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)

streamTextWithUTools ::
  UToolRegistry ->
  (StreamChunk -> IO ()) ->
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
```

Put registry on machine/runtime state (recommended):

```haskell
data Machine = Machine
  { ...
    mUToolRegistry :: UToolRegistry
  }
```

This avoids threading the registry parameter through every helper manually.

---

## 3) Resolve `agUTools` names for each `ExecTools`

In `runStep` `ExecTools` branch:

1. Keep legacy conversion:
    - `legacyUTools = map utTool (getResolvedTools agent rt)`
2. Resolve named UTools:
    - `namedUTools = resolveAgentUTools machine env.envAgent.agUTools`
3. Merge:
    - `allUTools = mergeUTools legacyUTools namedUTools`
4. Pass `allUTools` into `runToolsWithAbortChecks`.

Add helpers:

```haskell
resolveAgentUTools :: Machine -> [Text] -> Either GenerateError [UTool]
mergeUTools :: [UTool] -> [UTool] -> [UTool]
```

Recommended merge rule:

- UTools from registry override same-name legacy tools.

---

## 4) Add command for running workflows directly

Currently `AgentCommand` has no workflow-specific constructor.

Add:

```haskell
data AgentCommand
  = ...
  | RunWorkflowCommand Workflow
```

Handle in `applyAgentCommand'`:

- convert to a stack step:
    - `PushSteps [RunWorkflow workflow k]`
- where `k` maps `WorkflowResult` to either:
    - next parent continuation, or
    - `Done` with transformed output.

This is clearer than requiring every tool to handcraft `PushSteps [RunWorkflow ...]`.

---

## 5) Define continuation contract for workflow commands

When a workflow command is emitted from a tool call, define exactly:

- Parent tool round is interrupted (`ToolsInterrupted`) and child workflow frame starts.
- On completion, child produces `PopResult` summary.
- Parent resumes at `ResumeAfterSubagent` (or a dedicated `ResumeAfterWorkflow`) and receives a synthetic `ToolResult` containing summary.

If you want structured data returned to parent, standardize:

```haskell
PopResult
  { prSummary :: Text
  , prStructured :: Maybe Value
  , ...
  }
```

---

## 6) Validation and error policy

Add strict validation at run start or before each `ExecTools`:

- Unknown `agUTools` name -> deterministic failure (`GErrAllModelsFailed` is too generic; add a dedicated error).
- Duplicate names -> normalize once (map by name).
- Registry tool not marked readonly while `rtReadonly = True` -> filtered out or explicit error.

Recommended new error:

```haskell
GErrUnknownUTool Text
```

---

## 7) Keep `ToolUtils` unchanged initially

You can keep existing legacy tool system (`Tool`, `toTool`, `ToolUtils`) intact.

`Generate4` can become the bridge:

- legacy `Tool` -> `utTool`
- registry `UTool` -> direct `ToolOutcome`

This minimizes churn across the codebase.

---

## 8) App wiring

In `app/Main.hs`:

1. Construct `UToolRegistry`.
2. Add desired names to `agent.agUTools`.
3. Call `generateTextWithUTools registry ...`.

Sketch:

```haskell
let registry = Map.fromList
      [ ("run_workflow", runWorkflowUTool models someAgents)
      , ("delegate_research", delegateUTool models)
      ]

let agent = createAgent fsConfig
      { agUTools = ["run_workflow", "delegate_research"] }

r <- generateTextWithUTools registry agent models runtime conversation
```

---

## 9) Minimal tests to add

1. `agUTools` name resolves and appears callable in tool round.
2. Unknown UTool name fails with clear error.
3. UTool returns `RunWorkflowCommand wf`, child workflow runs, parent resumes.
4. Name collision: UTool overrides legacy tool of same name.
5. Readonly mode blocks non-readonly UTools.

---

## Open decisions

Fill these before implementation:

1. **Unknown `agUTools` name**
    - [ ] fail hard
    - [x] warn and ignore

2. **Name collision policy (legacy `Tool` vs registered `UTool`)**
    - [x] UTool wins
    - [ ] legacy wins
    - [ ] hard error

3. **Workflow command shape**
    - [x] add `RunWorkflowCommand Workflow`
    - [ ] reuse only `PushSteps [RunWorkflow ...]`

4. **Parent resume payload after workflow**
    - [x] summary text only
    - [ ] summary + structured JSON
    - [ ] full child transcript

5. **Registry lifecycle**
    - [x] concrete `Map Text UTool`
    - [ ] builder registry (`BuildCtx -> UTool`)

6. **Readonly behavior**
    - [x] filter non-readonly UTools
    - [ ] fail if requested by `agUTools`

7. **Where registry lives**
    - [ ] in `Machine`
    - [x] in `RuntimeArgs`
    - [ ] explicit parameter on `generateTextWithUTools`

8. **Error type**
    - [ ] reuse existing errors
    - [x] add `GErrUnknownUTool` / `GErrUToolExecution`

---

## Recommended order to implement

1. Add registry + `generateTextWithUTools`.
2. Resolve `agUTools` in `ExecTools`.
3. Add unknown-name validation.
4. Add `RunWorkflowCommand`.
5. Implement parent resume mapping for workflow result.
6. Add tests.
