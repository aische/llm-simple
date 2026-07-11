# llm-simple

Experimental Haskell library for talking to LLMs: multi-provider gateways, model fallbacks, agent tool loops, structured output, and built-in filesystem tools.

**Status:** early 0.1.x release. APIs may change.

## Features

- **Providers:** OpenAI, Claude, Gemini, Ollama, DeepSeek
- **Generate:** single-request text, streaming, and structured object generation with model fallbacks, timeouts, retries, and throttling
- **Agent:** multi-round tool loops (`generateText`, `streamText`, `generateObject`)
- **Tools:** filesystem tool suite with workspace path sandboxing
- **Load:** JSON model catalog and API-key gateway initialization
- **Observability:** hooks, logging, and generation lifecycle events

## Install

```bash
cabal build
```

Requires GHC 9.6+ with `GHC2021` (see `llm-simple.cabal`).

## Quick start

1. Create a `.env` file with API keys for the providers you use:

```env
OPENAI_API_KEY=...
CLAUDE_API_KEY=...
GEMINI_API_KEY=...
DEEPSEEK_API_KEY=...
```

Ollama needs no API key. The library reads keys from the **process environment**.
Load `.env` yourself in `main` (the example executable does this), or call
`loadGatewaysWithDotenv` when building gateways manually.

2. Configure models in `model-catalog.json` (an example is included in the repository and source distribution).

3. Create a workspace directory for filesystem tools (the example uses `./user-workspace/`).

4. Run the example executable:

```bash
cabal run llm-simple
```

The example loads `.env`, loads models from the catalog, wires up filesystem tools, and runs a small agent that asks about files in the workspace.

## Configuration

### Model catalog

Models are defined in a JSON array. Each entry maps a logical config name to a provider and model, plus runtime settings:

| Field | Description |
|-------|-------------|
| `modelConfigName` | Name used in code to look up this config |
| `providerName` | `"openai"`, `"claude"`, `"gemini"`, `"ollama"`, or `"deepseek"` |
| `modelName` | Provider-specific model identifier |
| `pricing` | `pricePerMillionInput` / `pricePerMillionOutput` for usage cost tracking |
| `maxTokens` | Max tokens per request |
| `temperature` | Sampling temperature (optional) |
| `requestTimeout` | Request timeout in ms (optional) |
| `throttleDelay` | Delay between requests in ms (optional) |
| `retryCount` | Number of retries on failure |
| `jitterBackoff` | Backoff jitter in ms between retries |
| `thinking` | Extended thinking effort level (optional, provider-dependent) |

A provider is only available if its API key is set (except Ollama, which is always available).

### Loading models

```haskell
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import LLM.Load (loadModelsOrThrow)

main = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()
  (gemini, deepseek) <-
    loadModelsOrThrow "./model-catalog.json" ("gemini_2_5_flash", "deepseek4flash")
  ...
```

`loadModelsOrThrow` takes a filepath and a tuple of config names, returning a tuple of `ModelConfig` values. Use `loadModelOrThrow` for a single model. On failure it throws a catchable `LoadConfigError`.

## Usage

### Agent with tools

An agent runs a loop: the model may call tools, results are fed back, and the loop continues until the model finishes or `agMaxToolRounds` is reached.

```haskell
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import Heptapod (generate)
import LLM.Agent (Agent (..), RuntimeArgs (..), generateText, noEventObserver)
import LLM.Core (Turn (UserTurn))
import LLM.Generate (ModelWithFallbacks (..), Hooks (..), llmHooks, noHooks)
import LLM.Load (fsTools, loadModelsOrThrow)

main :: IO ()
main = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()

  (gemini, deepseek) <-
    loadModelsOrThrow "./model-catalog.json" ("gemini_2_5_flash", "deepseek4flash")

  let models = ModelWithFallbacks { mwfModel = gemini, mwfFallbacks = [deepseek] }

  toolMap <- fsTools "./user-workspace/"
  genId <- generate

  let agent =
        Agent
          { agName = "friendly-assistant",
            agSystemPrompt = Nothing,
            agTools = ["directory_tree"],
            agMaxToolRounds = 10,
            agContextWindow = Nothing
          }
      rt =
        RuntimeArgs
          { rtGenerationId = genId,
            rtAbortSignal = Nothing,
            rtLLMHooks = llmHooks noHooks,
            rtHooks = noHooks,
            rtOnEvent = noEventObserver,
            rtReadonly = False
          }

  r <- generateText agent models toolMap rt [UserTurn "what files are here?"]
  print r
```

Set `rtReadonly = True` to omit mutating tools from the schema sent to the model and to block them at execution time.

For local debugging, `defaultDebugHooks` enables stderr logging and writes request/response JSON to `./dumps` — avoid that in production or shared environments.

### Single request (no agent loop)

For one-shot generation without a tool loop, use the Generate layer directly:

```haskell
import LLM.Generate (GenRequest (..), generateTextWithFallbacks)

result <- generateTextWithFallbacks genRequest models
```

Also available: `streamTextWithFallbacks`, `genObject`, and `genObjectUntyped`.

### Custom tools

Define a `Tool` with a `ToolDef` (sent to the model) and an execution function, then add it to a `ToolMap`. Use `toTool` to convert typed tool definitions. See `LLM.Agent.ToolUtils` and the modules under `LLM.Tools`.

## Built-in filesystem tools

`fsTools` registers these tools, all scoped to a workspace root:

| Tool name | Purpose |
|-----------|---------|
| `copy_file` | Copy a file |
| `create_directory` | Create a directory |
| `directory_tree` | Show directory tree |
| `file_info` | File metadata |
| `find_files` | Find files by pattern |
| `grep` | Search file contents |
| `move_file` | Move/rename a file |
| `multi_replace_in_file` | Multiple search-and-replace in one file |
| `readdir` | List directory contents |
| `read_file_paginated` | Read a file in pages |
| `remove_directory` | Remove a directory |
| `remove_file` | Remove a file |
| `replace_in_file` | Search-and-replace in a file |
| `writefile` | Write or overwrite a file |

Use `fsTools'` to map tool results through a custom embedding function.

### Filesystem sandbox

Filesystem tools enforce **workspace-relative path containment**: `..` escapes, absolute paths outside the workspace, and symlinks pointing outside are rejected. Recursive tools skip symlinks rather than following them.

**Limitations (0.1.x):**

- The sandbox is **path-scoped**, not kernel-level isolation. Use a dedicated, disposable workspace directory — not your home directory or a production tree.
- A **time-of-check/time-of-use (TOCTOU)** window exists between path validation and file open; a concurrent process inside the workspace could theoretically plant a symlink between those steps.
- There is no denylist for sensitive in-workspace files (`.env`, `.git/config`, etc.). Anything inside the workspace is accessible to tools the model can call.
- Read tools enforce byte caps: whole-file and edit tools use a 1 MiB limit; `read_file_paginated` accepts source files up to 10 MiB.
- `rtReadonly` omits mutating tools from the model schema and blocks them at execution time if they are invoked anyway.

## Module layout

| Module | Purpose |
|--------|---------|
| `LLM` | Convenience re-exports for common types, generation, loaders, and tool wiring |
| `LLM.Core` | Types, gateways, provider interface, usage tracking |
| `LLM.Providers` | Provider implementations (OpenAI, Claude, Gemini, Ollama, DeepSeek) |
| `LLM.Generate` | Single-request generation with fallbacks (`generateTextWithFallbacks`, `streamTextWithFallbacks`, `genObject`) |
| `LLM.Agent` | Agent loops with tool execution (`generateText`, `streamText`) |
| `LLM.Tools` | Built-in filesystem tool definitions |
| `LLM.Load` | Model catalog loading, gateway init, `fsTools` |

Import `LLM` for the common surface, or import sub-modules directly for agent loops, providers, and advanced configuration. Sub-module APIs are exposed but considered experimental in 0.1.x.

**Note:** Workflow support was removed from this repo and moved elsewhere.

## Testing

```bash
cabal test
```

Tests replay recorded provider responses from `test/fixtures/` so they run without live API calls.

## License

BSD-3-Clause. See [LICENSE](LICENSE).
