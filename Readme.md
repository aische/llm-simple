# llm-simple

An experimental Haskell **LLM toolbox** for trying out providers, tool loops, and workspace agents locally. It is meant to get you from zero to a working agent quickly — not to be a production agent platform.

The API stays small on purpose: JSON model and provider catalogs, bundled filesystem tools, and a straight line from `loadModelOrThrow` to `generateText`. Under that surface you still get multi-provider gateways, fallbacks, streaming, structured output, and sandboxed workspace tools — enough to prototype and compare ideas without assembling the pieces yourself.

**Status:** early 0.4.x release. APIs may change.

## Features

- **Providers:** OpenAI, Claude, Gemini, Ollama, DeepSeek
- **Generate:** single-request text, streaming, and structured object generation with model fallbacks, timeouts, retries, and throttling
- **Agent:** multi-round tool loops (`generateText`, `streamText`, `generateObject`)
- **Tools:** filesystem tool suite with workspace path sandboxing
- **Load:** JSON model and provider catalogs, API-key gateway initialization
- **Observability:** hooks, logging, and generation lifecycle events

## Install

```bash
cabal build
```

Requires GHC 9.6–9.12 with `GHC2021` (see `tested-with` in `llm-simple.cabal`).

Tested on Linux and macOS. Windows is not supported and not tested.

## Quick start

1. Create a `.env` file with API keys for the providers you use:

```env
OPENAI_API_KEY=...
CLAUDE_API_KEY=...
GEMINI_API_KEY=...
DEEPSEEK_API_KEY=...
```

Ollama needs no API key. It is used through its OpenAI-compatible endpoint
(`/v1/chat/completions`); tested with Ollama 0.32.4. Streamed responses
request usage via `stream_options.include_usage`; an Ollama version that does
not support it streams without usage.

The library reads keys from the **process environment**.
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

| Field             | Description                                                              |
| ----------------- | ------------------------------------------------------------------------ |
| `modelConfigName` | Name used in code to look up this config                                 |
| `providerName`    | Provider key defined in `providers.json`                                   |
| `modelName`       | Provider-specific model identifier                                       |
| `pricing`         | `pricePerMillionInput` / `pricePerMillionOutput` for usage cost tracking; optional `pricePerMillionCacheRead` / `pricePerMillionCacheWrite` (fall back to input rate) |
| `maxTokens`       | Max tokens per request                                                   |
| `temperature`     | Sampling temperature (optional)                                          |
| `requestTimeout`  | Timeout in ms per attempt (optional); for streaming, time until the first event; timeouts are retried |
| `throttleDelay`   | Delay in ms before each request attempt, retries included; added to the retry backoff (optional) |
| `retryCount`      | Number of retries on retryable errors (429, 500, 502, 503, 504, 529, network errors including streams that end early, timeouts) |
| `jitterBackoff`   | Base backoff in ms between retries; full jitter, exponential, each delay capped at 30 s. `Retry-After` is not honoured, so raise this for providers that often rate-limit you |
| `thinking`        | Extended thinking effort level (optional, provider-dependent)            |
| `capabilities`    | Optional object: `thinking`, `vision`, `promptCaching` (default `false`) |

A provider is only available if its API key is set (except Ollama, which is always available).

Capability flags are declarations used before each fallback candidate is
called: image parts require `vision`; enabled thinking requires `thinking`.
Unsupported candidates fail with `UnsupportedCapability` and the next
fallback is tried. Cache hints may be ignored by providers that do not
support prompt caching (they do not change prompt semantics).

### Image input

User messages may mix text and images:

```haskell
import LLM (imageUrlPart, imageBase64Part, textPart, UserMessage)

let Right png = imageBase64Part "image/png" "<base64>"
    msg = UserMessage [imageUrlPart "https://example.com/photo.jpg", png, textPart "describe"]
```

`imageBase64Part` validates MIME type (`image/jpeg|png|gif|webp|heic|heif`)
and base64 payload locally. Use a model with `"capabilities": {"vision": true}`.

### Prompt cache breakpoints

Mark stable content with `cacheEphemeral` (or `withCacheHint CacheEphemeral`).
Claude serializes that as `cache_control: {"type":"ephemeral"}` on the content
block (provider default TTL). Other providers omit the marker and send the same
content. Hints persist in conversation JSON and through agent tool rounds.

```haskell
import LLM (cacheEphemeral, textPart, UserMessage)

let msg =
      UserMessage
        [ cacheEphemeral (textPart "large stable context…"),
          textPart "question about the context"
        ]
```

`UserTurn "text"` still matches only a single unannotated text part; use
`UserMessage` when parts carry cache hints.

### Provider catalog

Providers are defined in `providers.json` (bundled with the package as a
reference). Built-in providers are always available. When you load a model
catalog, the loader looks for `providers.json` in the same directory; if present,
its entries are merged on top of the built-ins — adding new providers or
overriding built-ins with the same `providerName`.

| Field         | Description                                                                 |
| ------------- | --------------------------------------------------------------------------- |
| `providerName`| Key referenced by `providerName` in model catalog entries                   |
| `protocol`    | `"openai"`, `"claude"`, `"gemini"`, `"ollama"`, or `"deepseek"`             |
| `baseUrl`     | Provider origin (scheme+host[+port[+optional path prefix]]). OpenAI/DeepSeek/Ollama append `/v1/chat/completions`; Claude appends `/v1/messages`; Gemini appends `/v1beta/models/{model}:generateContent` |
| `apiKeyEnv`   | Environment variable holding the API key (omit for keyless providers)       |
| `baseUrlEnv`  | Optional env var that overrides `baseUrl` at runtime                      |

To add an OpenAI-compatible provider (OpenRouter, Groq, Together, etc.), add an
entry with `"protocol": "openai"`, set `baseUrl` and `apiKeyEnv`, then reference
`providerName` from your model catalog.

Optional base URL overrides for built-in providers: `OPENAI_BASE_URL`,
`CLAUDE_BASE_URL`, `GEMINI_BASE_URL`, `DEEPSEEK_BASE_URL`, `OLLAMA_BASE_URL`.
Do not include the protocol path (`/v1/...` or `/v1beta/...`) in `baseUrl`; the
library appends it.
A provider that sends an API key must use an `https://` base URL, or `http://`
to a loopback host (`localhost`, `127.0.0.0/8`, `::1`); any other `http://`
base URL makes the provider unavailable, so the key is never sent unencrypted.
Keyless providers (Ollama, or entries without `apiKeyEnv`) may use any
`http://` host.
`loadGateways` and `loadGatewaysWithDotenv` always use built-in provider defaults.
Custom providers from a co-located `providers.json` apply when loading models via
`loadModelOrThrow` or `loadModelsOrThrow`.

### Loading models

```haskell
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (IOException, catch)
import LLM.Load (loadModelsOrThrow)

main = do
  loadFile defaultConfig `catch` \(_ :: IOException) -> pure ()
  (gemini, deepseek) <-
    loadModelsOrThrow "./model-catalog.json" ("gemini_lite", "deepseek4flash")
  ...
```

`loadModelsOrThrow` takes a filepath and a tuple of config names, returning a tuple of `ModelConfig` values. Use `loadModelOrThrow` for a single model. On failure it throws a catchable `LoadConfigError`.

## Usage

### Agent with tools

An agent runs a loop: the model may call tools, results are fed back, and the loop continues until the model finishes or `agMaxToolRounds` is reached.

```haskell
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (IOException, catch)
import Heptapod (generate)
import LLM.Agent (Agent (..), RuntimeArgs (..), generateText, noEventObserver)
import LLM.Core (pattern UserTurn)
import LLM.Generate (ModelWithFallbacks (..), Hooks (..), llmHooks, noHooks)
import LLM.Load (fsTools, loadModelsOrThrow)

main :: IO ()
main = do
  loadFile defaultConfig `catch` \(_ :: IOException) -> pure ()

  (gemini, deepseek) <-
    loadModelsOrThrow "./model-catalog.json" ("gemini_lite", "deepseek4flash")

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

Hooks are for observation only. An exception a hook throws is caught and dropped (asynchronous exceptions still propagate), so a hook cannot stop a run.

For local debugging, `defaultDebugHooks` enables stderr logging and writes request/response JSON to `./dumps` — avoid that in production or shared environments. Each dump is a new file readable only by its owner; dumps hold prompts, tool arguments and results, and model output, and are never deleted by the library.

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

| Tool name               | Purpose                                 |
| ----------------------- | --------------------------------------- |
| `copy_file`             | Copy a file                             |
| `create_directory`      | Create a directory                      |
| `directory_tree`        | Show directory tree                     |
| `file_info`             | File metadata                           |
| `find_files`            | Find files by pattern                   |
| `grep`                  | Search file contents                    |
| `move_file`             | Move/rename a file                      |
| `multi_replace_in_file` | Multiple search-and-replace in one file |
| `readdir`               | List directory contents                 |
| `read_file_paginated`   | Read a file in pages                    |
| `remove_directory`      | Remove a directory                      |
| `remove_file`           | Remove a file                           |
| `replace_in_file`       | Search-and-replace in a file            |
| `writefile`             | Write or overwrite a file               |

Use `fsTools'` to map tool results through a custom embedding function.

### Filesystem sandbox

Filesystem tools enforce **workspace-relative path containment**: `..` escapes, absolute paths outside the workspace, and symlinks pointing outside are rejected. A symlink is judged by where it points, including when its target does not exist yet: writing through a dangling link that leads out of the workspace is refused and does not create the target. A link that resolves inside the workspace is followed, and the operation lands on its target. Recursive tools skip symlinks rather than following them. A path that cannot be checked for being a symlink (for example, permission denied) is treated as one.

Errors that reach the model name paths relative to the workspace, so the model does not learn where the workspace lives on the host. For any tool, the library drops an absolute file name from an `IOException` before it shows the error to the model.

**Threat model.** The sandbox keeps the *model* inside the workspace. Through the tools, a model can only reach paths inside it: `..`, absolute paths and symlinks are checked as described above. The tools cannot create symlinks, and an agent runs its tool calls one at a time, so a model cannot change the workspace while a check it relies on is in progress.

The sandbox does **not** protect against *other processes* that write to the workspace while the agent runs. A path is checked and then opened. A process that swaps a directory for a symlink between those two steps (a time-of-check/time-of-use race) can make a tool read or write outside the workspace. Hard links to files outside the workspace are not detected either. Such a process usually runs as the same user and can reach those files directly, so this only matters if it has less access than the agent. If a less-trusted process can write to the workspace, isolate the agent at the OS level (a container, or a separate user).

**Limitations (0.1.x):**

- The sandbox is **path-scoped**, not kernel-level isolation. Use a dedicated, disposable workspace directory — not your home directory or a production tree.
- There is no denylist for sensitive in-workspace files (`.env`, `.git/config`, etc.). Anything inside the workspace is accessible to tools the model can call.
- Read tools enforce byte caps: whole-file and edit tools use a 1 MiB limit; `read_file_paginated` accepts source files up to 10 MiB and returns at most 100 KiB per page.
- Search and listing tools bound their work: `find_files`, `grep` and `directory_tree` look at no more than 20,000 entries per call and say so when they stop; tool output is capped at 100 KiB.
- `rtReadonly` omits mutating tools from the model schema and blocks them at execution time if they are invoked anyway.
- `writefile` and the replace tools replace the file as a whole (a temporary file renamed over it), so a failed write leaves the old content. The file keeps its mode bits but gets a new inode, so hard links to it keep the old content. Concurrent edits of one file from the same process are serialized; edits from other processes are not.

## Module layout

| Module          | Purpose                                                                                                        |
| --------------- | -------------------------------------------------------------------------------------------------------------- |
| `LLM`           | Convenience re-exports for common types, generation, loaders, and tool wiring                                  |
| `LLM.Core`      | Types, gateways, provider interface, usage tracking                                                            |
| `LLM.Providers` | Provider implementations (OpenAI, Claude, Gemini, Ollama, DeepSeek)                                            |
| `LLM.Generate`  | Single-request generation with fallbacks (`generateTextWithFallbacks`, `streamTextWithFallbacks`, `genObject`) |
| `LLM.Agent`     | Agent loops with tool execution (`generateText`, `streamText`)                                                 |
| `LLM.Tools`     | Built-in filesystem tool definitions                                                                           |
| `LLM.Load`      | Model and provider catalog loading, gateway init, `fsTools`                                                    |

Import `LLM` for the common surface, or import sub-modules directly for agent loops, providers, and advanced configuration. Sub-module APIs are exposed but considered experimental in 0.1.x.

**Note:** Workflow support was removed from this repo and moved elsewhere.

## Testing

```bash
cabal test
```

Tests replay recorded provider responses from `test/fixtures/` so they run without live API calls.

## License

BSD-3-Clause. See [LICENSE](LICENSE).
