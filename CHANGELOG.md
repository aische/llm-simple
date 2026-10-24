# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Fixed

- Provider requests are no longer resent by req. req's default config
  retried timeouts and 408/504 responses up to 5 times, resending (and
  billing) the POST, before `withRetry` retried again. Its default 30 s
  response timeout also failed any non-streaming call that took longer than
  30 s to generate. All providers now share one TLS manager without a
  response timeout and a config that never retries; `mcRequestTimeout` is the
  only response deadline. New `mkLenientConfig` (`LLM.Core.ProviderUtils`)
  builds that config for a given manager.
- One missing API key no longer breaks loading of every model.
  `loadModelOrThrow` / `loadModelsOrThrow` used to build every catalog entry
  and fail on the first one whose provider had no key, with a misleading
  "Provider not found". Now only the requested models must be available; for
  one that is not, the error names the reason, e.g. "Model haiku_4_5 needs
  provider claude, but env var CLAUDE_API_KEY is not set" (or the invalid
  base URL). The returned `ModelConfigMap` / `GatewayMap` hold all currently
  available models and gateways. "Model not found" errors now name the model.
  New `resolveGateway`, `resolveGateways` and `ProviderUnavailable`
  (`LLM.Load.LoadGateways`).
- File tools no longer corrupt non-UTF-8 files or depend on the locale.
  `replace_in_file` and `multi_replace_in_file` decoded leniently and wrote
  the whole file back, replacing every invalid byte with U+FFFD; they now
  refuse files that are not valid UTF-8 and leave them unchanged. Writes
  (`writefile` and both replace tools) always produce UTF-8; under `LANG=C`
  they used to fail or mangle non-ASCII text. `grep` and `file_info` read
  bytes and decode leniently, so `grep` no longer skips files it cannot
  decode. `getSystemPrompt` reports a `file:` prompt that is not valid UTF-8
  as a `LoadSystemPromptError` naming the file.
- An abort during a tool round no longer leaves an unanswered tool call in
  `gerPartialNewMessages`. The partial history used to end with the
  assistant's tool calls and no `ToolTurn`, which Claude and OpenAI reject on
  the next request. It now ends with a `ToolTurn` holding the real results of
  the calls that ran and "Aborted before execution" for the rest.
  **Breaking:** `executeToolsWithAbort` returns the new `ToolRoundOutcome`
  (`ToolRoundCompleted` / `ToolRoundAborted`) instead of
  `GenerateResult [ToolResult]`.
- Gemini structured output works with thinking enabled. The response's first
  part was the thought summary, which the object parser tried to decode as
  JSON (`EmptyResponse`). The parser now joins the text of all parts not
  marked `"thought": true`, and object requests no longer ask for thought
  summaries (`includeThoughts`).
- Ollama honours the max-token limit. Chat requests sent `num_predict` and
  object requests `max_completion_tokens`; Ollama's OpenAI-compatible endpoint
  ignores both, so `mcMaxTokens` had no effect. Both now send `max_tokens`.
  Streamed Ollama responses now request usage (`stream_options.include_usage`)
  as OpenAI's do. The recorded Ollama conversation fixtures were re-recorded.
- File tools handle symbolic links as documented. `sandboxPath` resolves
  links, so the tools' `isSymlink` checks on its result never fired:
  `remove_file` and `remove_directory` on a link deleted the link's target
  inside the workspace, and `move_file` / `copy_file` moved or copied the
  target. `remove_file` and `remove_directory` now remove the link itself
  and leave its target unchanged (also for links pointing outside the
  workspace); `move_file` and `copy_file` refuse a link source. New
  `sandboxSymlink` (`LLM.Tools.FsConfig`).
- Claude and Gemini no longer send turns with empty content. After a
  fallback, an assistant turn holding only foreign thinking (e.g. a DeepSeek
  turn cut off during reasoning, or a Claude turn with only redacted
  thinking) encoded to `"content": []` / `"parts": []`, which the API
  rejects with a 400. Such turns, and user turns whose parts all encode to
  nothing, are now left out; both APIs accept the consecutive same-role
  turns this can leave. The OpenAI protocol was fixed in 0.3.0.0.
- Gemini thought signatures are only replayed to the model that produced
  them. Model names were compared by prefix, so `gemini-2.5-flash` and
  `gemini-2.5-flash-lite` counted as the same model and received each
  other's signatures. Names now must be equal apart from a `models/` prefix
  and a stable version suffix (`-001`); other calls get the documented
  placeholder signature.
- Claude non-streaming responses with unknown content blocks (e.g.
  `server_tool_use`, `web_search_tool_result`) no longer fail as
  `EmptyResponse`. Unknown blocks are skipped, as streaming already did.
- Non-streaming HTTP errors keep their status and raw body. `HttpError`
  bodies (and the `onLLMResponseError` text) were Haskell `show` output
  (`Object (fromList [...])`); they are now the body as sent, as for
  streaming. A non-JSON error response (e.g. an HTML 401/502 from a proxy)
  made req throw, which became a retryable `NetworkError` without its
  status; it is now `HttpError status body`. A 200 body that is not JSON is
  a `ParseError`. Streaming error bodies that are not valid UTF-8 are
  decoded leniently instead of throwing. **Breaking:** `sendRequest` and
  `sendObjectRequest` in `LLMProvider` return the raw body
  (`IO (Int, ByteString)`) instead of a decoded `Value`.
- HTTP 500, 502 and 504 are retried, like 429/503/529. They are usually
  transient, and a Claude mid-stream `api_error` (mapped to 500) was never
  retried. Each retry delay is now capped at 30 s; `fullJitterBackoff` grew
  without bound for large `retryCount`s. `Retry-After` is still ignored.
- Reasoning is no longer cut to its first chunk. `respReasoning` (and
  `projectReasoning`) returned only the first thinking part, so a streamed
  Gemini response kept only its first thought chunk, and the OpenAI/DeepSeek
  encoder replayed a truncated `reasoning_content`. Both now concatenate all
  thinking text. `mkChatResponse` merges adjacent thinking parts that carry
  no opaque payload; signed parts (e.g. Claude thinking blocks) stay one per
  block.
- The generate layer honours `grAbortSignal`, as its docs promised.
  `generateTextWithFallbacks` / `streamTextWithFallbacks` (and the
  `Rotated` variants) never read it, so the `Aborted` path was dead. The
  signal is now checked before each model in the fallback chain and before
  each attempt (also after throttle and backoff sleeps), and a stream is cut
  off at its next event; all yield `GErrAborted`. A non-streaming HTTP call
  already in flight is not interrupted. **Breaking:** removed the unused
  `LLMError` constructor `ToolLoopExceeded` (the agent loop reports
  `GErrToolExceeded`).
- Documented what happens at `agMaxToolRounds`: the last round's tools run
  and their results are in `gerPartialNewMessages`, but the model is not
  called again, so it never sees them.
- Smaller file-tool gaps. `grep` on an explicit file path skipped the 1 MB
  size cap that applies to files found by its directory walk; such a file is
  now refused with an error. `read_file_paginated` threw on bytes that are
  not valid UTF-8; it now shows them as U+FFFD, like the other read-only
  tools. `find_files` backtracked exponentially on patterns with many `*`s
  (e.g. `*a*a*a*a*a*a*a*a*b` took seconds on a 40-character name); matching
  is now O(pattern × name), with the same results. Its walk no longer
  recounts and reverses the match list at every entry.
- Gemini tool-call ids no longer repeat across processes. They were
  `call_<n>` from `Data.Unique`, which restarts at 1 in every process, so a
  saved conversation reloaded in a new process got new calls with ids it
  already held; after a fallback, Claude and OpenAI reject or mismatch
  those. Ids are now `call_<random UUID v4>`. New direct dependency:
  `random`.

## [0.3.0.0] - 2026-10-13

### Added

- Cross-provider history regression test (`test/LLM/CrossProviderSpec.hs`).
  One mixed history (Claude → Gemini → OpenAI → Claude turns, a base64
  image, a redacted-thinking-only turn) is encoded by every provider and
  checked against rules the live APIs enforce: Gemini thought signatures on
  every function call, DeepSeek `reasoning_content` in thinking mode, no empty
  assistant messages on the OpenAI protocol, only Claude thinking blocks and
  paired tool ids for Claude, and no provider's opaque payload in another
  provider's request.

### Changed

- **Breaking:** `LLMProvider.sendStreamRequest` is split into `openStream`
  (HTTP transport; hands the raw body reader to a continuation) and
  `parseStream` (turns raw body bytes into a response). Custom providers must
  implement both. Streaming behaviour is unchanged; the split lets recorded
  conversation fixtures capture the raw SSE body and replay it through the
  real stream parsers.
- Agent runs (`generateText` / `streamText`) stay on a fallback model once it
  has answered. Each tool round used to start again at `mwfModel`, so while
  the primary was down every round paid its full retry/backoff/timeout cost
  and the history alternated between providers. Now each round uses the chain
  rotated so the model that answered comes first; models that failed move to
  the end and remain a last resort. This lasts for one `generateText` /
  `streamText` call; the next call starts from the caller's chain again. A
  switch is logged once at `Info` ("Continuing run on …"). New
  `withModelFallbacksRotated` (`LLM.Generate.GenerateUtils`),
  `generateTextWithFallbacksRotated` and `streamTextWithFallbacksRotated`
  (`LLM.Generate.Generate`).

### Fixed

- `remove_directory` no longer deletes the whole workspace when given a path
  that resolves to the workspace root (`"."`, `""`, `"./"`, `"sub/.."`); it
  returns an error message instead. `remove_file` and `move_file` (source)
  refuse the root too, and `remove_file` on a directory returns a clear error
  instead of throwing. New helper `isSandboxRoot` in `LLM.Tools.FsConfig`.
- `mcRequestTimeout` now applies to each attempt instead of covering all
  retries and backoff sleeps, and `TimeoutError` is now retryable
  (`isRetryable`). For streaming, the timeout only covers the time until the
  first stream event, so long answers are no longer cut off. New
  `withFirstEventTimeout` (`LLM.Core.Utils`) and `callWithRetry`
  (`LLM.Generate.GenerateUtils`).
- **Breaking:** `StreamChunk` gains `StreamAttemptReset`. Streaming consumers
  used to see duplicated output when an attempt failed after streaming some
  chunks and a retry or fallback model started over. The reset is emitted
  before the next attempt (only if the failed attempt streamed something);
  consumers should discard the chunks of the current round when they get it.
  Exhaustive pattern matches on `StreamChunk` need a new case.
- Claude thinking: `budget_tokens` is capped at `max_tokens - 1`; Anthropic
  rejects a budget that is not below `max_tokens`, so medium effort or above
  with `maxTokens: 4096` failed with HTTP 400 on every call. If `max_tokens`
  leaves no room for the 1024-token minimum budget, the Claude gateway returns
  `UnsupportedCapability` without sending the request (new
  `checkClaudeThinkingBudget`). Example catalog: `haiku_4_5` uses
  `maxTokens: 16000` so thinking leaves room for the answer.
- Mid-stream errors are no longer ignored. A Claude `event: error` or an
  OpenAI-compatible `data: {"error": ...}` chunk (OpenAI, DeepSeek, Ollama)
  now yields `HttpError status body` instead of partial content or
  `EmptyResponse`. Claude error types map to Anthropic's HTTP statuses
  (`overloaded_error` → 529, `rate_limit_error` → 429, `api_error` → 500, …),
  so overload and rate limits are retried. OpenAI-style errors use their numeric
  code if present, otherwise 429 / 503 / 500.
- Tool calls whose arguments are not valid JSON (e.g. cut off by `max_tokens` /
  `finish_reason: "length"`) now yield `ParseError` instead of a tool call with
  `String` arguments, in both streaming and non-streaming parsers. Empty
  argument strings decode as `{}`. A non-streaming Claude response that stops
  at `max_tokens` inside a `tool_use` block is also a `ParseError`.
- Tool execution, `safeHooks` and `emitEvent` no longer catch asynchronous
  exceptions. A `timeout`, `killThread` or `cancel` during a tool call used to
  become a "Tool error" result and the agent loop kept going. New `trySync`
  in `LLM.Core.Utils`.
- `stripBoundsAndComments` is schema-aware: it removes `minimum` / `maximum` /
  `$comment` keywords but keeps properties with those names and leaves
  literal values (`enum`, `const`, `default`, `examples`) alone. Gemini applies
  it only to tool `parameters` and `responseSchema` at body-build time, not to
  the whole request (which could strip keys from tool-call arguments).
- OpenAI-compatible stream parser (OpenAI, DeepSeek, Ollama) keeps every tool
  call in a chunk, not just the first. Ollama often sends all calls complete in
  one chunk, sometimes without `index`; the array position is used then. Tool
  calls are emitted in index order.
- Gemini: a `functionCall` part without a matching thought signature (a call
  from another provider, or from a different Gemini model) now carries
  Google's placeholder `"skip_thought_signature_validator"` (new
  `skipThoughtSignatureValidator`). Gemini 3 rejected such history with
  `400 "Function call is missing a thought_signature"`, so a fallback into
  Gemini 3, or between two Gemini models, failed partway through a tool loop.
- DeepSeek with thinking enabled: every assistant message now carries
  `reasoning_content`, `""` when the turn has no thinking text (turns from
  OpenAI, Ollama, or Claude without thinking). DeepSeek rejected such history
  with `400 "The reasoning_content in the thinking mode must be passed back"`.
  Requests with thinking off are unchanged. **Breaking:**
  `MessageEncodeOptions` is now a two-field record with the new
  `meoRequireReasoning`; code that constructs it directly must set it. The
  streamed DeepSeek fixture's requests gained the empty field.
- OpenAI-protocol providers (OpenAI, OpenRouter, Ollama, DeepSeek) no longer
  send an assistant message with neither `content` nor `tool_calls`, e.g. a
  Claude turn that held only (redacted) thinking. OpenAI rejected
  `{"role":"assistant"}` with "expected a string, got null"; such turns now
  send `"content": ""`. Tool-call-only turns still omit `content`.
- Image media types are checked per provider. `mkImageBase64` accepts HEIC
  and HEIF, but only Gemini does; Claude, OpenAI, Ollama and DeepSeek return
  400 for anything but jpeg, png, gif and webp. A HEIC image in a Gemini
  conversation broke the fallback to those providers, because the vision
  check passed. `validateModelCapabilities` now skips a model whose gateway
  does not accept every inline image's media type (`UnsupportedCapability`,
  like a model without vision). URL images have no known type and are not
  checked. New `commonImageMediaTypes` (`LLM.Core.Types`). **Breaking:**
  `LLMGateway` gained `gwImageMediaTypes` and `LLMProvider` gained
  `providerImageMediaTypes` (copied by `toGateway`); code that constructs
  either directly must set it.
- Ollama models are skipped for conversations with URL images. Ollama's
  OpenAI-compatible endpoint answers an `image_url` with an https URL with
  `400 "image URLs are not currently supported, please use base64 encoded
  data instead"` (checked against Ollama with `moondream`; base64 data URLs
  work). **Breaking:** `LLMGateway` gained `gwImageUrlSupport` and
  `LLMProvider` gained `providerImageUrlSupport` (`False` only for Ollama);
  code that constructs either directly must set it.

## [0.2.0.0] - 2026-10-06

### Changed

- **Breaking:** conversation turns use ordered content parts.
  `UserMessage` / `AssistantMessage` hold `[ContentPart]`; `UserTurn` is a
  bidirectional pattern synonym for a single text part. Replace
  `AssistantTurn text reasoning calls` with `assistantTurn` (canonical order)
  or `AssistantMessage respContent` for authoritative replay.
- **Breaking:** `ChatResponse.respContent` is now `[ContentPart]` (authoritative).
  `respText` / `respReasoning` remain convenience projections.
- **Breaking:** `ToolCall.tcProviderMeta` is `Maybe ProviderOpaque` (tagged
  provider + optional model + opaque payload), not bare `Value`.
- **Breaking:** removed `ContentBlock`; use `ContentPart` / `PartBody`.
- **Breaking:** `Turn` / `ToolCall` JSON shapes changed (role/content based).
  Persisted histories must be migrated; old and new shapes are not mixed.
- Claude adapter: thinking request mapping (`thinking.type=enabled` +
  `budget_tokens` from catalog effort), ordered thinking/text/tool_use parse
  and stream, signed-block replay around tool rounds. Temperature is omitted
  when thinking is enabled (Anthropic incompatibility).
- Gemini adapter: catalog thinking maps to `thinkingConfig`; tool-call thought
  signatures use `ProviderOpaque` and still replay only for a matching model.
- OpenAI / DeepSeek / Ollama adapters encode and parse ordered parts; DeepSeek
  reasoning continues via `reasoning_content` on ordered `ThinkingPart`s.
- Foreign opaque thinking / tool metadata is stripped at encode time on
  provider fallback (history unchanged).
- **Breaking:** `ModelConfig` gains `mcCapabilities` (`ModelCapabilities`).
  Manual record construction must set it (use `defaultModelCapabilities`).
- Example catalog: `capabilities` on vision/thinking models; removed
  `temperature` from `haiku_4_5` (Claude thinking temperature footgun).

- **Breaking:** `Usage` adds `usageCacheReadTokens` / `usageCacheCreationTokens`.
  `usageInputTokens` is total input including cache read/write tokens (providers
  normalize wire semantics so totals are not double-counted). Prefer `mkUsage`.
- **Breaking:** `PricingInfo` adds optional `pricePerMillionCacheRead` /
  `pricePerMillionCacheWrite`; absent rates fall back to the ordinary input rate.
  Prefer `defaultPricingInfo` for catalogs without cache rates.
- Cost estimation: ordinary input × input rate + cache read × cache-read rate +
  cache creation × cache-write rate + output × output rate.
- **Breaking:** `ContentPart` gains `partCacheHint :: Maybe CacheHint`. Manual
  construction and pattern matches must account for the new field; prefer
  `textPart` / `cacheEphemeral`. `UserTurn` still matches only a single
  unannotated text part.

### Added

- Image input: `ImageSource`, `ImagePart`, `imageUrlPart`, `imageBase64Part` /
  `mkImageBase64` with MIME and base64 validation. Encoded for Claude, Gemini,
  and OpenAI Chat Completions (DeepSeek/Ollama reuse the OpenAI shape when
  `capabilities.vision` is declared).
- Catalog `capabilities` object (`thinking`, `vision`, `promptCaching`; missing
  flags default to `false`). Carried on `ModelCatalogItem` / `ModelConfig`.
- Fallback candidates are validated before I/O: images require `vision`;
  enabled thinking requires `thinking`. Unsupported capability yields
  `UnsupportedCapability` and continues the fallback chain.
- `ProviderOpaque`, `ThinkingContent`, `ContentPart`, `PartBody`, `textPart`,
  `thinkingPart`, `toolCallPart`, `mkChatResponse`, `projectText`,
  `projectReasoning`, `turnToolCalls`, `validateTurn`, `stripForeignOpaque`.
- Claude thinking fixture and unit tests for ordered replay / foreign opaque
  omission; Gemini signature matching tests.
- Image request-shape tests (Claude/Gemini/OpenAI) and vision fallback tests.
- `mkUsage`, `defaultPricingInfo`, `usageOrdinaryInputTokens`; provider parsers
  report cache read/creation counters (Claude, OpenAI, DeepSeek, Gemini).
- Optional Claude prompt-cache breakpoints: `CacheHint` / `CacheEphemeral` on
  `ContentPart.partCacheHint`, helpers `withCacheHint` / `cacheEphemeral`.
  Claude emits wire `cache_control: {type: ephemeral}` (default TTL) on marked
  parts without reordering; other providers ignore hints and leave content
  unchanged. Hints round-trip in conversation JSON and survive agent tool rounds.

## [0.1.1.0] - 2026-08-23

### Changed

- Widen dependency bounds for GHC 9.8–9.12 (`base`, `bytestring`, `containers`).
  `tested-with`: GHC 9.6.7, 9.8.4, 9.10.2, 9.12.2. GitHub Actions CI runs
  `cabal test` and `cabal haddock` on that matrix.
- Example `model-catalog.json`: default Gemini entry is `gemini_lite` (`gemini-3.1-flash-lite`);
  removed deprecated `gemini-2.5-flash` config; raised example `maxTokens` to 4096; added
  optional `gpt_5_6_terra` (`gpt-5.6-terra`); corrected `gemini_lite` and `deepseek4flash`
  pricing (DeepSeek rates are peak cache-miss; off-peak is half).
- `get_history` tool description now documents the `"(no earlier history)"` /
  `"(no more history)"` sentinels instead of claiming an empty result.

### Fixed

- Claude and Gemini gateways honor catalog `baseUrl` / `baseUrlEnv` (previously
  ignored; always hit the public Anthropic/Google hosts). New
  `claudeGatewayWith` / `geminiGatewayWith` constructors; optional
  `CLAUDE_BASE_URL` and `GEMINI_BASE_URL` env overrides.
- `get_history` no longer hangs when the visible context window has zero user
  turns (page size 0); `chunkBackward` treats `n <= 0` as a single unpaged chunk.
- `generateObject` / `generateObjectUntyped` never advertise tools (including
  auto-injected `get_history`), even when `agContextWindow` is set. New
  `createGenRequestNoTools` helper; windowing still truncates messages.

## [0.1.0.2] - 2026-07-15

### Added

- `providers.json` provider catalog: configure provider endpoints, API key env
  vars, and protocols without hardcoding providers in Haskell.
- `loadProviderCatalog`, `ProviderCatalogItem`, and `loadGatewaysFromCatalog`
  for explicit provider catalog loading.
- Optional `baseUrlEnv` overrides for OpenAI, DeepSeek, and Ollama.

### Changed

- Partial `providers.json` files are merged with built-in provider defaults;
  file entries add or override providers by `providerName`.

## [0.1.0.1] - 2026-07-11

### Changed

- `loadModelsOrThrow` and `loadModelOrThrow` now throw catchable `LoadConfigError`
  exceptions via `throwIO` instead of calling `error`.
- Gateway loading no longer reads a `.env` file implicitly: `loadGateways` reads the
  process environment only; use `loadGatewaysWithDotenv` for local-dev convenience.
- Sandbox violation errors shown to the model omit absolute workspace paths.
- Streaming tool-call phase detection uses conversation context instead of stream
  ordering heuristics.
- Top-level `LLM` module export surface refined.
- `replace_in_file` and `multi_replace_in_file` refuse files larger than 1 MiB.
- `read_file_paginated` refuses source files larger than 10 MiB before line skipping.
- `rtReadonly` now blocks mutating tools at execution time, not only in the tool schema.

### Added

- `loadGatewaysWithDotenv` for explicit `.env` loading.
- `formatSandboxViolation` for workspace-relative sandbox error messages.
- `readBoundedTextFile` and `maxPaginatedFileBytes` filesystem resource limits.
- DeepSeek recorded conversation fixtures and `record-conversation` executable.
- Filesystem sandbox tests (`FsConfigSpec`, `FsLimitsSpec`, `FsToolsSpec`, `ToolUtilsSpec`).
- Load, streaming, history, and structured-output tests (`LoadSpec`, `StreamingSpec`, `HistoryToolSpec`, `GenerateObjectSpec`).

### Removed

- Weather tool moved from the library to test helpers.

## [0.1.0.0] - 2026-07-10

### Added

- Initial release: multi-provider LLM gateways (OpenAI, Claude, Gemini, Ollama,
  DeepSeek), single-shot generation with fallbacks, agent tool loops, structured
  output, JSON model catalog loading, and workspace-scoped filesystem tools.

[0.3.0.0]: https://github.com/aische/llm-simple/compare/v0.2.0.0...v0.3.0.0
[0.2.0.0]: https://github.com/aische/llm-simple/compare/v0.1.1.0...v0.2.0.0
[0.1.1.0]: https://github.com/aische/llm-simple/compare/v0.1.0.2...v0.1.1.0
[0.1.0.2]: https://github.com/aische/llm-simple/compare/v0.1.0.1...v0.1.0.2
[0.1.0.1]: https://github.com/aische/llm-simple/compare/v0.1.0.0...v0.1.0.1
[0.1.0.0]: https://github.com/aische/llm-simple/releases/tag/v0.1.0.0
