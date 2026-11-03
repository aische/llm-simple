# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- `LLM.Tools.FsLimits.atomicWriteWith` replaces a file by writing a
  temporary file in the same directory and renaming it over the target.
- `LLM.Tools.FsLimits.maxVisitedEntries` (20,000) and `maxOutputBytes`
  (100 KiB): the work and output bounds of the filesystem tools.
- `LLM.Core.ProviderUtils.readResponseBody` reads a non-streaming response
  with the size limits below, for `sendRequest` in custom providers
  (`reqBr POST url body opts readResponseBody`).
- `LLM.Generate.Logger.safeLLMHooks` (also in `LLM.Generate`): `safeHooks`
  for `LLMHooks`. `LLM.Generate.GenerateUtils.safeGenRequest` and
  `LLM.Agent.ToolUtils.safeRuntimeArgs` apply both to a request.
- `LLM.Agent.Generate.toolBudgetUsedUpPrompt` (also in `LLM.Agent`): the
  system-prompt line of the agent's final call after the last tool round.
- `LLM.Agent.ToolUtils.validateAgent` (also in `LLM.Agent`) lists the
  warnings the agent loop logs about an agent configuration.
- `LLM.Core.LLMProvider.withoutRoutingFields` removes the `_model`
  routing key from a request body (see below).
- `LLM.Core.Types.validateTurns` (also in `LLM.Core`): the invalid turns
  of a conversation, by index, with `validateTurn`'s reason.

### Fixed

- `onLLMRequest` (and so `onRequest` and the JSON dumps) now sees a Gemini
  request body as it is sent. It included the `_model` key that the
  provider uses to build the URL and removes before sending. A top-level
  `_model` key is now documented as routing data for any `LLMProvider`,
  and the request hooks never see it.
- `imageBase64Part` and `mkImageBase64` reject `=` anywhere but in the
  last one or two characters (`====`, `A===` and `ab=c` were accepted).
  **Behaviour change** for malformed input only.
- `validateTurn` is now applied: every generation entry point checks the
  request's messages once, before trying the first model, and logs each
  invalid turn (a user turn with a thinking or tool-call part, an
  assistant turn with an image) as `onLog Warn "turn <n>: <reason>"`, `n`
  counting from 0 in `grMessages`. The request is still sent unchanged.
  In an agent run the check runs once per model call. **0.5 will reject
  such a request** with an error instead.
- Loading the configuration rejects ambiguous or invalid values.
  **Behaviour change:** a catalog that loaded before can now fail to load.
  - `loadModelCatalog` (and every model loader) fails with
    `LoadModelCatalogError` naming the entry for a `modelConfigName` used
    twice (the last one silently won), `maxTokens <= 0`, or a negative
    `requestTimeout`, `throttleDelay`, `retryCount`, `jitterBackoff` or
    price. The whole catalog is rejected, not just the entry.
  - `loadProviderCatalog` fails with `LoadProviderCatalogError` for a
    `providerName` used twice in one file. Overriding a built-in provider
    is still allowed.
  - An API key variable that is set but empty or only whitespace counts as
    missing (`MissingApiKey`, "env var X is not set or is empty"). Before,
    the provider was available and sent the blank key, and an empty key
    also skipped the plain-HTTP base URL check.
- The agent checks its tool configuration. **Behaviour change:**
  - A name repeated in `agTools` is sent to the model once. Before, each
    copy was sent, a duplicate tool definition.
  - A name in `agTools` that the tool map lacks is logged as a warning
    (`onLog Warn`) once at the start of the run, as is a repeated name. It
    was dropped silently.
  - While `agContextWindow` is set, a caller tool named `get_history` is
    dropped with a warning, and only the built-in one is sent. Before, both
    were sent under one name.
  - A model call to an unknown tool fires `onToolError` (it already got
    the result "Unknown tool: <name>").
  - `agContextWindow = Just n` with `n <= 0` means no windowing, with a
    warning. A negative `n` sent an empty message list, and `windowOffset`
    returned the length of the conversation; it now returns 0.
- After the last allowed tool round, the agent makes one more model call
  instead of returning `GErrToolExceeded` with tool results the model never
  saw. **Behaviour change:** runs that ended in `GErrToolExceeded` mostly
  return an answer now, and a run makes up to `agMaxToolRounds + 1` model
  calls.
  - The final call is a normal request (the tools are still declared) with
    `toolBudgetUsedUpPrompt` appended to the system prompt.
  - A text reply is returned as a success. Tool calls in the reply are not
    run: each gets the result "Not run: tool round limit reached", and the
    run returns `GErrToolExceeded` with that assistant turn and tool turn
    in `gerPartialNewMessages`, so the history stays valid.
  - `agMaxToolRounds <= 0` makes only the final call.
- Invalid tool arguments are reported instead of silently replaced.
  **Behaviour change** for invalid arguments only: `find_files` returns an
  error for an unknown `type` (it searched for any kind) and for
  `maxResults <= 0`; `grep` for `maxResults <= 0`; `read_file_paginated`
  for `offset < 1` or `limit < 1` (all were clamped to 1). Values above a
  tool's hard cap are still lowered to the cap.
- `file_info` reports a symbolic link as a link. It resolved the path
  first, so it described the target and never said "symlink". It now
  shows `=== <path> | symbolic link ===` and the target relative to the
  workspace, with its kind (file, directory, or does not exist); a target
  outside the workspace is shown as "outside the workspace", not by its
  host path. **Behaviour change.**
- A path in which `..` would step back out of a symbolic link (`link/..`,
  `link/sub/../..`) is now refused with an error naming the link. The
  sandbox resolved `..` by name before following links, so such a path
  named a different file than the operating system would open.
  **Behaviour change** for such paths only. A surplus `..` at the
  filesystem root no longer turns the path into a relative one.
- `move_file` no longer overwrites its destination silently. If anything
  exists there, a symbolic link included (even a dangling one), it returns
  "Error: destination already exists: <dst>; remove it first" and changes
  nothing. **Behaviour change.** The check and the rename are not atomic,
  so another process can still create the destination in between.
- An exception thrown by a hook no longer fails a run whose side effects
  (a tool call, a paid model call) already happened. **Behaviour change:**
  every entry point (`generateTextLLM`, `streamTextLLM`, the
  `*WithFallbacks` functions, `genObject`, `genObjectUntyped`, and the
  agent's `generateText`, `streamText` and `generateObject*`) now wraps
  `Hooks` with `safeHooks` and `LLMHooks` with `safeLLMHooks`, which drop
  synchronous exceptions. A hook that throws to stop a run no longer stops
  it; use an `AbortSignal`. Asynchronous exceptions still propagate.
- An `HttpException` thrown by the caller's own stream callback (for
  example from an HTTP call it makes) is no longer reported as the
  provider's `NetworkError` and retried, which repeated the callback's
  effects. It now propagates from `streamTextLLM` (and the functions built
  on it) unchanged, after one attempt, like any other exception from the
  callback.
- Provider response bodies are now bounded. **Behaviour change:**
  - An error body (non-200) is read up to 64 KiB and cut there, with `...`
    appended, for both streaming and non-streaming calls. `readAll` does
    the same.
  - A 200 body of a non-streaming call larger than 32 MiB is a
    `ParseError "response body exceeds 33554432 bytes"`, as is a single
    stream event (or line) larger than 8 MiB. Before, either was read
    into memory in full.
  - The bundled providers read bodies with `readResponseBody`. Custom
    providers that read with `bsResponse` keep working, unbounded.
- `writefile`, `replace_in_file` and `multi_replace_in_file` no longer
  leave a partial or empty file when a write fails, and two edits of one
  file at the same time no longer lose one of them. **Behaviour change:**
  - The file is written to a temporary file next to it, then renamed over
    it. An existing file keeps its mode bits; a new file gets the default
    mode (`0666` minus the umask), as before. The data is flushed but not
    `fsync`ed.
  - The file gets a new inode. A hard link to it keeps the old content, so
    a write no longer changes a file outside the workspace through a hard
    link (a read still can). The new file is owned by the user running the
    process; ACLs and extended attributes are not copied.
  - The edit tools hold a per-file lock from the read to the write. It
    covers threads of the same process only; edits from other processes
    can still be lost.
- The search and listing tools now bound their total work, not just their
  output. **Behaviour change:**
  - `find_files`, `grep` and `directory_tree` look at no more than
    `maxVisitedEntries` directory entries per call. A walk that runs out
    stops and says so in its header ("stopped after visiting 20000
    entries; narrow the path or pattern"). Before, a large tree with no
    match was walked in full.
  - `grep` searches files as it walks and stops at `maxResults`, instead of
    collecting every candidate first and reading on to count files. When it
    stops early, the header shows the file count as `N+`, and "stopped at
    maxResults" replaces "truncated".
  - `readdir` lists at most 2,000 entries and ends with a line saying how
    many there are. `find_files`, `grep`, `readdir` and `directory_tree`
    cut their output at `maxOutputBytes` and say so.
  - `file_info` stops counting a directory's entries at its 10,000 cap.
  - A `read_file_paginated` page holds at most `maxOutputBytes` of text. A
    page ends before a line that would go over; a single longer line is cut
    and marked. Lines before `offset` are skipped without being decoded.
  - `read_file_paginated` no longer reports a file that grew past its
    10 MiB cap during the read as ending at the cap; it returns an error.

## [0.4.1.0] - 2026-11-02

### Added

- `waitAborted :: AbortSignal -> IO ()` (in `LLM.Core.Abort` and
  `LLM.Core`) blocks until the signal fires.
- `LLM.Core.Utils.interruptOnAbort` runs an action and interrupts it when
  the signal fires.
- `LLM.Tools.FsConfig.withWorkspacePaths` runs a filesystem action so that
  an `IOException` it throws names its file relative to the workspace.
- `LLM.Core.ProviderUtils.decodeObjectText` and `unexpectedResponse`, and
  `LLM.Providers.OpenAI.parseOpenAIObjectResponse`: the structured-output
  parsing the bundled providers use, for custom providers.

### Fixed

- An abort now interrupts work in flight. Before, the signal was only
  checked between steps, so it could not stop a non-streaming call that was
  waiting for its response, a stream that stalled after its first event, a
  throttle or backoff sleep, or a running tool. **Behaviour change:**
  - When `grAbortSignal` / `rtAbortSignal` fires, the model call in flight
    is interrupted wherever it is, its HTTP connection is closed, and the
    call returns `GErrAborted`.
  - A tool call that is running when the signal fires is interrupted. It
    counts as not run: it gets the result "Aborted before execution", like
    the calls after it, and `onToolError` reports `"Aborted"`. Its side
    effects may have partly happened.
  - The interrupt is an asynchronous exception thrown to the calling thread.
    Tool code that catches every exception, asynchronous ones included, can
    swallow it and keep running.
  - A stream that stalls after its first event still blocks until it is
    aborted: there is no idle timeout yet.
- A truncated or corrupt stream is no longer reported as success.
  **Behaviour change:** some streams that used to return `Right` with a
  partial answer, or throw, now return `Left`.
  - The stream parsers did not check for the end-of-stream marker, so a
    connection that closed cleanly partway through returned the text received
    so far as a complete answer. Each protocol now needs its marker:
    `[DONE]` or a `finish_reason` (OpenAI-compatible: OpenAI, DeepSeek,
    Ollama), a `finishReason` or `blockReason` (Gemini), or `message_stop`
    (Claude). A stream that ends without its marker is
    `NetworkError "stream ended before <marker>"`. It is retried and then
    falls back like any other network error.
  - A data event that is not valid JSON was skipped. It is now a
    `ParseError` quoting the start of the event.
  - A stream event that is not valid UTF-8 threw a `UnicodeException`, which
    escaped the typed error path, so fallback models were never tried. It is
    now a `ParseError`.
  - An in-band provider error event (for example Claude's `overloaded_error`)
    still takes precedence over these checks.
- API keys are no longer sent over plain HTTP. A provider that sends a key
  with a base URL (or `*_BASE_URL` override) of `http://` to a non-loopback
  host is now unavailable, with `InvalidBaseUrl` saying the URL is insecure;
  `loadGatewaysFromCatalogOrThrow` throws for it. `http://` to `localhost`,
  `127.0.0.0/8` or `::1` is still allowed, as is any `http://` host for
  Ollama and other keyless providers. **Behaviour change:** a catalog that
  pointed a keyed provider at a remote `http://` proxy no longer loads that
  provider; use `https://`.
- `mcThrottleDelay` (`throttleDelay`) now applies before every attempt,
  retries included. It was slept once before the first attempt, so retries
  ran after their backoff alone and could hit the rate limit the throttle
  was meant to avoid. Before a retry, throttle and backoff add up.
  **Behaviour change:** a call that retries with a throttle set now takes
  longer.
- `loadGatewaysWithDotenv` no longer swallows cancellation. It ignored every
  exception from loading `.env`, including `ThreadKilled`, `UserInterrupt`
  and timeouts, so a cancellation that arrived during loading was lost. It
  now ignores only `IOException` (a missing or unreadable `.env`). The
  example executables and the README examples do the same.
  **Behaviour change:** a `.env` that dotenv cannot parse used to be ignored
  silently; its parse error is now thrown.
- JSON dumps (`withJsonDump`, `debugHooks`, `defaultDebugHooks`) stay in
  their directory and no longer overwrite each other. The provider name,
  which for a custom provider comes from `providers.json`, was used in the
  file name as is, so a name like `../leak` wrote outside the dump
  directory. Dumps written in the same millisecond had the same name, and
  the later one replaced the earlier. In the provider name, every character
  other than an ASCII letter, digit, `-` or `_` now becomes `_`, and each
  dump is created as a new file with a unique suffix:
  `{provider}-{request|response}-{timestamp}-{unique}.json`. **Behaviour
  change:** dump file names have the extra suffix, and dump files are now
  readable only by their owner (mode 0600).
- Structured-output responses that cannot be parsed are no longer reported
  as `EmptyResponse`. OpenAI, Ollama, DeepSeek and Claude reported model
  text that is not valid JSON, and a response body without the expected
  fields, as `EmptyResponse`, dropping the reason; Gemini did the same for
  text that is not valid JSON. These are now `ParseError`s with the JSON
  decoder's message and the first 200 characters of the text or body.
  `EmptyResponse` is kept for content that is actually empty: no choice or
  text block, or text that is empty once code fences are stripped.
  **Behaviour change:** code that matched `EmptyResponse` for these cases
  gets `ParseError`. Neither is retried, and both still fall back to the
  next model.
- Tool errors no longer show the model absolute host paths. An
  `IOException` from a filesystem tool (a missing file, a permission error)
  was shown to the model with its absolute path, revealing where the
  workspace lives on the host. The bundled filesystem tools now name the
  file relative to the workspace (`sub/missing.txt: removeLink: does not
  exist ...`), or leave it out if it is outside the workspace. For any tool,
  the model-facing error drops an absolute file name from an
  `IOException`. **Behaviour change:** the `IOException`s the filesystem
  tools throw carry the relative path, and `onToolError` gets the same
  redacted text as the model.
- `isSymlink` (`LLM.Tools.FsConfig`) no longer fails open. When the lookup
  failed for any reason, it answered `False` ("not a link"), so the sandbox
  checks and the recursive tools treated a path they could not inspect as a
  regular file or directory. It still answers `False` when nothing exists
  at the path (missing, or a component is not a directory), so new paths
  can be written. Any other failure (permission denied, a symlink loop) now
  answers `True`, so the path is refused or skipped like a symlink.
  **Behaviour change:** such a path now fails the sandbox check, is skipped
  by `grep`, `find_files` and `directory_tree`, and is listed with `@` by
  `readdir`.

## [0.4.0.1] - 2026-10-27

### Fixed

- A write through a dangling symlink no longer escapes the workspace.
  `sandboxPath` tested path existence with `doesPathExist`, which stats
  through symlinks and so reported a link whose target does not exist as
  absent. Such a link was treated as an ordinary new file: the containment
  check ran on the link's own path, and the tool's later open followed the
  link. A workspace holding `output -> /tmp/outside`, with the target
  missing, let `writefile output` create `/tmp/outside`. Link components are
  now lstat'd whether or not they resolve, so a dangling link is resolved to
  its target before the containment check and the chain walk rejects a
  dangling link planted after it. A dangling link whose target is inside the
  workspace still resolves, and now yields the target's path rather than the
  link's.

## [0.4.0.0] - 2026-10-24

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
  without bound for large `retryCount`s. `Retry-After` is still ignored;
  the `mcJitterBackoff` docs and the README now say so.
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
- Gemini errors, blocks and finish reasons are no longer all
  `EmptyResponse`. A stream that fails midway stays HTTP 200 and sends
  `{"error": {"code": 503, ...}}` as a data event; that event was ignored,
  so the call ended as a non-retryable `EmptyResponse` (or as a shortened
  success). It is now `HttpError code body`, retried like the same error
  before streaming, as for Claude and OpenAI. A response without content
  names why Gemini stopped: a blocked prompt (`promptFeedback.blockReason`)
  or a filter finish reason (`SAFETY`, `RECITATION`, `PROHIBITED_CONTENT`,
  …) gives the new `ResponseBlocked`; `MAX_TOKENS` (common when thinking
  uses the whole budget) and other finish reasons give a `ParseError` naming
  the reason. A response with content is still returned when the token
  limit cut it off. Structured output cut off by `MAX_TOKENS` is a
  `ParseError` saying so. **Breaking:** new `LLMError` constructor
  `ResponseBlocked`.

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

[0.4.1.0]: https://github.com/aische/llm-simple/compare/v0.4.0.1...v0.4.1.0
[0.4.0.1]: https://github.com/aische/llm-simple/compare/v0.4.0.0...v0.4.0.1
[0.4.0.0]: https://github.com/aische/llm-simple/compare/v0.3.0.0...v0.4.0.0
[0.3.0.0]: https://github.com/aische/llm-simple/compare/v0.2.0.0...v0.3.0.0
[0.2.0.0]: https://github.com/aische/llm-simple/compare/v0.1.1.0...v0.2.0.0
[0.1.1.0]: https://github.com/aische/llm-simple/compare/v0.1.0.2...v0.1.1.0
[0.1.0.2]: https://github.com/aische/llm-simple/compare/v0.1.0.1...v0.1.0.2
[0.1.0.1]: https://github.com/aische/llm-simple/compare/v0.1.0.0...v0.1.0.1
[0.1.0.0]: https://github.com/aische/llm-simple/releases/tag/v0.1.0.0
