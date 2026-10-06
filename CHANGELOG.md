# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

[0.1.1.0]: https://github.com/aische/llm-simple/compare/v0.1.0.2...v0.1.1.0
[0.1.0.2]: https://github.com/aische/llm-simple/compare/v0.1.0.1...v0.1.0.2
[0.1.0.1]: https://github.com/aische/llm-simple/compare/v0.1.0.0...v0.1.0.1
[0.1.0.0]: https://github.com/aische/llm-simple/releases/tag/v0.1.0.0
