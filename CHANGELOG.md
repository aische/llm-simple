# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Fixed

- Claude and Gemini gateways honor catalog `baseUrl` / `baseUrlEnv` (previously
  ignored; always hit the public Anthropic/Google hosts). New
  `claudeGatewayWith` / `geminiGatewayWith` constructors; optional
  `CLAUDE_BASE_URL` and `GEMINI_BASE_URL` env overrides.

### Changed

- Example `model-catalog.json`: default Gemini entry is `gemini_lite` (`gemini-3.1-flash-lite`);
  removed deprecated `gemini-2.5-flash` config; raised example `maxTokens` to 4096; added
  optional `gpt_5_6_terra` (`gpt-5.6-terra`); corrected `gemini_lite` and `deepseek4flash`
  pricing (DeepSeek rates are peak cache-miss; off-peak is half).

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

[0.1.0.2]: https://github.com/aische/llm-simple/compare/v0.1.0.1...v0.1.0.2
[0.1.0.1]: https://github.com/aische/llm-simple/compare/v0.1.0.0...v0.1.0.1
[0.1.0.0]: https://github.com/aische/llm-simple/releases/tag/v0.1.0.0
