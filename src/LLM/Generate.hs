-- | Single provider requests with retries and model fallbacks.
--
-- These functions perform one logical LLM call (possibly retried, possibly
-- falling through 'ModelWithFallbacks') but do __not__ run a tool loop.
-- For automatic tool execution use 'LLM.Agent.generateText' instead.
--
-- Lower-level entry points 'generateTextLLM' and 'streamTextLLM' target a
-- single 'ModelConfig' without fallback orchestration.
module LLM.Generate
  ( generateTextLLM,
    streamTextLLM,
    generateTextWithFallbacks,
    streamTextWithFallbacks,
    genObject,
    genObjectUntyped,
    ModelConfig (..),
    ModelWithFallbacks (..),
    GenRequest (..),
    GenerateResult,
    GenerateError (..),
    GenerateErrorResult (..),
    GenerateTextResult (..),
    StreamChunk (..),
    RoundTextRole (..),
    GeneratableObject,
    Hooks (..),
    Logger,
    LogLevel (..),
    noHooks,
    withStderrLogger,
    withJsonDump,
    noLogger,
    stderrLogger,
    safeHooks,
    debugHooks,
    defaultDebugHooks,
    llmHooks,
  )
where

import LLM.Generate.Generate
import LLM.Generate.GenerateObject
import LLM.Generate.GenerateUtils (llmHooks)
import LLM.Generate.Logger
import LLM.Generate.ModelConfig
import LLM.Generate.Types
