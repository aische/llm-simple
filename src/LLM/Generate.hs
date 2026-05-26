-- | Single-request generation functions.
module LLM.Generate
  ( generateTextLLM,
    streamTextLLM,
    generateTextWithFallbacks,
    streamTextWithFallbacks,
    generateObject,
    generateObjectUntyped,
    ModelConfig (..),
    ModelWithFallbacks (..),
    GenRequest (..),
    GenerateResult,
    GenerateError (..),
    GenerateErrorResult (..),
    GenerateTextResult (..),
    StreamChunk (..),
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
  )
where

import LLM.Generate.Generate
import LLM.Generate.GenerateObject
import LLM.Generate.Logger
import LLM.Generate.ModelConfig
import LLM.Generate.Types
