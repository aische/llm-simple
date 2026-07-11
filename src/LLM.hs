-- | Convenience re-exports for the most common integration path.
--
-- == Quick start
--
-- A typical agent application wires four pieces together:
--
-- 1. __Models__ — load a 'ModelConfig' (and optional fallbacks) from a JSON
--    catalog via 'loadModelOrThrow' or 'loadModelsOrThrow'. See 'LLM.Load' for
--    catalog format, provider names, and environment variables.
-- 2. __Tools__ — build a 'ToolMap' of implementations the model may call. Use
--    'fsTools' for the bundled workspace-scoped filesystem tools, or define
--    custom tools with 'TypedTool' and 'toTool'.
-- 3. __Agent__ — configure an 'Agent' (system prompt, tool allow-list, optional
--    context window) and per-request 'RuntimeArgs' (hooks, abort signal,
--    readonly mode, lifecycle events).
-- 4. __Run__ — call 'generateText' or 'streamText' from 'LLM.Agent' with the
--    conversation so far as a list of 'Turn's.
--
-- @
-- import LLM
-- import LLM.Agent (generateText)
--
-- main = do
--   model <- loadModelOrThrow \"./model-catalog.json\" \"llama_3_2\"
--   tools <- fsTools \"./workspace\"
--   let agent = Agent { agName = \"demo\", agSystemPrompt = Nothing,
--                       agTools = Map.keys tools, agMaxToolRounds = 10,
--                       agContextWindow = Nothing }
--   rt <- mkRuntime  -- your RuntimeArgs
--   result <- generateText agent (ModelWithFallbacks model []) tools rt
--                           [UserTurn \"hello\"]
--   ...
-- @
--
-- == Two layers
--
-- * __Agent layer__ ('LLM.Agent') — multi-round tool loop, context windowing,
--   automatic @get_history@ injection, lifecycle events, and readonly enforcement.
--   Prefer this for chat agents and coding assistants.
-- * __Generate layer__ ('LLM.Generate') — single provider request with retries
--   and model fallbacks ('generateTextWithFallbacks', 'streamTextWithFallbacks',
--   'genObject'). Use directly when you control the conversation loop yourself,
--   or via 'createGenRequest' to share agent configuration.
--
-- == Observability
--
-- * 'Hooks' — application-level callbacks (logging, request/response dumps,
--   tool call tracing). Passed through 'RuntimeArgs' / 'GenRequest'.
-- * 'LLMHooks' — provider wire logging. Set on 'RuntimeArgs' and bridged via
--   'llmHooks'.
module LLM
  ( -- * Core conversation types
    Turn (..),
    ToolCall (..),
    ToolResult (..),
    ToolDef (..),
    TypedTool (..),
    ChatResponse (..),
    ContentBlock (..),
    LLMGateway (..),
    LLMError (..),
    Usage (..),
    PricingInfo (..),
    emptyUsage,
    addUsage,
    estimateCost,
    ThinkingMode (..),
    LLMHooks (..),
    AbortSignal,
    getToolCalls,

    -- * Single-shot generation
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
    noHooks,
    llmHooks,
    defaultDebugHooks,
    debugHooks,

    -- * Agent and tool building blocks
    generateText,
    streamText,
    Agent (..),
    RuntimeArgs (..),
    Tool (..),
    ToolContext (..),
    createGenRequest,
    toTool,
    GenerateEvent (..),
    GenerateEventDetail (..),
    EventObserver,
    noEventObserver,

    -- * Model catalog and filesystem tool wiring
    loadModelOrThrow,
    loadModelsOrThrow,
    LoadConfigError (..),
    loadModelCatalog,
    ModelCatalogItem (..),
    ModelCatalogMap,
    fsTools,
    fsTools',
  )
where

import LLM.Agent
  ( Agent (..),
    EventObserver,
    GenerateEvent (..),
    GenerateEventDetail (..),
    RuntimeArgs (..),
    Tool (..),
    ToolContext (..),
    createGenRequest,
    generateText,
    noEventObserver,
    streamText,
    toTool,
  )
import LLM.Core
  ( AbortSignal,
    ChatResponse (..),
    ContentBlock (..),
    LLMError (..),
    LLMGateway (..),
    LLMHooks (..),
    PricingInfo (..),
    ThinkingMode (..),
    ToolCall (..),
    ToolDef (..),
    ToolResult (..),
    Turn (..),
    TypedTool (..),
    Usage (..),
    addUsage,
    emptyUsage,
    estimateCost,
    getToolCalls,
  )
import LLM.Generate
  ( GenRequest (..),
    GenerateError (..),
    GenerateErrorResult (..),
    GenerateResult,
    GenerateTextResult (..),
    GeneratableObject,
    Hooks (..),
    ModelConfig (..),
    ModelWithFallbacks (..),
    StreamChunk (..),
    RoundTextRole (..),
    debugHooks,
    defaultDebugHooks,
    genObject,
    genObjectUntyped,
    generateTextWithFallbacks,
    llmHooks,
    noHooks,
    streamTextWithFallbacks,
  )
import LLM.Load
  ( LoadConfigError (..),
    ModelCatalogItem (..),
    ModelCatalogMap,
    fsTools,
    fsTools',
    loadModelCatalog,
    loadModelOrThrow,
    loadModelsOrThrow,
  )
