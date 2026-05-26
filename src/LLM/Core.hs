-- | Core types and functions for LLM operations
module LLM.Core
  ( LLMGateway (..),
    ChatRequest (..),
    ChatResponse (..),
    LLMTextResult,
    LLMObjectResult,
    LLMResult,
    LLMError (..),
    Turn (..),
    ToolCall (..),
    ToolResult (..),
    LLMHooks (..),
    TypedTool (..),
    ToolDef (..),
    ContentBlock (..),
    StreamEvent (..),
    ThinkingMode (..),
    MessageEncodeOptions (..),
    defaultMessageEncodeOptions,
    deepSeekMessageEncodeOptions,
    assistantTurn,
    Usage (..),
    PricingInfo (..),
    emptyUsage,
    addUsage,
    estimateCost,
    AbortSignal,
    newAbortSignal,
    abort,
    isAborted,
    isAbortedMaybe,
    LLMProvider (..),
    hasToolCalls,
    getToolCalls,
    toolResult,
  )
where

import LLM.Core.Abort
import LLM.Core.LLMProvider
import LLM.Core.Types
import LLM.Core.Usage
import LLM.Core.Utils
