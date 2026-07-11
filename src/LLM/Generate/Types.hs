module LLM.Generate.Types
  ( GenRequest (..),
    GenerateResult,
    GenerateError (..),
    GenerateErrorResult (..),
    GenerateTextResult (..),
    StreamChunk (..),
    RoundTextRole (..),
    GeneratableObject,
  )
where

import Autodocodec (HasCodec)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.UUID.Types (UUID)
import LLM.Core.Abort (AbortSignal)
import LLM.Core.Types
  ( LLMError,
    LLMHooks,
    ToolCall,
    ToolDef (..),
    Turn,
  )
import LLM.Core.Usage (Usage)
import LLM.Generate.Logger (Hooks)

-- | A single provider request. Built manually or via 'createGenRequest'.
--
-- For structured output ('genObject', 'genObjectUntyped'), leave 'grTools' empty.
data GenRequest = GenRequest
  { -- | System prompt override (also available on 'Agent.agSystemPrompt').
    grSystemPrompt :: Maybe Text,
    -- | Conversation turns sent to the provider (may already be windowed).
    grMessages :: [Turn],
    -- | Tool definitions included in the provider request.
    grTools :: [ToolDef],
    -- | Cooperative cancellation checked before and during the call.
    grAbortSignal :: Maybe AbortSignal,
    -- | Provider wire hooks for raw request/response JSON.
    grLLMHooks :: LLMHooks,
    -- | Application hooks (logging, dumps, tool tracing).
    grHooks :: Hooks
  }

-- | Outcome of a generation call at the provider/fallback layer.
type GenerateResult a = Either GenerateError a

-- | Successful result from the agent loop or a single-shot text generation.
data GenerateTextResult = GenerateTextResult
  { -- | Matches 'RuntimeArgs.rtGenerationId' for this run.
    gtrGenerationId :: UUID,
    -- | New turns produced during this call (assistant and tool turns only).
    gtrNewMessages :: [Turn],
    -- | Final assistant text from the last model response.
    gtrText :: Text,
    -- | Accumulated token usage including cost estimates.
    gtrUsage :: Usage
  }
  deriving (Show, Eq)

-- | Failure result preserving partial progress from the current run.
data GenerateErrorResult = GenerateErrorResult
  { gerError :: GenerateError,
    -- | Turns produced before the failure (may include an in-progress tool round).
    gerPartialNewMessages :: [Turn],
    gerUsage :: Usage
  }
  deriving (Show, Eq)

-- | Errors surfaced by the generate and agent layers.
data GenerateError
  = -- | Provider-level failure (HTTP, network, parse, etc.).
    GErrLLM LLMError
  | -- | 'Agent.agMaxToolRounds' exhausted without a final text reply.
    GErrToolExceeded
  | -- | Every model in the fallback chain failed.
    GErrAllModelsFailed
  | -- | 'AbortSignal' fired before completion.
    GErrAborted
  | -- | Provider JSON did not decode into the requested typed object.
    GErrParseObjectError Text
  deriving (Show, Eq)

-- | Role assigned to streamed text for a single model round.
data RoundTextRole = AnswerRole | PreambleRole
  deriving (Show, Eq)

data StreamChunk
  = -- | Final answer text for the pre-allocated assistant message.
    AnswerDelta Text
  | -- | Chain-of-thought text from thinking mode.
    ReasoningDelta Text
  | -- | Text from an LLM round that also issued tool calls.
    PreambleDelta Text
  | -- | Unclassified text while the round role is still unknown.
    TextDelta Text
  | -- | Signals that prior 'TextDelta' chunks (or misclassified deltas) belong
    -- to the given role for this round.
    RoundTextRoleCommitted RoundTextRole
  | -- | A complete tool call from the provider stream.
    StreamToolCallChunk ToolCall
  deriving (Show, Eq)

-- | Constraint for typed structured output via Autodocodec.
type GeneratableObject t = (FromJSON t, HasCodec t)
