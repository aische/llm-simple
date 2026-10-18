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
    -- | Cooperative cancellation. Checked before each model in the fallback
    -- chain and before each attempt (also after throttle and backoff
    -- sleeps); a stream is cut off at its next event. A non-streaming HTTP
    -- call already in flight is not interrupted. Yields 'GErrAborted'.
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
    -- | Turns produced before the failure. Safe to append to the history:
    -- every tool call has a result. On an abort during a tool round, calls
    -- that did not run get the result "Aborted before execution".
    gerPartialNewMessages :: [Turn],
    gerUsage :: Usage
  }
  deriving (Show, Eq)

-- | Errors surfaced by the generate and agent layers.
data GenerateError
  = -- | Provider-level failure (HTTP, network, parse, etc.).
    GErrLLM LLMError
  | -- | 'Agent.agMaxToolRounds' exhausted without a final text reply. The
    -- last round's tool results are in 'gerPartialNewMessages' but were not
    -- sent to the model.
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
  | -- | The previous attempt of this model call failed after streaming some
    -- chunks, and a retry or fallback model starts over. Discard every chunk
    -- received since the start of this model call (round). Only sent when
    -- the failed attempt emitted at least one chunk.
    StreamAttemptReset
  deriving (Show, Eq)

-- | Constraint for typed structured output via Autodocodec.
type GeneratableObject t = (FromJSON t, HasCodec t)
