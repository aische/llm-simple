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

data GenRequest = GenRequest
  { grSystemPrompt :: Maybe Text,
    grMessages :: [Turn],
    grTools :: [ToolDef],
    grAbortSignal :: Maybe AbortSignal,
    grLLMHooks :: LLMHooks,
    grHooks :: Hooks
  }

type GenerateResult a = Either GenerateError a

data GenerateTextResult = GenerateTextResult
  { gtrGenerationId :: UUID,
    gtrNewMessages :: [Turn],
    gtrText :: Text,
    gtrUsage :: Usage
  }
  deriving (Show, Eq)

data GenerateErrorResult = GenerateErrorResult
  { gerError :: GenerateError,
    gerPartialNewMessages :: [Turn], -- What was generated before the crash
    gerUsage :: Usage
  }
  deriving (Show, Eq)

data GenerateError
  = GErrLLM LLMError
  | GErrToolExceeded
  | GErrAllModelsFailed
  | GErrAborted
  | GErrParseObjectError Text
  deriving (Show, Eq)

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

type GeneratableObject t = (FromJSON t, HasCodec t)
