module LLM.Generate.Types
  ( GenRequest (..),
    GenerateResult,
    GenerateError (..),
    GenerateErrorResult (..),
    GenerateTextResult (..),
    StreamChunk (..),
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

data StreamChunk
  = -- | Final answer text for the pre-allocated assistant message.
    AnswerDelta Text
  | -- | Text from an LLM round that also issued tool calls.
    PreambleDelta Text
  | -- | A complete tool call from the provider stream.
    StreamToolCallChunk ToolCall
  deriving (Show, Eq)

type GeneratableObject t = (FromJSON t, HasCodec t)
