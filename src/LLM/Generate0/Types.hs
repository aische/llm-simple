module LLM.Generate0.Types where

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
import LLM.Generate0.Logger (Hooks)

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
  { grGenerationId :: UUID,
    grNewMessages :: [Turn],
    grText :: Text,
    grUsage :: Usage
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

-- | Generation lifecycle event
-- data GenerateEvent = GenerateEvent
--   { geGenerationId :: UUID,
--     geDetail :: GenerateEventDetail
--   }
--   deriving (Show, Eq)

-- | Generation lifecycle event details
-- data GenerateEventDetail
--   = GenerationStarted
--   | GenerationFinished GenerateTextResult
--   | GenerationFailed GenerateError GenerateErrorResult
--   | MessageCreated Turn
--   | MessageUpdated UUID Text
--   | MessageFinalized Turn
--   | ToolRoundStarted Int
--   | ToolRoundFinished Int
--   deriving (Show, Eq)

-- type EventObserver = GenerateEvent -> IO ()

type GeneratableObject t = (FromJSON t, HasCodec t)
