module LLM.Generate.Types where

import Autodocodec (HasCodec)
import Data.Aeson (FromJSON, Value)
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

-- | Agent configuration
data Agent = Agent
  { agName :: Text,
    agSystemPrompt :: Maybe Text,
    agTools :: [Tool],
    agWorkers :: Maybe [Text],
    agMaxToolRounds :: Int,
    agContextWindow :: Maybe Int -- max recent turns sent to the model; Nothing = all
  }

-- | Runtime arguments
data RuntimeArgs = RuntimeArgs
  { rtGenerationId :: UUID,
    rtAbortSignal :: Maybe AbortSignal,
    rtLLMHooks :: LLMHooks,
    rtHooks :: Hooks,
    rtOnEvent :: EventObserver,
    rtReadonly :: Bool
  }

-- | History of messages (empty or ending with final assistant message)
type History = [Turn]

-- | A tool: its definition (sent to the model) paired with its implementation.
-- 'toolExecute' receives a 'ToolContext' (full conversation + usage) and
-- the JSON arguments from the model.
data Tool = Tool
  { toolDef :: ToolDef,
    toolExecute :: ToolContext -> Value -> IO Text
  }

-- | Context passed to tool implementations during execution.
data ToolContext = ToolContext
  { -- | Full conversation history (not windowed), one message per turn
    tcConversation :: [Turn],
    -- | Accumulated token usage so far
    tcUsage :: Usage,
    -- | Index into 'tcConversation' where the visible window starts.
    -- Everything before this index is hidden from the model.
    -- A @get_history@ tool can use this to serve paginated history.
    tcWindowOffset :: Int,
    tcRuntimeArgs :: RuntimeArgs
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
    gerGenerationId :: UUID,
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
    AnswerDelta UUID Text
  | -- | Text from an LLM round that also issued tool calls.
    PreambleDelta Text
  | -- | A complete tool call from the provider stream.
    StreamToolCallChunk ToolCall
  deriving (Show, Eq)

-- | Generation lifecycle event
data GenerateEvent = GenerateEvent
  { geGenerationId :: UUID,
    geDetail :: GenerateEventDetail
  }
  deriving (Show, Eq)

-- | Generation lifecycle event details
data GenerateEventDetail
  = GenerationStarted
  | GenerationFinished GenerateTextResult
  | GenerationFailed GenerateError GenerateErrorResult
  | MessageCreated Turn
  | MessageUpdated UUID Text
  | MessageFinalized Turn
  | ToolRoundStarted Int
  | ToolRoundFinished Int
  deriving (Show, Eq)

type EventObserver = GenerateEvent -> IO ()

type GeneratableObject t = (FromJSON t, HasCodec t)
