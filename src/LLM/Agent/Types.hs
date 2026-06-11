module LLM.Agent.Types
  ( Agent (..),
    RuntimeArgs (..),
    Tool (..),
    ToolMap,
    ToolContext (..),
    GenerateEvent (..),
    GenerateEventDetail (..),
    EventObserver,
  )
where

import Data.Aeson (Value)
import Data.Map (Map)
import Data.Text (Text)
import Data.UUID.Types (UUID)
import LLM.Core.Abort (AbortSignal)
import LLM.Core.Types
  ( LLMHooks,
    ToolDef (..),
    Turn,
  )
import LLM.Core.Usage (Usage)
import LLM.Generate.Logger (Hooks)
import LLM.Generate.Types (GenerateError, GenerateErrorResult, GenerateTextResult)

-- | Agent configuration
data Agent = Agent
  { agName :: Text,
    agSystemPrompt :: Maybe Text,
    agTools :: [Text],
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

-- | A tool: its definition (sent to the model) paired with its implementation.
-- 'toolExecute' receives a 'ToolContext' (full conversation + usage) and
-- the JSON arguments from the model.
data Tool result = Tool
  { toolDef :: ToolDef,
    toolExecute :: ToolContext -> Value -> IO result
  }

type ToolMap result = Map Text (Tool result)

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
