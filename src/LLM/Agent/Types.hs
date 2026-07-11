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

-- | Static agent configuration shared across requests.
data Agent = Agent
  { -- | Display name used in logs and debug hooks.
    agName :: Text,
    -- | Optional system prompt prepended to every provider request.
    agSystemPrompt :: Maybe Text,
    -- | Tool names from the 'ToolMap' that may be sent to the model.
    -- Only these names are exposed; the map may contain additional tools.
    agTools :: [Text],
    -- | Maximum number of model→tool rounds before returning 'GErrToolExceeded'.
    agMaxToolRounds :: Int,
    -- | When 'Just', only the last @n@ user messages (and their follow-on
    -- turns) are sent to the provider. Older turns remain in 'ToolContext' and
    -- can be retrieved via the auto-injected @get_history@ tool.
    agContextWindow :: Maybe Int
  }

-- | Per-generation runtime state and callbacks.
data RuntimeArgs = RuntimeArgs
  { -- | Correlates lifecycle events and result records for one run.
    rtGenerationId :: UUID,
    -- | When fired, in-flight generation and tool execution stop cooperatively.
    rtAbortSignal :: Maybe AbortSignal,
    -- | Provider wire hooks (request/response JSON). See 'LLMHooks'.
    rtLLMHooks :: LLMHooks,
    -- | Application hooks: logging, tool tracing, JSON dumps. See 'Hooks'.
    rtHooks :: Hooks,
    -- | Observer for 'GenerateEvent' lifecycle notifications (UI, telemetry).
    rtOnEvent :: EventObserver,
    -- | When 'True', mutating tools are rejected at execution time even if
    -- they appear in 'Agent.agTools'. Read-only tools still run.
    rtReadonly :: Bool
  }

-- | A tool: its definition (sent to the model) paired with its implementation.
--
-- 'toolExecute' receives a 'ToolContext' (full conversation + usage) and
-- the JSON arguments from the model.
data Tool result = Tool
  { toolDef :: ToolDef,
    toolExecute :: ToolContext -> Value -> IO result
  }

-- | Map from tool name to implementation. Built manually or via 'fsTools'.
type ToolMap result = Map Text (Tool result)

-- | Context passed to tool implementations during execution.
data ToolContext = ToolContext
  { -- | Full conversation history (not windowed), one message per turn
    tcConversation :: [Turn],
    -- | Accumulated token usage so far
    tcUsage :: Usage,
    -- | Index into 'tcConversation' where the visible window starts.
    -- Everything before this index is hidden from the model.
    -- The @get_history@ tool uses this to serve paginated history.
    tcWindowOffset :: Int,
    tcRuntimeArgs :: RuntimeArgs
  }

-- | Generation lifecycle event tagged with the run's 'rtGenerationId'.
data GenerateEvent = GenerateEvent
  { geGenerationId :: UUID,
    geDetail :: GenerateEventDetail
  }
  deriving (Show, Eq)

-- | Details carried by a 'GenerateEvent'.
data GenerateEventDetail
  = -- | The agent loop has started.
    GenerationStarted
  | -- | Final assistant text is ready.
    GenerationFinished GenerateTextResult
  | -- | The run failed; partial turns and usage are in the 'GenerateErrorResult'.
    GenerationFailed GenerateError GenerateErrorResult
  | -- | A new turn was appended (assistant tool-call round or tool results).
    MessageCreated Turn
  | -- | Streaming update to an in-flight assistant message (unused by default loop).
    MessageUpdated UUID Text
  | -- | The final assistant turn for a successful run.
    MessageFinalized Turn
  | -- | Tool execution for round @n@ is about to start.
    ToolRoundStarted Int
  | -- | Tool execution for round @n@ finished.
    ToolRoundFinished Int
  deriving (Show, Eq)

-- | Callback invoked for each 'GenerateEvent' during a run.
type EventObserver = GenerateEvent -> IO ()
