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
  { -- | Name of the agent. The agent entry points prefix every 'LLM.Generate.Logger.onLog'
    -- message of a run with @[\<agName\>] @; an empty name adds no prefix.
    -- It is not sent to the model.
    agName :: Text,
    -- | Optional system prompt prepended to every provider request.
    agSystemPrompt :: Maybe Text,
    -- | Tool names from the 'ToolMap' that may be sent to the model.
    -- Only these names are exposed; the map may contain additional tools.
    -- A repeated name is sent once; a name missing from the map is not sent
    -- and logged as a warning at the start of the run.
    agTools :: [Text],
    -- | Maximum number of model→tool rounds. Once that many rounds have
    -- run, the agent makes one final model call with the tools still
    -- declared and a line appended to the system prompt asking for a text
    -- answer ('LLM.Agent.Generate.toolBudgetUsedUpPrompt'). A text reply
    -- ends the run successfully. Tool calls in that reply are not run: each
    -- gets the result @\"Not run: tool round limit reached\"@ and the run
    -- returns v'LLM.Generate.Types.GErrToolExceeded'. So every tool that runs has its result
    -- seen by the model, and a run makes at most @agMaxToolRounds + 1@
    -- model calls. A value @<= 0@ makes only the final call.
    agMaxToolRounds :: Int,
    -- | When 'Just', only the last @n@ user messages (and their follow-on
    -- turns) are sent to the provider. Older turns remain in 'ToolContext' and
    -- can be retrieved via the auto-injected @get_history@ tool, which
    -- replaces a caller tool of that name. @'Just' n@ with @n <= 0@ means no
    -- windowing (as 'Nothing'), with a warning at the start of the run.
    agContextWindow :: Maybe Int
  }

-- | Per-generation runtime state and callbacks.
data RuntimeArgs = RuntimeArgs
  { -- | Correlates lifecycle events and result records for one run.
    rtGenerationId :: UUID,
    -- | When fired, the model call or tool call in flight is interrupted and
    -- the run ends with 'LLM.Generate.Types.GErrAborted'.
    rtAbortSignal :: Maybe AbortSignal,
    -- | Provider wire hooks (request/response JSON). See 'LLMHooks'.
    rtLLMHooks :: LLMHooks,
    -- | Application hooks: logging, tool tracing, JSON dumps. See 'Hooks'.
    rtHooks :: Hooks,
    -- | Observer for 'GenerateEvent' lifecycle notifications (UI, telemetry).
    rtOnEvent :: EventObserver,
    -- | When 'True', mutating tools are rejected at execution time even if
    -- they appear in 'LLM.Agent.Types.agTools'. Read-only tools still run.
    rtReadonly :: Bool
  }

-- | A tool: its definition (sent to the model) paired with its implementation.
--
-- 'LLM.Agent.Types.toolExecute' receives a 'ToolContext' (full conversation + usage) and
-- the JSON arguments from the model.
data Tool result = Tool
  { toolDef :: ToolDef,
    toolExecute :: ToolContext -> Value -> IO result
  }

-- | Map from tool name to implementation. Built manually or via 'LLM.Load.FsTools.fsTools'.
type ToolMap result = Map Text (Tool result)

-- | Context passed to tool implementations during execution.
data ToolContext = ToolContext
  { -- | Full conversation history (not windowed), one message per turn
    tcConversation :: [Turn],
    -- | Accumulated token usage so far
    tcUsage :: Usage,
    -- | Index into 'LLM.Agent.Types.tcConversation' where the visible window starts.
    -- Everything before this index is hidden from the model.
    -- The @get_history@ tool uses this to serve paginated history.
    tcWindowOffset :: Int,
    tcRuntimeArgs :: RuntimeArgs
  }

-- | Generation lifecycle event tagged with the run's 'LLM.Agent.Types.rtGenerationId'.
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
  | -- | Never emitted. Deprecated; removed in 0.5.
    MessageUpdated UUID Text
  | -- | The final assistant turn for a successful run.
    MessageFinalized Turn
  | -- | Tool execution for round @n@ is about to start.
    ToolRoundStarted Int
  | -- | Tool execution for round @n@ finished.
    ToolRoundFinished Int
  deriving (Show, Eq)

{-# DEPRECATED MessageUpdated "Never emitted by the agent loop; removed in 0.5. Streamed text arrives through the streamText callback." #-}

-- | Callback invoked for each 'GenerateEvent' during a run.
type EventObserver = GenerateEvent -> IO ()
