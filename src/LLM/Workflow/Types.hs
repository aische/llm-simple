module LLM.Workflow.Types
  ( Agent (..),
    RuntimeArgs (..),
    Tool (..),
    ToolContext (..),
    GenerateEvent (..),
    GenerateEventDetail (..),
    EventObserver,
    TranscriptPolicy (..),
    MergePolicy (..),
    FinalResult (..),
    PromptArgs (..),
    Prompt (..),
    PromptState (..),
    Step (..),
    Kont (..),
    CID (..),
    Workflow (..),
    AgentWithModels (..),
    PromptToolCalling (..),
    ToolOutcome (..),
    TypedWorkflowTool (..),
    ToolMap,
  )
where

import Data.Aeson (Value)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.UUID.Types (UUID)
import LLM.Core.Abort (AbortSignal)
import LLM.Core.Types
  ( LLMHooks,
    ToolCall (..),
    ToolDef (..),
    ToolResult (..),
    Turn (..),
  )
import LLM.Core.Usage (Usage)
import LLM.Generate (ModelWithFallbacks)
import LLM.Generate.Logger (Hooks)
import LLM.Generate.Types (GenerateError, GenerateErrorResult, GenerateTextResult)

-- | Agent configuration
data Agent = Agent
  { agName :: Text,
    agSystemPrompt :: Maybe Text,
    agTools :: [Text],
    agUTools :: [Text],
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
    rtReadonly :: Bool,
    rtToolMap :: ToolMap
  }

-- | A tool: its definition (sent to the model) paired with its implementation.
-- 'toolExecute' receives a 'ToolContext' (full conversation + usage) and
-- the JSON arguments from the model.
data Tool = Tool
  { toolDef :: ToolDef,
    toolExecute :: ToolContext -> Value -> IO ToolOutcome
  }

data ToolOutcome
  = ToolReply Text
  | ToolWorkflow Workflow PromptArgs

data TypedWorkflowTool c a = TypedWorkflowTool
  { twtName :: Text,
    twtDescription :: Text,
    twtReadonly :: Bool,
    twtExecute :: c -> a -> IO ToolOutcome
  }

type ToolMap = Map.Map Text Tool

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

data AgentWithModels = AgentWithModels
  { agent :: Agent,
    models :: ModelWithFallbacks
  }

instance Show AgentWithModels where
  show AgentWithModels {agent} = "AgentWithModels {agent = " <> show agent.agName <> "}"

data PromptArgs = PromptArgs
  { history :: [Turn],
    prompt :: Text
  }
  deriving (Show)

data Prompt = Prompt
  { agentWithModels :: AgentWithModels,
    history :: [Turn],
    prompt :: Text
  }
  deriving (Show)

data PromptState
  = PromptStatePending [Turn]
  | PromptStateFinal FinalResult
  | PromptStateToolCalls PromptToolCalling
  deriving (Show)

data FinalResult = FinalResult
  { history :: [Turn],
    prompt :: Text,
    toolTurns :: [Turn],
    assistantTurn :: Turn,
    text :: Text
  }
  deriving (Show)

data PromptToolCalling = PromptToolCalling
  { toolRounds :: [Turn],
    answer :: Turn,
    toolCalls :: [ToolCall],
    toolResults :: [ToolResult]
  }
  deriving (Show)

data Step
  = RunPrompt Prompt PromptState
  | RunWorkflow Workflow PromptArgs
  deriving (Show)

data Kont
  = KontToolCall Prompt PromptToolCalling ToolCall
  | KontSeq Workflow TranscriptPolicy
  | KontPar1 Workflow PromptArgs MergePolicy
  | KontPar2 FinalResult MergePolicy
  | KontLoop Int Workflow TranscriptPolicy (Map CID [Turn])
  | KontUpdate CID
  deriving (Show)

newtype CID = CID UUID
  deriving (Eq, Ord, Show)

data Workflow
  = WPrompt AgentWithModels (Maybe CID)
  | WSeq Workflow Workflow TranscriptPolicy
  | WPar Workflow Workflow MergePolicy
  | WLoop Int Workflow TranscriptPolicy [CID]
  deriving (Show)

data TranscriptPolicy
  = TranscriptPolicy
  | TranscriptSummaryOnly
  deriving (Show)

data MergePolicy = MergePolicy
  deriving (Show)

-- * Generation events -------------------------------------------------------

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
