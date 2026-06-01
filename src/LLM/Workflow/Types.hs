module LLM.Workflow.Types where

import Data.Aeson (Value)
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
import LLM.Generate.Types (GeneratableObject, GenerateError, GenerateErrorResult, GenerateTextResult)

-- | Agent configuration
data Agent = Agent
  { agName :: Text,
    agSystemPrompt :: Maybe Text,
    agTools :: [Text],
    agMaxToolRounds :: Int,
    agContextWindow :: Maybe Int -- max recent turns sent to the model; Nothing = all
  }

-- | Runtime arguments
data RuntimeArgs m = RuntimeArgs
  { rtGenerationId :: UUID,
    rtAbortSignal :: Maybe AbortSignal,
    rtLLMHooks :: LLMHooks,
    rtHooks :: Hooks,
    rtOnEvent :: EventObserver,
    rtReadonly :: Bool,
    rtToolMap :: ToolMap m
  }

-- | A tool: its definition (sent to the model) paired with its implementation.
-- 'toolExecute' receives a 'ToolContext' (full conversation + usage) and
-- the JSON arguments from the model.
data Tool m = Tool
  { toolDef :: ToolDef,
    toolExecute :: ToolContext m -> Value -> m (ToolOutcome m)
  }

data ToolOutcome m
  = ToolReply Text
  | ToolWorkflow (Workflow m PromptArgs Text) PromptArgs

data PromptArgs = PromptArgs
  { history :: [Turn],
    prompt :: Text
  }
  deriving (Show)

data Prompt = Prompt
  { agent :: AgentWithModels,
    prompt :: Text,
    history :: [Turn]
  }

data Pending = Pending
  { prompt :: Prompt,
    toolRounds :: [Turn]
  }

data Final = Final
  { prompt :: Prompt,
    history :: [Turn],
    newMessages :: [Turn],
    text :: Text
  }

data TranscriptPolicy i o where
  TranscriptPolicyFunc :: (i -> o) -> TranscriptPolicy i o
  TranscriptFinalToPromptArgs :: TranscriptPolicy Final PromptArgs
  TranscriptFinalText :: TranscriptPolicy Final Text

transcriptPolicy :: TranscriptPolicy i o -> i -> o
transcriptPolicy (TranscriptPolicyFunc f) i = f i
transcriptPolicy TranscriptFinalToPromptArgs final = PromptArgs {history = [], prompt = final.text}
transcriptPolicy TranscriptFinalText final = final.text

type MergePolicy o1 o2 o = o1 -> o2 -> o

data Workflow m i o where
  WPrompt :: AgentWithModels -> Workflow m PromptArgs Final
  WPromptO :: (GeneratableObject a) => AgentWithModels -> Workflow m PromptArgs a
  WSeq :: Workflow m i x -> Workflow m y o -> TranscriptPolicy x y -> Workflow m i o
  WPar :: Workflow m i x -> Workflow m i y -> MergePolicy x y o -> Workflow m i o
  WLift :: (i -> m o) -> Workflow m i o
  WMap :: Workflow m i o -> TranscriptPolicy o o' -> Workflow m i o'

-- WLoop :: Int -> Workflow m i o -> TranscriptPolicy o i -> Workflow m i o

data Step m o where
  SPrompt :: Pending -> Step m Final
  SPromptO :: (GeneratableObject a) => Pending -> Step m a
  SReturn :: o -> Step m o
  STool :: Pending -> Turn -> ToolCall -> Step m Text
  SThrow :: GenerateError -> Step m o
  SWorkflow :: Workflow m i o -> i -> Step m o

-- RunWorkflow :: Workflow m o -> Step m o

data Kont m o r where
  KEmpty :: Kont m o o
  KTool :: Pending -> Turn -> [ToolCall] -> [ToolResult] -> ToolCall -> Kont m Final r -> Kont m Text r
  KSeq1 :: Workflow m y o -> TranscriptPolicy x y -> Kont m o r -> Kont m x r
  KPar1 :: i -> Workflow m i y -> MergePolicy x y o -> Kont m o r -> Kont m x r
  KPar2 :: x -> MergePolicy x y o -> Kont m o r -> Kont m y r
  KMap :: TranscriptPolicy o o' -> Kont m o' r -> Kont m o r

-- KPar :: x -> Workflow m i y -> MergePolicy x y o -> Kont m y r -> Kont m i r
-- KLoop :: Int -> Workflow m i o -> TranscriptPolicy o i -> [CID] -> Kont m o r -> Kont m i r

data Stack m r where
  Stack :: (Step m o) -> (Kont m o r) -> Stack m r

data TypedWorkflowTool m c a = TypedWorkflowTool
  { twtName :: Text,
    twtDescription :: Text,
    twtReadonly :: Bool,
    twtExecute :: c -> a -> m (ToolOutcome m)
  }

type ToolMap m = Map.Map Text (Tool m)

-- | Context passed to tool implementations during execution.
data ToolContext m = ToolContext
  { -- | Full conversation history (not windowed), one message per turn
    tcConversation :: [Turn],
    -- | Accumulated token usage so far
    tcUsage :: Usage,
    -- | Index into 'tcConversation' where the visible window starts.
    -- Everything before this index is hidden from the model.
    -- A @get_history@ tool can use this to serve paginated history.
    tcWindowOffset :: Int,
    tcRuntimeArgs :: RuntimeArgs m
  }

data AgentWithModels = AgentWithModels
  { agent :: Agent,
    models :: ModelWithFallbacks
  }

instance Show AgentWithModels where
  show AgentWithModels {agent} = "AgentWithModels {agent = " <> show agent.agName <> "}"

newtype CID = CID UUID
  deriving (Eq, Ord, Show)

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
