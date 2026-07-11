module LLM.Core.Types
  ( Turn (..),
    assistantTurn,
    ContentBlock (..),
    ChatRequest (..),
    ChatResponse (..),
    LLMError (..),
    LLMTextResult,
    LLMObjectResult,
    LLMResult,
    ToolDef (..),
    ToolCall (..),
    mkToolCall,
    LLMGateway (..),
    ToolResult (..),
    StreamEvent (..),
    TypedTool (..),
    LLMHooks (..),
    ThinkingMode (..),
    MessageEncodeOptions (..),
    defaultMessageEncodeOptions,
    deepSeekMessageEncodeOptions,
  )
where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import GHC.Generics (Generic)
import LLM.Core.Usage (Usage)

-- | Provider adapter for one LLM backend.
--
-- Implement the three callbacks to add a custom provider, or use the
-- bundled constructors in 'LLM.Providers'.
data LLMGateway = LLMGateway
  { -- | Short name used in logs and hooks (e.g. @openai@).
    gwName :: Text,
    -- | Non-streaming chat completion.
    gwGenerateText :: LLMHooks -> ChatRequest -> IO LLMTextResult,
    -- | Streaming chat; invoke the callback for each 'StreamEvent'.
    gwStreamText :: LLMHooks -> ChatRequest -> (StreamEvent -> IO ()) -> IO LLMTextResult,
    -- | Structured JSON output against a caller-supplied schema.
    gwGenerateObject :: LLMHooks -> Value -> ChatRequest -> IO LLMObjectResult
  }

-- | Result of an LLM operation: either an error, a chat response, or a generated object
type LLMResult a = Either LLMError a

type LLMTextResult = LLMResult ChatResponse

type LLMObjectResult = LLMResult (Value, Maybe Usage)

-- | Hooks for observing raw provider request and response JSON on the wire.
--
-- Distinct from 'LLM.Generate.Logger.Hooks', which covers application-level
-- logging and tool tracing. Bridge to this type via 'llmHooks'.
data LLMHooks = LLMHooks
  { onLLMRequest :: Text -> Value -> IO (),
    onLLMResponse :: Text -> Value -> IO (),
    onLLMResponseError :: Text -> Text -> IO ()
  }

-- | DeepSeek thinking mode configuration.
data ThinkingMode = ThinkingMode
  { tmEnabled :: Bool,
    tmEffort :: Maybe Text -- e.g. @high@ or @max@
  }
  deriving (Show, Eq)

-- | Controls how conversation turns are encoded for provider APIs.
newtype MessageEncodeOptions = MessageEncodeOptions
  { meoIncludeReasoning :: Bool
  }
  deriving (Show, Eq)

defaultMessageEncodeOptions :: MessageEncodeOptions
defaultMessageEncodeOptions = MessageEncodeOptions {meoIncludeReasoning = False}

deepSeekMessageEncodeOptions :: MessageEncodeOptions
deepSeekMessageEncodeOptions = MessageEncodeOptions {meoIncludeReasoning = True}

-- | A single turn in a conversation
data Turn
  = UserTurn Text
  | AssistantTurn Text (Maybe Text) [ToolCall] -- content, reasoning_content, tool calls
  | ToolTurn [ToolResult]
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

assistantTurn :: Text -> Maybe Text -> [ToolCall] -> Turn
assistantTurn = AssistantTurn

-- | A tool definition sent to the model
data ToolDef = ToolDef
  { toolName :: Text,
    toolDescription :: Text,
    toolParameters :: Value, -- JSON Schema object
    -- | When 'True', the tool is advertised as read-only and remains available
    -- when 'RuntimeArgs.rtReadonly' is set. Mutating tools should use 'False'.
    toolReadonly :: Bool
  }
  deriving (Show, Eq)

-- | Typed tool definition before conversion to 'Tool' via 'toTool'.
--
-- Define one with an Autodocodec argument type; 'toTool' derives the JSON
-- Schema for 'ToolDef.toolParameters' automatically.
data TypedTool c a = TypedTool
  { ttoolName :: Text,
    ttoolDescription :: Text,
    ttoolReadonly :: Bool,
    ttoolExecute :: c -> a -> IO Text
  }

-- | A tool invocation returned by the model.
--
-- 'tcProviderMeta' is an opaque, provider-owned JSON bag that must round-trip
-- back to the provider on subsequent requests when this tool call is replayed
-- as part of the conversation history. It exists because some providers
-- (currently Gemini 2.5 thinking models, with their @thoughtSignature@) bind
-- internal state to a specific tool-call part and reject the request if it
-- isn't echoed back verbatim. Each provider is responsible for the shape of
-- its own metadata; consumers should treat it as opaque.
data ToolCall = ToolCall
  { tcId :: Text, -- provider-specific call id
    tcName :: Text,
    tcArguments :: Value,
    tcProviderMeta :: Maybe Value
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Smart constructor for a 'ToolCall' with no provider metadata. Use this
-- everywhere except when a provider parser is attaching its own metadata.
mkToolCall :: Text -> Text -> Value -> ToolCall
mkToolCall cid name args = ToolCall cid name args Nothing

-- | The result of executing a tool, sent back to the model
data ToolResult = ToolResult
  { trCallId :: Text, -- unique call id (matches tcId)
    trName :: Text, -- function name (matches tcName)
    trContent :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Errors from LLM operations
data LLMError
  = HttpError Int Text -- status code + raw body
  | NetworkError Text -- connection / DNS / TLS failure
  | TimeoutError -- request timed out
  | ParseError Text -- JSON we couldn't make sense of
  | EmptyResponse -- valid JSON, but no content in it
  | ToolLoopExceeded Int -- hit the max tool rounds limit
  | Aborted -- user cancelled the request
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A request to an LLM provider
data ChatRequest = ChatRequest
  { reqModel :: Text,
    reqConversation :: [Turn],
    reqSystem :: Maybe Text,
    reqMaxTokens :: Int,
    reqTemperature :: Maybe Double,
    reqTools :: [ToolDef],
    reqThinking :: Maybe ThinkingMode
  }
  deriving (Show, Eq)

-- | A content block in a response — either text or a tool call
data ContentBlock
  = TextBlock Text
  | ToolCallBlock ToolCall
  deriving (Show, Eq)

-- | A response from an LLM provider
data ChatResponse = ChatResponse
  { respText :: Text,
    respContent :: [ContentBlock],
    respUsage :: Maybe Usage,
    respReasoning :: Maybe Text
  }
  deriving (Show, Eq)

-- | Events emitted during streaming
data StreamEvent
  = StreamReasoningDelta Text -- incremental chain-of-thought chunk
  | StreamDelta Text -- incremental answer text chunk
  | StreamToolCall ToolCall -- complete tool call
  deriving (Show, Eq)
