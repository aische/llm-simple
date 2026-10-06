{-# LANGUAGE PatternSynonyms #-}

module LLM.Core.Types
  ( -- * Conversation turns
    Turn (..),
    pattern UserTurn,
    assistantTurn,
    ContentPart (..),
    PartBody (..),
    ThinkingContent (..),
    ProviderOpaque (..),
    textPart,
    thinkingPart,
    toolCallPart,
    projectText,
    projectReasoning,
    turnToolCalls,
    coalesceAdjacentTextParts,
    validateTurn,
    opaqueForProvider,
    stripForeignOpaque,

    -- * Chat
    ChatRequest (..),
    ChatResponse (..),
    mkChatResponse,
    LLMError (..),
    LLMTextResult,
    LLMObjectResult,
    LLMResult,

    -- * Tools
    ToolDef (..),
    ToolCall (..),
    mkToolCall,
    ToolResult (..),
    TypedTool (..),

    -- * Provider surface
    LLMGateway (..),
    LLMHooks (..),
    StreamEvent (..),
    ThinkingMode (..),
    MessageEncodeOptions (..),
    defaultMessageEncodeOptions,
    deepSeekMessageEncodeOptions,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
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

-- | Thinking / reasoning mode configuration shared across providers.
data ThinkingMode = ThinkingMode
  { tmEnabled :: Bool,
    tmEffort :: Maybe Text -- e.g. @high@, @max@, or a provider-specific token budget
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

-- | Opaque, provider-owned payload that must round-trip for replay.
--
-- Used for Claude thinking signatures, Gemini thought signatures, and any
-- similar provider-bound state. Consumers must treat 'poPayload' as opaque.
data ProviderOpaque = ProviderOpaque
  { poProvider :: Text,
    poModel :: Maybe Text,
    poPayload :: Value
  }
  deriving (Show, Eq, Generic)

instance ToJSON ProviderOpaque where
  toJSON po =
    object $
      [ "provider" .= po.poProvider,
        "payload" .= po.poPayload
      ]
        ++ ["model" .= m | Just m <- [po.poModel]]

instance FromJSON ProviderOpaque where
  parseJSON = withObject "ProviderOpaque" $ \o ->
    ProviderOpaque
      <$> o .: "provider"
      <*> o .:? "model"
      <*> o .: "payload"

-- | Displayable and/or opaque thinking content for one assistant part.
data ThinkingContent = ThinkingContent
  { thinkingText :: Maybe Text,
    thinkingOpaque :: Maybe ProviderOpaque
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Body of one ordered content part.
data PartBody
  = TextPart Text
  | ThinkingPart ThinkingContent
  | ToolCallPart ToolCall
  deriving (Show, Eq, Generic)

instance ToJSON PartBody where
  toJSON (TextPart t) = object ["type" .= ("text" :: Text), "text" .= t]
  toJSON (ThinkingPart tc) =
    object $
      ["type" .= ("thinking" :: Text)]
        ++ ["text" .= t | Just t <- [tc.thinkingText]]
        ++ ["opaque" .= o | Just o <- [tc.thinkingOpaque]]
  toJSON (ToolCallPart tc) =
    object
      [ "type" .= ("tool_call" :: Text),
        "tool_call" .= tc
      ]

instance FromJSON PartBody where
  parseJSON = withObject "PartBody" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "text" -> TextPart <$> o .: "text"
      "thinking" -> do
        mText <- o .:? "text"
        mOpaque <- o .:? "opaque"
        pure $ ThinkingPart (ThinkingContent mText mOpaque)
      "tool_call" -> ToolCallPart <$> o .: "tool_call"
      _ -> fail $ "Unknown part type: " <> T.unpack typ

-- | One ordered content part in a user or assistant message.
data ContentPart = ContentPart
  { partBody :: PartBody
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

textPart :: Text -> ContentPart
textPart t = ContentPart (TextPart t)

thinkingPart :: ThinkingContent -> ContentPart
thinkingPart tc = ContentPart (ThinkingPart tc)

toolCallPart :: ToolCall -> ContentPart
toolCallPart tc = ContentPart (ToolCallPart tc)

-- | A single turn in a conversation.
--
-- Prefer 'UserTurn' / 'assistantTurn' for simple text construction. Match
-- 'UserMessage' / 'AssistantMessage' when inspecting arbitrary ordered parts.
data Turn
  = UserMessage [ContentPart]
  | AssistantMessage [ContentPart]
  | ToolTurn [ToolResult]
  deriving (Show, Eq, Generic)

instance ToJSON Turn where
  toJSON (UserMessage parts) =
    object ["role" .= ("user" :: Text), "content" .= parts]
  toJSON (AssistantMessage parts) =
    object ["role" .= ("assistant" :: Text), "content" .= parts]
  toJSON (ToolTurn results) =
    object ["role" .= ("tool" :: Text), "results" .= results]

instance FromJSON Turn where
  parseJSON = withObject "Turn" $ \o -> do
    role <- o .: "role" :: Parser Text
    case role of
      "user" -> UserMessage <$> o .: "content"
      "assistant" -> AssistantMessage <$> o .: "content"
      "tool" -> ToolTurn <$> o .: "results"
      _ -> fail $ "Unknown turn role: " <> T.unpack role

-- | Bidirectional pattern for a single unannotated user text part.
pattern UserTurn :: Text -> Turn
pattern UserTurn text = UserMessage [ContentPart (TextPart text)]

{-# COMPLETE UserMessage, AssistantMessage, ToolTurn #-}

-- | Migration helper: emit thinking (if any), then text, then tool calls.
--
-- Does not recover arbitrary provider block order; use 'AssistantMessage'
-- with 'respContent' when replaying authoritative ordered parts.
assistantTurn :: Text -> Maybe Text -> [ToolCall] -> Turn
assistantTurn text mReasoning calls =
  AssistantMessage $
    [ thinkingPart (ThinkingContent (Just r) Nothing)
      | Just r <- [mReasoning],
        not (T.null r)
    ]
      ++ [textPart text | not (T.null text)]
      ++ map toolCallPart calls

-- | Concatenate text parts in order.
projectText :: [ContentPart] -> Text
projectText = T.concat . mapMaybe go
  where
    go (ContentPart (TextPart t)) = Just t
    go _ = Nothing

-- | First non-empty thinking text, if any.
projectReasoning :: [ContentPart] -> Maybe Text
projectReasoning = go
  where
    go [] = Nothing
    go (ContentPart (ThinkingPart tc) : rest) =
      case tc.thinkingText of
        Just t | not (T.null t) -> Just t
        _ -> go rest
    go (_ : rest) = go rest

-- | Tool calls in part order.
turnToolCalls :: [ContentPart] -> [ToolCall]
turnToolCalls = mapMaybe go
  where
    go (ContentPart (ToolCallPart tc)) = Just tc
    go _ = Nothing

-- | Merge runs of adjacent text parts (e.g. streamed token deltas).
coalesceAdjacentTextParts :: [ContentPart] -> [ContentPart]
coalesceAdjacentTextParts = go
  where
    go [] = []
    go (ContentPart (TextPart t1) : ContentPart (TextPart t2) : rest) =
      go (ContentPart (TextPart (t1 <> t2)) : rest)
    go (p : rest) = p : go rest

-- | Validate role/part combinations for a turn.
--
-- User messages may contain text parts; assistant messages may contain text,
-- thinking, and tool-call parts. Returns 'Left' with an error message when
-- the combination is invalid.
validateTurn :: Turn -> Either Text ()
validateTurn (UserMessage parts) =
  mapM_ userPart parts
  where
    userPart (ContentPart (TextPart _)) = Right ()
    userPart (ContentPart (ThinkingPart _)) =
      Left "user messages may not contain thinking parts"
    userPart (ContentPart (ToolCallPart _)) =
      Left "user messages may not contain tool-call parts"
validateTurn (AssistantMessage parts) =
  mapM_ assistantPart parts
  where
    assistantPart (ContentPart (TextPart _)) = Right ()
    assistantPart (ContentPart (ThinkingPart _)) = Right ()
    assistantPart (ContentPart (ToolCallPart _)) = Right ()
validateTurn (ToolTurn _) = Right ()

-- | Keep opaque metadata only when it belongs to @provider@.
opaqueForProvider :: Text -> Maybe ProviderOpaque -> Maybe ProviderOpaque
opaqueForProvider provider (Just o)
  | o.poProvider == provider = Just o
opaqueForProvider _ _ = Nothing

-- | Drop foreign opaque state from thinking parts and tool-call metadata.
--
-- Used by provider encoders during fallback so stored history is not mutated.
stripForeignOpaque :: Text -> ContentPart -> ContentPart
stripForeignOpaque provider (ContentPart (ThinkingPart tc)) =
  ContentPart $
    ThinkingPart
      tc
        { thinkingOpaque = opaqueForProvider provider tc.thinkingOpaque
        }
stripForeignOpaque provider (ContentPart (ToolCallPart tc)) =
  ContentPart $
    ToolCallPart
      tc
        { tcProviderMeta = opaqueForProvider provider tc.tcProviderMeta
        }
stripForeignOpaque _ p = p

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
-- 'tcProviderMeta' is opaque, provider-owned state that must round-trip back
-- to the same provider (and usually the same model) when this tool call is
-- replayed. Gemini thought signatures are the current example.
data ToolCall = ToolCall
  { tcId :: Text, -- provider-specific call id
    tcName :: Text,
    tcArguments :: Value,
    tcProviderMeta :: Maybe ProviderOpaque
  }
  deriving (Show, Eq, Generic)

instance ToJSON ToolCall where
  toJSON tc =
    object $
      [ "id" .= tc.tcId,
        "name" .= tc.tcName,
        "arguments" .= tc.tcArguments
      ]
        ++ ["provider_meta" .= m | Just m <- [tc.tcProviderMeta]]

instance FromJSON ToolCall where
  parseJSON = withObject "ToolCall" $ \o ->
    ToolCall
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "arguments"
      <*> o .:? "provider_meta"

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

-- | A response from an LLM provider.
--
-- 'respContent' is authoritative ordered content. 'respText' and
-- 'respReasoning' are convenience projections and may be lossy.
data ChatResponse = ChatResponse
  { respText :: Text,
    respContent :: [ContentPart],
    respUsage :: Maybe Usage,
    respReasoning :: Maybe Text
  }
  deriving (Show, Eq)

-- | Build a 'ChatResponse' with text/reasoning projections derived from parts.
--
-- Adjacent text parts are coalesced so streamed deltas become one part.
mkChatResponse :: [ContentPart] -> Maybe Usage -> ChatResponse
mkChatResponse parts usage =
  let coalesced = coalesceAdjacentTextParts parts
   in ChatResponse
        { respText = projectText coalesced,
          respContent = coalesced,
          respUsage = usage,
          respReasoning = projectReasoning coalesced
        }

-- | Events emitted during streaming
data StreamEvent
  = StreamReasoningDelta Text -- incremental chain-of-thought chunk
  | StreamDelta Text -- incremental answer text chunk
  | StreamToolCall ToolCall -- complete tool call
  deriving (Show, Eq)
