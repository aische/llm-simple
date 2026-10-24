{-# LANGUAGE PatternSynonyms #-}

module LLM.Core.Types
  ( -- * Conversation turns
    Turn (..),
    pattern UserTurn,
    assistantTurn,
    ContentPart (..),
    PartBody (..),
    CacheHint (..),
    ImageSource (..),
    ThinkingContent (..),
    ProviderOpaque (..),
    textPart,
    thinkingPart,
    toolCallPart,
    imageUrlPart,
    imageBase64Part,
    withCacheHint,
    cacheEphemeral,
    mkImageBase64,
    supportedImageMediaTypes,
    commonImageMediaTypes,
    projectText,
    projectReasoning,
    turnToolCalls,
    conversationHasImages,
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

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
    object,
    withObject,
    withText,
    (.:),
    (.:?),
    (.=),
  )
import Data.Aeson.Types (Parser)
import Data.Char (isSpace, isAsciiUpper, isAsciiLower, isDigit)
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
    -- | MIME types of inline ('ImageBase64') images the provider accepts.
    -- Models on this gateway are skipped for conversations with other types.
    gwImageMediaTypes :: [Text],
    -- | Whether the provider accepts URL ('ImageUrl') images. Models on a
    -- gateway without it are skipped for conversations with URL images.
    gwImageUrlSupport :: Bool,
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
data MessageEncodeOptions = MessageEncodeOptions
  { -- | Send assistant thinking text as @reasoning_content@.
    meoIncludeReasoning :: Bool,
    -- | Send @reasoning_content@ on every assistant message, @""@ when the
    -- turn has no thinking text (DeepSeek thinking mode rejects assistant
    -- messages without it). Only takes effect with 'meoIncludeReasoning'.
    meoRequireReasoning :: Bool
  }
  deriving (Show, Eq)

defaultMessageEncodeOptions :: MessageEncodeOptions
defaultMessageEncodeOptions =
  MessageEncodeOptions {meoIncludeReasoning = False, meoRequireReasoning = False}

-- | DeepSeek encoding with thinking off. 'deepSeekBuildBodyPairs' sets
-- 'meoRequireReasoning' when the request enables thinking.
deepSeekMessageEncodeOptions :: MessageEncodeOptions
deepSeekMessageEncodeOptions =
  MessageEncodeOptions {meoIncludeReasoning = True, meoRequireReasoning = False}

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

-- | Image input by HTTPS URL or base64 payload.
data ImageSource
  = ImageUrl Text
  | ImageBase64 {imageMediaType :: Text, imageData :: Text}
  deriving (Show, Eq, Generic)

instance ToJSON ImageSource where
  toJSON (ImageUrl url) =
    object ["type" .= ("url" :: Text), "url" .= url]
  toJSON (ImageBase64 mediaType data_) =
    object
      [ "type" .= ("base64" :: Text),
        "media_type" .= mediaType,
        "data" .= data_
      ]

instance FromJSON ImageSource where
  parseJSON = withObject "ImageSource" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "url" -> ImageUrl <$> o .: "url"
      "base64" ->
        ImageBase64
          <$> o .: "media_type"
          <*> o .: "data"
      _ -> fail $ "Unknown image source type: " <> T.unpack typ

-- | Image MIME types every provider with vision accepts (Claude, OpenAI,
-- Ollama, DeepSeek). Gemini also accepts HEIC / HEIF.
commonImageMediaTypes :: [Text]
commonImageMediaTypes =
  [ "image/jpeg",
    "image/png",
    "image/gif",
    "image/webp"
  ]

-- | MIME types accepted by 'mkImageBase64' / 'imageBase64Part'. Not every
-- provider accepts all of them; see 'gwImageMediaTypes'.
supportedImageMediaTypes :: [Text]
supportedImageMediaTypes =
  commonImageMediaTypes ++ ["image/heic", "image/heif"]

-- | Validate MIME type and base64 payload for an inline image.
mkImageBase64 :: Text -> Text -> Either Text ImageSource
mkImageBase64 mediaType rawData
  | mediaType `notElem` supportedImageMediaTypes =
      Left $
        "unsupported image media type: "
          <> mediaType
          <> "; expected one of: "
          <> T.intercalate ", " supportedImageMediaTypes
  | T.null cleaned =
      Left "image base64 data must not be empty"
  | not (isBase64Text cleaned) =
      Left "image data is not valid base64"
  | otherwise =
      Right $ ImageBase64 mediaType cleaned
  where
    cleaned = T.filter (not . isSpace) rawData

isBase64Text :: Text -> Bool
isBase64Text t =
  let n = T.length t
   in n > 0
        && n `mod` 4 == 0
        && T.all isBase64Char t
  where
    isBase64Char c =
      isAsciiUpper c
        || isAsciiLower c
        || isDigit c
        || c == '+'
        || c == '/'
        || c == '='

-- | Body of one ordered content part.
data PartBody
  = TextPart Text
  | ImagePart ImageSource
  | ThinkingPart ThinkingContent
  | ToolCallPart ToolCall
  deriving (Show, Eq, Generic)

instance ToJSON PartBody where
  toJSON (TextPart t) = object ["type" .= ("text" :: Text), "text" .= t]
  toJSON (ImagePart src) = object ["type" .= ("image" :: Text), "image" .= src]
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
      "image" -> ImagePart <$> o .: "image"
      "thinking" -> do
        mText <- o .:? "text"
        mOpaque <- o .:? "opaque"
        pure $ ThinkingPart (ThinkingContent mText mOpaque)
      "tool_call" -> ToolCallPart <$> o .: "tool_call"
      _ -> fail $ "Unknown part type: " <> T.unpack typ

-- | Portable prompt-cache breakpoint intent.
--
-- Unsupported providers ignore the hint without dropping the underlying
-- content. Claude serializes 'CacheEphemeral' as wire @cache_control@.
data CacheHint = CacheEphemeral
  deriving (Show, Eq, Generic)

instance ToJSON CacheHint where
  toJSON CacheEphemeral = String "ephemeral"

instance FromJSON CacheHint where
  parseJSON = withText "CacheHint" $ \t ->
    case t of
      "ephemeral" -> pure CacheEphemeral
      _ -> fail $ "Unknown cache hint: " <> T.unpack t

-- | One ordered content part in a user or assistant message.
data ContentPart = ContentPart
  { partBody :: PartBody,
    -- | Optional cache breakpoint. Portable intent; see 'CacheHint'.
    partCacheHint :: Maybe CacheHint
  }
  deriving (Show, Eq, Generic)

instance ToJSON ContentPart where
  toJSON (ContentPart body mHint) =
    object $
      ("partBody" .= body) : ["partCacheHint" .= h | Just h <- [mHint]]

instance FromJSON ContentPart where
  parseJSON = withObject "ContentPart" $ \o ->
    ContentPart
      <$> o .: "partBody"
      <*> o .:? "partCacheHint"

-- | Content part with no cache hint.
mkPart :: PartBody -> ContentPart
mkPart body = ContentPart body Nothing

textPart :: Text -> ContentPart
textPart t = mkPart (TextPart t)

thinkingPart :: ThinkingContent -> ContentPart
thinkingPart tc = mkPart (ThinkingPart tc)

toolCallPart :: ToolCall -> ContentPart
toolCallPart tc = mkPart (ToolCallPart tc)

-- | User image part from a publicly reachable URL.
imageUrlPart :: Text -> ContentPart
imageUrlPart url = mkPart (ImagePart (ImageUrl url))

-- | User image part from base64 data; validates MIME type and payload.
imageBase64Part :: Text -> Text -> Either Text ContentPart
imageBase64Part mediaType data_ =
  mkPart . ImagePart <$> mkImageBase64 mediaType data_

-- | Attach a cache hint to a content part.
withCacheHint :: CacheHint -> ContentPart -> ContentPart
withCacheHint hint cp = cp {partCacheHint = Just hint}

-- | Mark a content part with Claude's default ephemeral cache breakpoint.
cacheEphemeral :: ContentPart -> ContentPart
cacheEphemeral = withCacheHint CacheEphemeral

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
--
-- Matches only when there is no cache hint. Use 'UserMessage' for annotated
-- or multi-part content.
pattern UserTurn :: Text -> Turn
pattern UserTurn text = UserMessage [ContentPart (TextPart text) Nothing]

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
    go (ContentPart (TextPart t) _) = Just t
    go _ = Nothing

-- | All non-empty thinking text concatenated in order (without separator,
-- as it was streamed), or 'Nothing' if there is none.
projectReasoning :: [ContentPart] -> Maybe Text
projectReasoning parts =
  case [t | ContentPart (ThinkingPart tc) _ <- parts, Just t <- [tc.thinkingText], not (T.null t)] of
    [] -> Nothing
    ts -> Just (T.concat ts)

-- | Tool calls in part order.
turnToolCalls :: [ContentPart] -> [ToolCall]
turnToolCalls = mapMaybe go
  where
    go (ContentPart (ToolCallPart tc) _) = Just tc
    go _ = Nothing

-- | Merge runs of adjacent text parts (e.g. streamed token deltas).
--
-- Only coalesces when both parts share the same cache hint, so breakpoints
-- are not lost.
coalesceAdjacentTextParts :: [ContentPart] -> [ContentPart]
coalesceAdjacentTextParts = go
  where
    go [] = []
    go (ContentPart (TextPart t1) h1 : ContentPart (TextPart t2) h2 : rest)
      | h1 == h2 =
          go (ContentPart (TextPart (t1 <> t2)) h1 : rest)
    go (p : rest) = p : go rest

-- | Merge runs of adjacent thinking parts (e.g. streamed Gemini thought
-- chunks). Only parts without an opaque payload and with the same cache hint
-- are merged; opaque parts (e.g. Claude signed blocks) stay one per block.
coalesceAdjacentThinkingParts :: [ContentPart] -> [ContentPart]
coalesceAdjacentThinkingParts = go
  where
    go [] = []
    go
      ( ContentPart (ThinkingPart (ThinkingContent t1 Nothing)) h1
          : ContentPart (ThinkingPart (ThinkingContent t2 Nothing)) h2
          : rest
        )
        | h1 == h2 =
            go (ContentPart (ThinkingPart (ThinkingContent (t1 <> t2) Nothing)) h1 : rest)
    go (p : rest) = p : go rest

-- | Whether any turn in the conversation contains an image part.
conversationHasImages :: [Turn] -> Bool
conversationHasImages = any turnHasImage
  where
    turnHasImage (UserMessage parts) = any isImagePart parts
    turnHasImage (AssistantMessage parts) = any isImagePart parts
    turnHasImage (ToolTurn _) = False
    isImagePart (ContentPart (ImagePart _) _) = True
    isImagePart _ = False

-- | Validate role/part combinations for a turn.
--
-- User messages may contain text and image parts; assistant messages may
-- contain text, thinking, and tool-call parts. Returns 'Left' with an error
-- message when the combination is invalid. Cache hints do not affect validity.
validateTurn :: Turn -> Either Text ()
validateTurn (UserMessage parts) =
  mapM_ userPart parts
  where
    userPart (ContentPart (TextPart _) _) = Right ()
    userPart (ContentPart (ImagePart _) _) = Right ()
    userPart (ContentPart (ThinkingPart _) _) =
      Left "user messages may not contain thinking parts"
    userPart (ContentPart (ToolCallPart _) _) =
      Left "user messages may not contain tool-call parts"
validateTurn (AssistantMessage parts) =
  mapM_ assistantPart parts
  where
    assistantPart (ContentPart (TextPart _) _) = Right ()
    assistantPart (ContentPart (ThinkingPart _) _) = Right ()
    assistantPart (ContentPart (ToolCallPart _) _) = Right ()
    assistantPart (ContentPart (ImagePart _) _) =
      Left "assistant messages may not contain image parts"
validateTurn (ToolTurn _) = Right ()

-- | Keep opaque metadata only when it belongs to @provider@.
opaqueForProvider :: Text -> Maybe ProviderOpaque -> Maybe ProviderOpaque
opaqueForProvider provider (Just o)
  | o.poProvider == provider = Just o
opaqueForProvider _ _ = Nothing

-- | Drop foreign opaque state from thinking parts and tool-call metadata.
--
-- Used by provider encoders during fallback so stored history is not mutated.
-- Cache hints are preserved.
stripForeignOpaque :: Text -> ContentPart -> ContentPart
stripForeignOpaque provider (ContentPart (ThinkingPart tc) hint) =
  ContentPart
    ( ThinkingPart
        tc
          { thinkingOpaque = opaqueForProvider provider tc.thinkingOpaque
          }
    )
    hint
stripForeignOpaque provider (ContentPart (ToolCallPart tc) hint) =
  ContentPart
    ( ToolCallPart
        tc
          { tcProviderMeta = opaqueForProvider provider tc.tcProviderMeta
          }
    )
    hint
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
  | Aborted -- the request's abort signal fired
  | -- | Model lacks a required catalog capability (e.g. vision) for this request.
    UnsupportedCapability Text
  | -- | The provider blocked the prompt or the response (e.g. a safety or
    -- recitation filter); the text names the reason. Not retried.
    ResponseBlocked Text
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
-- Adjacent text parts, and adjacent thinking parts without an opaque
-- payload, are coalesced so streamed deltas become one part.
mkChatResponse :: [ContentPart] -> Maybe Usage -> ChatResponse
mkChatResponse parts usage =
  let coalesced = coalesceAdjacentThinkingParts (coalesceAdjacentTextParts parts)
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
