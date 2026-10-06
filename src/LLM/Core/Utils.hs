module LLM.Core.Utils
  ( hasToolCalls,
    getToolCalls,
    toolResult,
    isRetryable,
    withRetry,
    withTimeout,
    streamResponseJson,
    printValue,
    parseChatResponse,
  )
where

import Control.Retry (RetryPolicyM, RetryStatus (rsIterNumber), retrying)
import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson qualified as AE
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Types
  ( ChatResponse (..),
    ContentPart (..),
    ImageSource (..),
    LLMError (..),
    LLMResult,
    PartBody (..),
    ProviderOpaque (..),
    ThinkingContent (..),
    ToolCall (..),
    ToolResult (..),
    mkChatResponse,
    projectReasoning,
    thinkingPart,
    turnToolCalls,
  )
import LLM.Core.Usage (Usage (..))
import System.Timeout (timeout)

-- | Smart constructor for tool results
toolResult :: ToolCall -> Text -> ToolResult
toolResult tc = ToolResult tc.tcId tc.tcName

-- | Check whether a response contains tool calls
hasToolCalls :: ChatResponse -> Bool
hasToolCalls = not . null . getToolCalls

-- | Extract tool calls from a response
getToolCalls :: ChatResponse -> [ToolCall]
getToolCalls r = turnToolCalls r.respContent

-- | Whether an error is worth retrying
isRetryable :: LLMError -> Bool
isRetryable (HttpError status _) = status `elem` [429, 503, 529]
isRetryable (NetworkError _) = True
isRetryable _ = False

-- | Wrap an action with a timeout (ms). Returns 'TimeoutError' on expiry.
withTimeout :: Maybe Int -> IO (LLMResult a) -> IO (LLMResult a)
withTimeout Nothing action = action
withTimeout (Just us) action = do
  result <- timeout (us * 1000) action
  pure $ fromMaybe (Left TimeoutError) result

-- | Retry an action using the retry package's policy (exponential backoff + jitter).
-- The policy controls max attempts, delays, and jitter.
withRetry :: RetryPolicyM IO -> (Text -> IO ()) -> IO (LLMResult a) -> IO (LLMResult a)
withRetry policy logRetryableError action =
  retrying
    policy
    ( \status result -> case result of
        Left err | isRetryable err -> do
          logRetryableError $
            "Retryable error (attempt "
              <> T.pack (show (rsIterNumber status + 1))
              <> "): "
              <> T.pack (show err)
          pure True
        _ -> pure False
    )
    (const action)

-- | Build a synthetic JSON summary from a streamed ChatResponse,
-- used by providers to fire 'onResponse' after streaming completes.
streamResponseJson :: ChatResponse -> Value
streamResponseJson r =
  object
    [ "text" .= r.respText,
      "content" .= map partToJson r.respContent,
      "usage" .= fmap usageToJson r.respUsage,
      "reasoning" .= r.respReasoning
    ]
  where
    partToJson (ContentPart (TextPart t)) =
      object ["type" .= ("text" :: Text), "text" .= t]
    partToJson (ContentPart (ImagePart src)) =
      object ["type" .= ("image" :: Text), "image" .= imageToJson src]
    partToJson (ContentPart (ThinkingPart tc)) =
      object $
        ["type" .= ("thinking" :: Text)]
          ++ ["text" .= t | Just t <- [tc.thinkingText]]
          ++ ["opaque" .= opaqueToJson o | Just o <- [tc.thinkingOpaque]]
    partToJson (ContentPart (ToolCallPart tc)) =
      object $
        [ "type" .= ("tool_call" :: Text),
          "id" .= tc.tcId,
          "name" .= tc.tcName,
          "arguments" .= tc.tcArguments
        ]
          ++ ["provider_meta" .= opaqueToJson m | Just m <- [tc.tcProviderMeta]]
    imageToJson (ImageUrl url) =
      object ["type" .= ("url" :: Text), "url" .= url]
    imageToJson (ImageBase64 mediaType data_) =
      object
        [ "type" .= ("base64" :: Text),
          "media_type" .= mediaType,
          "data" .= data_
        ]
    opaqueToJson o =
      object $
        [ "provider" .= o.poProvider,
          "payload" .= o.poPayload
        ]
          ++ ["model" .= m | Just m <- [o.poModel]]
    usageToJson u =
      object
        [ "input_tokens" .= u.usageInputTokens,
          "output_tokens" .= u.usageOutputTokens,
          "cache_read_tokens" .= u.usageCacheReadTokens,
          "cache_creation_tokens" .= u.usageCacheCreationTokens
        ]

parseChatResponse :: Value -> Parser ChatResponse
parseChatResponse = AE.withObject "ChatResponse" $ \v -> do
  content <- v AE..: "content" >>= mapM parseContentPart
  usage <- v AE..:? "usage" >>= mapM parseUsage
  -- Synthetic stream summaries store reasoning beside content blocks.
  mReasoning <- v AE..:? "reasoning"
  let contentWithReasoning =
        case (projectReasoning content, mReasoning) of
          (Nothing, Just rc)
            | not (T.null rc) ->
                thinkingPart (ThinkingContent (Just rc) Nothing) : content
          _ -> content
  pure $ mkChatResponse contentWithReasoning usage
  where
    parseContentPart = AE.withObject "ContentPart" $ \o -> do
      t <- o AE..: "type"
      case (t :: Text) of
        "text" -> ContentPart . TextPart <$> o AE..: "text"
        "image" -> ContentPart . ImagePart <$> (o AE..: "image" >>= parseImageSource)
        "thinking" -> do
          mText <- o AE..:? "text"
          mOpaque <- o AE..:? "opaque" >>= mapM parseOpaque
          pure $ ContentPart (ThinkingPart (ThinkingContent mText mOpaque))
        "tool_call" -> do
          tcId <- o AE..: "id"
          tcName <- o AE..: "name"
          tcArgs <- o AE..: "arguments"
          tcMeta <- o AE..:? "provider_meta" >>= mapM parseOpaque
          pure $ ContentPart (ToolCallPart (ToolCall tcId tcName tcArgs tcMeta))
        _ -> fail "Unknown content part type"

    parseImageSource = AE.withObject "ImageSource" $ \o -> do
      typ <- o AE..: "type" :: Parser Text
      case typ of
        "url" -> ImageUrl <$> o AE..: "url"
        "base64" ->
          ImageBase64
            <$> o AE..: "media_type"
            <*> o AE..: "data"
        _ -> fail "Unknown image source type"

    parseOpaque = AE.withObject "ProviderOpaque" $ \o ->
      ProviderOpaque
        <$> o AE..: "provider"
        <*> o AE..:? "model"
        <*> o AE..: "payload"

    parseUsage = AE.withObject "Usage" $ \o -> do
      input <- o AE..: "input_tokens"
      output <- o AE..: "output_tokens"
      cacheRead <- fromMaybe 0 <$> o AE..:? "cache_read_tokens"
      cacheCreate <- fromMaybe 0 <$> o AE..:? "cache_creation_tokens"
      pure $
        Usage
          { usageInputTokens = input,
            usageOutputTokens = output,
            usageCacheReadTokens = cacheRead,
            usageCacheCreationTokens = cacheCreate,
            usageTotalCost = 0.0
          }

printValue :: Value -> IO ()
printValue val = L8.putStrLn (encode val)
