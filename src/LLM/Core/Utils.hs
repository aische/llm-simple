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
    ContentBlock (..),
    LLMError (..),
    LLMResult,
    ToolCall (..),
    ToolResult (..),
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
getToolCalls r = concatMap go r.respContent
  where
    go (ToolCallBlock tc) = [tc]
    go _ = []

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
      "content" .= map blockToJson r.respContent,
      "usage" .= fmap usageToJson r.respUsage,
      "reasoning" .= r.respReasoning
    ]
  where
    blockToJson (TextBlock t) = object ["type" .= ("text" :: Text), "text" .= t]
    blockToJson (ToolCallBlock tc) =
      object
        [ "type" .= ("tool_call" :: Text),
          "id" .= tc.tcId,
          "name" .= tc.tcName,
          "arguments" .= tc.tcArguments
        ]
    usageToJson u =
      object
        [ "input_tokens" .= u.usageInputTokens,
          "output_tokens" .= u.usageOutputTokens
        ]

parseChatResponse :: Value -> Parser ChatResponse
parseChatResponse = AE.withObject "ChatResponse" $ \v -> do
  text <- v AE..: "text"
  content <- v AE..: "content" >>= mapM parseContentBlock
  usage <- v AE..:? "usage" >>= mapM parseUsage
  reasoning <- v AE..:? "reasoning"
  pure
    ChatResponse
      { respText = text,
        respContent = content,
        respUsage = usage,
        respReasoning = reasoning
      }
  where
    parseContentBlock = AE.withObject "ContentBlock" $ \o -> do
      t <- o AE..: "type"
      case (t :: Text) of
        "text" -> TextBlock <$> o AE..: "text"
        "tool_call" -> do
          tcId <- o AE..: "id"
          tcName <- o AE..: "name"
          tcArgs <- o AE..: "arguments"
          pure $ ToolCallBlock $ ToolCall tcId tcName tcArgs
        _ -> fail "Unknown content block type"

    parseUsage = AE.withObject "Usage" $ \o -> do
      input <- o AE..: "input_tokens"
      output <- o AE..: "output_tokens"
      pure $ Usage input output 0.0

printValue :: Value -> IO ()
printValue val = L8.putStrLn (encode val)
