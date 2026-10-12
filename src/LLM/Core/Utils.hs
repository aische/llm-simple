module LLM.Core.Utils
  ( hasToolCalls,
    getToolCalls,
    toolResult,
    isRetryable,
    withRetry,
    withTimeout,
    withFirstEventTimeout,
    trySync,
    streamResponseJson,
    printValue,
    parseChatResponse,
  )
where

import Control.Concurrent (forkIOWithUnmask, killThread, myThreadId, threadDelay, throwTo)
import Control.Exception
  ( Exception (..),
    SomeAsyncException,
    SomeException,
    asyncExceptionFromException,
    asyncExceptionToException,
    bracket,
    handleJust,
    throwIO,
    try,
    uninterruptibleMask_,
  )
import Control.Monad (guard, unless)
import Control.Retry (RetryPolicyM, RetryStatus (rsIterNumber), retrying)
import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson qualified as AE
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Unique (Unique, newUnique)
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
isRetryable TimeoutError = True
isRetryable _ = False

-- | Wrap an action with a timeout (ms). Returns 'TimeoutError' on expiry.
withTimeout :: Maybe Int -> IO (LLMResult a) -> IO (LLMResult a)
withTimeout Nothing action = action
withTimeout (Just ms) action = do
  result <- timeout (ms * 1000) action
  pure $ fromMaybe (Left TimeoutError) result

-- | Like 'withTimeout', but for streaming: the timeout (ms) only covers the
-- time until the first event reaches the callback. Once an event has
-- arrived, the stream may run as long as it needs. Returns 'TimeoutError'
-- if no event arrives in time.
--
-- @run@ receives the wrapped callback and must pass every event through it.
withFirstEventTimeout ::
  Maybe Int ->
  ((e -> IO ()) -> IO (LLMResult a)) ->
  (e -> IO ()) ->
  IO (LLMResult a)
withFirstEventTimeout Nothing run onEvent = run onEvent
withFirstEventTimeout (Just ms) run onEvent = do
  startedRef <- newIORef False
  caller <- myThreadId
  ex <- FirstEventTimeout <$> newUnique
  -- Same structure as 'System.Timeout.timeout': a watchdog thread throws a
  -- unique async exception at the caller unless the first event came in.
  let watchdog = forkIOWithUnmask $ \unmask -> unmask $ do
        threadDelay (ms * 1000)
        started <- readIORef startedRef
        unless started $ throwTo caller ex
      onEvent' e = writeIORef startedRef True >> onEvent e
  handleJust
    (guard . (== ex))
    (\_ -> pure (Left TimeoutError))
    (bracket watchdog (uninterruptibleMask_ . killThread) (\_ -> run onEvent'))

-- | Like @try \@SomeException@, but re-throws asynchronous exceptions
-- ('System.Timeout.timeout', 'Control.Concurrent.killThread', @cancel@), so
-- a caller that interrupts the thread is not turned into an ordinary error.
trySync :: IO a -> IO (Either SomeException a)
trySync act = do
  r <- try act
  case r of
    Left e | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
    _ -> pure r

newtype FirstEventTimeout = FirstEventTimeout Unique
  deriving (Eq)

instance Show FirstEventTimeout where
  show _ = "<<first event timeout>>"

instance Exception FirstEventTimeout where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

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
    partToJson (ContentPart (TextPart t) hint) =
      object $
        ["type" .= ("text" :: Text), "text" .= t]
          ++ cacheHintPair hint
    partToJson (ContentPart (ImagePart src) hint) =
      object $
        ["type" .= ("image" :: Text), "image" .= imageToJson src]
          ++ cacheHintPair hint
    partToJson (ContentPart (ThinkingPart tc) hint) =
      object $
        ["type" .= ("thinking" :: Text)]
          ++ ["text" .= t | Just t <- [tc.thinkingText]]
          ++ ["opaque" .= opaqueToJson o | Just o <- [tc.thinkingOpaque]]
          ++ cacheHintPair hint
    partToJson (ContentPart (ToolCallPart tc) hint) =
      object $
        [ "type" .= ("tool_call" :: Text),
          "id" .= tc.tcId,
          "name" .= tc.tcName,
          "arguments" .= tc.tcArguments
        ]
          ++ ["provider_meta" .= opaqueToJson m | Just m <- [tc.tcProviderMeta]]
          ++ cacheHintPair hint
    cacheHintPair hint =
      ["cache_hint" .= h | Just h <- [hint]]
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
      mHint <- o AE..:? "cache_hint"
      body <- case (t :: Text) of
        "text" -> TextPart <$> o AE..: "text"
        "image" -> ImagePart <$> (o AE..: "image" >>= parseImageSource)
        "thinking" -> do
          mText <- o AE..:? "text"
          mOpaque <- o AE..:? "opaque" >>= mapM parseOpaque
          pure $ ThinkingPart (ThinkingContent mText mOpaque)
        "tool_call" -> do
          tcId <- o AE..: "id"
          tcName <- o AE..: "name"
          tcArgs <- o AE..: "arguments"
          tcMeta <- o AE..:? "provider_meta" >>= mapM parseOpaque
          pure $ ToolCallPart (ToolCall tcId tcName tcArgs tcMeta)
        _ -> fail "Unknown content part type"
      pure $ ContentPart body mHint

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
