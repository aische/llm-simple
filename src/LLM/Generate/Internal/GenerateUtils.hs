-- | Implementation of "LLM.Generate.GenerateUtils", plus the variants of
-- the fallback loop that the agent loop uses ('TurnCheck').
module LLM.Generate.Internal.GenerateUtils
  ( maybeThrottle,
    mkRequest,
    usageWithModelCost,
    callWithRetry,
    callWithRetryTimeout,
    withModelFallbacks,
    withModelFallbacksRotated,
    TurnCheck (..),
    withModelFallbacksRotatedWith,
    logInvalidTurns,
    validateModelCapabilities,
    llmHooks,
    safeGenRequest,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe)
import LLM.Core.Abort (isAbortedMaybe)
import LLM.Core.Types
  ( ChatRequest (..),
    ContentPart (..),
    ImageSource (..),
    LLMError (..),
    LLMGateway (gwImageMediaTypes, gwImageUrlSupport, gwName),
    PartBody (..),
    LLMHooks (..),
    ThinkingMode (..),
    Turn (..),
    conversationHasImages,
    validateConversation,
    validateTurns,
  )
import LLM.Core.Usage (Usage (..), estimateCost)
import LLM.Core.Utils (interruptOnAbort, withRetry, withTimeout)
import LLM.Generate.Logger (Hooks (..), LogLevel (..), safeHooks, safeLLMHooks)
import LLM.Generate.ModelConfig
  ( ModelCapabilities (..),
    ModelConfig (..),
    ModelWithFallbacks (..),
    mfwToModelConfigs,
    modelRetryPolicy,
  )
import LLM.Generate.Types (GenRequest (..), GenerateError (..), GenerateResult)

maybeThrottle :: Maybe Int -> IO a -> IO a
maybeThrottle Nothing io = io
maybeThrottle (Just ms) io = threadDelay (ms * 1000) >> io

mkRequest :: GenRequest -> ModelConfig -> ChatRequest
mkRequest gr mc =
  ChatRequest
    { reqModel = mc.mcModel,
      reqConversation = gr.grMessages,
      reqSystem = gr.grSystemPrompt,
      reqMaxTokens = mc.mcMaxTokens,
      reqTemperature = mc.mcTemperature,
      reqTools = gr.grTools,
      reqThinking = mc.mcThinking
    }

usageWithModelCost :: ModelConfig -> Usage -> Usage
usageWithModelCost mc u = u {usageTotalCost = estimateCost mc.mcPricing u}

-- | Check that a candidate model declares every capability the request needs.
--
-- Unsupported vision is a candidate failure (do not drop images), and so is
-- an inline image whose media type the gateway does not accept
-- ('gwImageMediaTypes') or a URL image on a gateway without
-- 'gwImageUrlSupport'. URL images have no known media type, so their type
-- is not checked.
-- Thinking configuration requires a declared thinking capability. Cache
-- hints are not validated here; unsupported providers ignore them.
validateModelCapabilities :: ModelConfig -> [Turn] -> Either LLMError ()
validateModelCapabilities mc turns = do
  whenNeedsVision
  whenNeedsMediaTypes
  whenNeedsImageUrls
  whenNeedsThinking
  where
    caps = mc.mcCapabilities
    whenNeedsVision
      | conversationHasImages turns,
        not caps.capVision =
          Left $
            UnsupportedCapability $
              "model "
                <> mc.mcModel
                <> " does not support vision"
      | otherwise = Right ()
    whenNeedsMediaTypes =
      case filter (`notElem` mc.mcGateway.gwImageMediaTypes) (inlineImageMediaTypes turns) of
        [] -> Right ()
        unsupported ->
          Left $
            UnsupportedCapability $
              "model "
                <> mc.mcModel
                <> " via "
                <> mc.mcGateway.gwName
                <> " does not accept image media type "
                <> T.intercalate ", " unsupported
    whenNeedsImageUrls
      | conversationHasImageUrls turns,
        not mc.mcGateway.gwImageUrlSupport =
          Left $
            UnsupportedCapability $
              "model "
                <> mc.mcModel
                <> " via "
                <> mc.mcGateway.gwName
                <> " does not accept image URLs"
      | otherwise = Right ()
    whenNeedsThinking =
      case mc.mcThinking of
        Just ThinkingMode {tmEnabled = True}
          | not caps.capThinking ->
              Left $
                UnsupportedCapability $
                  "model "
                    <> mc.mcModel
                    <> " does not support thinking"
        _ -> Right ()

-- | Distinct media types of the inline ('ImageBase64') images in a conversation.
inlineImageMediaTypes :: [Turn] -> [Text]
inlineImageMediaTypes turns =
  nub [mt | ImageBase64 mt _ <- imageSources turns]

conversationHasImageUrls :: [Turn] -> Bool
conversationHasImageUrls turns =
  not (null [() | ImageUrl _ <- imageSources turns])

imageSources :: [Turn] -> [ImageSource]
imageSources turns =
  [src | parts <- map turnParts turns, ContentPart (ImagePart src) _ <- parts]
  where
    turnParts (UserMessage parts) = parts
    turnParts (AssistantMessage parts) = parts
    turnParts (ToolTurn _) = []

-- | Run one model call with the model's throttle delay and retry policy.
-- @attempt@ is run once per try; any per-attempt timeout must be inside it.
--
-- 'mcThrottleDelay' is slept before every try, retries included. Before a
-- retry it comes after the retry backoff, so the two delays add up.
--
-- When 'grAbortSignal' fires, the call is interrupted wherever it is (in an
-- attempt, or in the throttle or backoff sleep) and returns 'Aborted'. An
-- interrupted attempt is cancelled, which closes its HTTP connection.
callWithRetry ::
  GenRequest ->
  ModelConfig ->
  IO (Either LLMError a) ->
  IO (Either LLMError a)
callWithRetry gr mc attempt =
  fmap (fromMaybe (Left Aborted)) $
    interruptOnAbort gr.grAbortSignal $
      withRetry
        (modelRetryPolicy mc)
        (gr.grHooks.onLog Warn)
        (maybeThrottle mc.mcThrottleDelay (unlessAborted gr attempt))

-- | Run @act@ unless 'grAbortSignal' has fired, else return 'Aborted'
-- (which is not retried). Checked before every attempt, so also after
-- throttle and backoff sleeps.
unlessAborted :: GenRequest -> IO (Either LLMError a) -> IO (Either LLMError a)
unlessAborted gr act = do
  aborted <- isAbortedMaybe gr.grAbortSignal
  if aborted then pure (Left Aborted) else act

-- | 'callWithRetry' where each attempt is limited by 'mcRequestTimeout'.
--
-- The timeout applies to each attempt separately, not to the whole
-- sequence of retries and backoff sleeps. A timed-out attempt yields
-- 'TimeoutError', which is retryable. For streaming, see
-- 'LLM.Core.Utils.withFirstEventTimeout' (used by 'streamTextLLM').
callWithRetryTimeout ::
  GenRequest ->
  ModelConfig ->
  IO (Either LLMError a) ->
  IO (Either LLMError a)
callWithRetryTimeout gr mc invoke =
  callWithRetry gr mc (withTimeout mc.mcRequestTimeout invoke)

withModelFallbacks ::
  GenRequest ->
  ModelWithFallbacks ->
  (ModelConfig -> IO (Either LLMError a)) ->
  IO (GenerateResult a)
withModelFallbacks gr models invokePerModel =
  fmap (\(_, _, a) -> a) <$> runModelFallbacks CheckTurns gr models invokePerModel

-- | Like 'withModelFallbacks', but also returns the chain rotated so that the
-- model that answered comes first. The models before it (the ones that
-- failed) move to the end, so they stay a last resort:
-- @xs ++ [m] ++ ys@ becomes @ModelWithFallbacks m (ys ++ xs)@.
--
-- Used to keep an agent run on the model that answered. Logs at 'Info' when
-- the chain changes.
withModelFallbacksRotated ::
  GenRequest ->
  ModelWithFallbacks ->
  (ModelConfig -> IO (Either LLMError a)) ->
  IO (GenerateResult (ModelWithFallbacks, a))
withModelFallbacksRotated = withModelFallbacksRotatedWith CheckTurns

-- | Whether the fallback loop checks the request's messages first.
data TurnCheck
  = -- | Log the problems of @grMessages@ before the first model
    -- ('logInvalidTurns'). What every public entry point does.
    CheckTurns
  | -- | Do not check. For the agent loop, which checks the caller's turns
    -- once per run; the turns it adds come from providers.
    SkipTurnCheck

-- | 'withModelFallbacksRotated' with the choice of 'TurnCheck'.
withModelFallbacksRotatedWith ::
  TurnCheck ->
  GenRequest ->
  ModelWithFallbacks ->
  (ModelConfig -> IO (Either LLMError a)) ->
  IO (GenerateResult (ModelWithFallbacks, a))
withModelFallbacksRotatedWith check gr models invokePerModel = do
  r <- runModelFallbacks check gr models invokePerModel
  case r of
    Left err -> pure $ Left err
    Right (rotated, chain, a) -> do
      when rotated $
        gr.grHooks.onLog Info (formatContinuingOn chain.mwfModel)
      pure $ Right (chain, a)

-- | Shared loop of 'withModelFallbacks' and 'withModelFallbacksRotated'.
-- On success, returns whether the chain was rotated, the rotated chain and
-- the result.
--
-- With 'CheckTurns', 'logInvalidTurns' runs before the first model is
-- tried. The request is sent unchanged.
runModelFallbacks ::
  TurnCheck ->
  GenRequest ->
  ModelWithFallbacks ->
  (ModelConfig -> IO (Either LLMError a)) ->
  IO (GenerateResult (Bool, ModelWithFallbacks, a))
runModelFallbacks check gr models invokePerModel = do
  case check of
    CheckTurns -> logInvalidTurns gr.grHooks gr.grMessages
    SkipTurnCheck -> pure ()
  case mfwToModelConfigs models of
    [] -> pure $ Left GErrAllModelsFailed
    modelConfigs -> loop [] modelConfigs Nothing
  where
    loop _ [] mLast =
      pure $
        Left $
          maybe GErrAllModelsFailed GErrLLM mLast
    loop failed (mc : rest) mLast = do
      aborted <- isAbortedMaybe gr.grAbortSignal
      if aborted then pure (Left GErrAborted) else tryModel failed mc rest mLast
    tryModel failed mc rest _ = do
      gr.grHooks.onLog Info (formatTryingModel mc)
      case validateModelCapabilities mc gr.grMessages of
        Left err ->
          case rest of
            [] -> pure $ Left (GErrLLM err)
            _ -> do
              gr.grHooks.onLog Warn (formatModelFallback mc err)
              loop (failed ++ [mc]) rest (Just err)
        Right () -> do
          r <- invokePerModel mc
          case r of
            Left Aborted -> pure $ Left GErrAborted
            Left err ->
              case rest of
                [] -> pure $ Left (GErrLLM err)
                _ -> do
                  gr.grHooks.onLog Warn (formatModelFallback mc err)
                  loop (failed ++ [mc]) rest (Just err)
            Right a -> pure $ Right (not (null failed), ModelWithFallbacks mc (rest ++ failed), a)

-- | Log each invalid turn ('validateTurns') and each problem in the
-- tool-call structure ('validateConversation') at 'Warn' as
-- @turn <n>: <reason>@ (@n@ counts from 0), in turn order.
logInvalidTurns :: Hooks -> [Turn] -> IO ()
logInvalidTurns hooks turns =
  mapM_
    (\(i, reason) -> hooks.onLog Warn ("turn " <> T.pack (show i) <> ": " <> reason))
    (sortOn fst (validateTurns turns ++ validateConversation turns))

formatTryingModel :: ModelConfig -> Text
formatTryingModel mc =
  "Trying model: "
    <> mc.mcModel
    <> " via "
    <> mc.mcGateway.gwName

formatContinuingOn :: ModelConfig -> Text
formatContinuingOn mc =
  "Continuing run on "
    <> mc.mcModel
    <> " via "
    <> mc.mcGateway.gwName

formatModelFallback :: ModelConfig -> LLMError -> Text
formatModelFallback mc err =
  "Falling back from "
    <> mc.mcModel
    <> ": "
    <> T.pack (show err)

llmHooks :: Hooks -> LLMHooks
llmHooks hooks =
  LLMHooks
    { onLLMRequest = hooks.onRequest,
      onLLMResponse = hooks.onResponse,
      onLLMResponseError = hooks.onResponseError
    }

-- | The request with its hooks wrapped by 'safeHooks' and 'safeLLMHooks',
-- so an exception thrown by a hook cannot fail a call whose side effects
-- already happened. Applied by every public generation entry point.
safeGenRequest :: GenRequest -> GenRequest
safeGenRequest gr =
  gr
    { grHooks = safeHooks gr.grHooks,
      grLLMHooks = safeLLMHooks gr.grLLMHooks
    }
