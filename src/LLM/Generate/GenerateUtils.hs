module LLM.Generate.GenerateUtils
  ( maybeThrottle,
    mkRequest,
    usageWithModelCost,
    callWithRetry,
    callWithRetryTimeout,
    withModelFallbacks,
    withModelFallbacksRotated,
    validateModelCapabilities,
    llmHooks,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.List (nub)
import LLM.Core.Types
  ( ChatRequest (..),
    ContentPart (..),
    ImageSource (..),
    LLMError (..),
    LLMGateway (gwImageMediaTypes, gwName),
    PartBody (..),
    LLMHooks (..),
    ThinkingMode (..),
    Turn (..),
    conversationHasImages,
  )
import LLM.Core.Usage (Usage (..), estimateCost)
import LLM.Core.Utils (withRetry, withTimeout)
import LLM.Generate.Logger (Hooks (..), LogLevel (..))
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
-- ('gwImageMediaTypes'). URL images have no known type and are not checked.
-- Thinking configuration requires a declared thinking capability. Cache
-- hints are not validated here; unsupported providers ignore them.
validateModelCapabilities :: ModelConfig -> [Turn] -> Either LLMError ()
validateModelCapabilities mc turns = do
  whenNeedsVision
  whenNeedsMediaTypes
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
  nub [mt | parts <- map turnParts turns, ContentPart (ImagePart (ImageBase64 mt _)) _ <- parts]
  where
    turnParts (UserMessage parts) = parts
    turnParts (AssistantMessage parts) = parts
    turnParts (ToolTurn _) = []

-- | Run one model call with the model's throttle delay and retry policy.
-- @attempt@ is run once per try; any per-attempt timeout must be inside it.
callWithRetry ::
  GenRequest ->
  ModelConfig ->
  IO (Either LLMError a) ->
  IO (Either LLMError a)
callWithRetry gr mc attempt =
  maybeThrottle mc.mcThrottleDelay $
    withRetry (modelRetryPolicy mc) (gr.grHooks.onLog Warn) attempt

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
  fmap (\(_, _, a) -> a) <$> runModelFallbacks gr models invokePerModel

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
withModelFallbacksRotated gr models invokePerModel = do
  r <- runModelFallbacks gr models invokePerModel
  case r of
    Left err -> pure $ Left err
    Right (rotated, chain, a) -> do
      when rotated $
        gr.grHooks.onLog Info (formatContinuingOn chain.mwfModel)
      pure $ Right (chain, a)

-- | Shared loop of 'withModelFallbacks' and 'withModelFallbacksRotated'.
-- On success, returns whether the chain was rotated, the rotated chain and
-- the result.
runModelFallbacks ::
  GenRequest ->
  ModelWithFallbacks ->
  (ModelConfig -> IO (Either LLMError a)) ->
  IO (GenerateResult (Bool, ModelWithFallbacks, a))
runModelFallbacks gr models invokePerModel =
  case mfwToModelConfigs models of
    [] -> pure $ Left GErrAllModelsFailed
    modelConfigs -> loop [] modelConfigs Nothing
  where
    loop _ [] mLast =
      pure $
        Left $
          maybe GErrAllModelsFailed GErrLLM mLast
    loop failed (mc : rest) _ = do
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
