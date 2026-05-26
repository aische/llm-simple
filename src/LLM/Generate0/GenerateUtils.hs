module LLM.Generate0.GenerateUtils
  ( maybeThrottle,
    mkRequest,
    usageWithModelCost,
    callWithRetryTimeout,
    withModelFallbacks,
  )
where

import Control.Concurrent (threadDelay)

import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Types (ChatRequest (..), LLMError (..), LLMGateway (gwName), Turn (..))
import LLM.Core.Usage (Usage (..), estimateCost)
import LLM.Core.Utils (withRetry, withTimeout)
import LLM.Generate0.Logger (Hooks (..), LogLevel (..), onLog)
import LLM.Generate0.ModelConfig
  ( ModelConfig (..),
    ModelWithFallbacks (..),
    mfwToModelConfigs,
    modelRetryPolicy,
  )
import LLM.Generate0.Types (GenRequest (..), GenerateError (..), GenerateResult)

maybeThrottle :: Maybe Int -> IO a -> IO a
maybeThrottle Nothing io = io
maybeThrottle (Just ms) io = threadDelay (ms * 1000) >> io

mkRequest :: GenRequest -> ModelConfig -> [Turn] -> ChatRequest
mkRequest gr mc conv =
  ChatRequest
    { reqModel = mcModel mc,
      reqConversation = conv,
      reqSystem = grSystemPrompt gr,
      reqMaxTokens = mcMaxTokens mc,
      reqTemperature = mcTemperature mc,
      reqTools = grTools gr
    }

usageWithModelCost :: ModelConfig -> Usage -> Usage
usageWithModelCost mc u = u {usageTotalCost = estimateCost (mcPricing mc) u}

callWithRetryTimeout ::
  GenRequest ->
  ModelConfig ->
  IO (Either LLMError a) ->
  IO (Either LLMError a)
callWithRetryTimeout gr mc invoke =
  maybeThrottle (mcThrottleDelay mc) $
    withTimeout (mcRequestTimeout mc) $
      withRetry (modelRetryPolicy mc) (onLog (grHooks gr) Warn) invoke

withModelFallbacks ::
  GenRequest ->
  ModelWithFallbacks ->
  (ModelConfig -> IO (Either LLMError a)) ->
  IO (GenerateResult a)
withModelFallbacks gr models invokePerModel =
  case mfwToModelConfigs models of
    [] -> pure $ Left GErrAllModelsFailed
    modelConfigs -> loop modelConfigs Nothing
  where
    loop [] mLast =
      pure $
        Left $
          maybe GErrAllModelsFailed GErrLLM mLast
    loop (mc : rest) _ = do
      onLog (grHooks gr) Info (formatTryingModel mc)
      r <- invokePerModel mc
      case r of
        Left Aborted -> pure $ Left GErrAborted
        Left err ->
          case rest of
            [] -> pure $ Left (GErrLLM err)
            _ -> do
              onLog (grHooks gr) Warn (formatModelFallback mc err)
              loop rest (Just err)
        Right a -> pure $ Right a

formatTryingModel :: ModelConfig -> Text
formatTryingModel mc =
  "Trying model: "
    <> mcModel mc
    <> " via "
    <> gwName (mcGateway mc)

formatModelFallback :: ModelConfig -> LLMError -> Text
formatModelFallback mc err =
  "Falling back from "
    <> mcModel mc
    <> ": "
    <> T.pack (show err)
