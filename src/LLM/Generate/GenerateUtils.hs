module LLM.Generate.GenerateUtils
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
import LLM.Core.Types (ChatRequest (..), LLMError (..), LLMGateway (gwName))
import LLM.Core.Usage (Usage (..), estimateCost)
import LLM.Core.Utils (withRetry, withTimeout)
import LLM.Generate.Logger (Hooks (..), LogLevel (..), onLog)
import LLM.Generate.ModelConfig
  ( ModelConfig (..),
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

callWithRetryTimeout ::
  GenRequest ->
  ModelConfig ->
  IO (Either LLMError a) ->
  IO (Either LLMError a)
callWithRetryTimeout gr mc invoke =
  maybeThrottle mc.mcThrottleDelay $
    withTimeout mc.mcRequestTimeout $
      withRetry (modelRetryPolicy mc) (gr.grHooks.onLog Warn) invoke

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
      gr.grHooks.onLog Info (formatTryingModel mc)
      r <- invokePerModel mc
      case r of
        Left Aborted -> pure $ Left GErrAborted
        Left err ->
          case rest of
            [] -> pure $ Left (GErrLLM err)
            _ -> do
              gr.grHooks.onLog Warn (formatModelFallback mc err)
              loop rest (Just err)
        Right a -> pure $ Right a

formatTryingModel :: ModelConfig -> Text
formatTryingModel mc =
  "Trying model: "
    <> mc.mcModel
    <> " via "
    <> mc.mcGateway.gwName

formatModelFallback :: ModelConfig -> LLMError -> Text
formatModelFallback mc err =
  "Falling back from "
    <> mc.mcModel
    <> ": "
    <> T.pack (show err)
