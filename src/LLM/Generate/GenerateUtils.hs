module LLM.Generate.GenerateUtils
  ( maybeThrottle,
    mkRequest,
    usageWithModelCost,
    emitGenerationStart,
    callWithRetryTimeout,
    withModelFallbacks,
  )
where

import Control.Concurrent (threadDelay)
-- GenerateStepResult,
-- PendingConversation (..),

import LLM.Core.Types (ChatRequest (..), LLMError (Aborted), Turn (..))
import LLM.Core.Usage (Usage (..), estimateCost)
import LLM.Core.Utils (withRetry, withTimeout)
import LLM.Generate.Events (emitEvent)
import LLM.Generate.Log
  ( formatModelFallback,
    formatTryingModel,
  )
import LLM.Generate.Logger (LogLevel (..), onLog)
import LLM.Generate.ModelConfig
  ( ModelConfig (..),
    ModelWithFallbacks (..),
    mfwToModelConfigs,
    modelRetryPolicy,
  )
import LLM.Generate.ToolUtils (filterReadonlyTools, windowOffset)
import LLM.Generate.Types
  ( Agent (..),
    GenerateError (..),
    GenerateEventDetail (..),
    RuntimeArgs (..),
    Tool (toolDef),
  )

maybeThrottle :: Maybe Int -> IO a -> IO a
maybeThrottle Nothing io = io
maybeThrottle (Just ms) io = threadDelay (ms * 1000) >> io

mkRequest :: Agent -> ModelConfig -> [Turn] -> Bool -> ChatRequest
mkRequest agent mc conv readonly =
  ChatRequest
    { reqModel = mcModel mc,
      reqConversation = drop offset conv,
      reqSystem = agSystemPrompt agent,
      reqMaxTokens = mcMaxTokens mc,
      reqTemperature = mcTemperature mc,
      reqTools = toolDef <$> filterReadonlyTools readonly (agTools agent)
    }
  where
    offset = windowOffset (agContextWindow agent) conv

usageWithModelCost :: ModelConfig -> Usage -> Usage
usageWithModelCost mc u = u {usageTotalCost = estimateCost (mcPricing mc) u}

emitGenerationStart :: RuntimeArgs -> [Turn] -> IO ()
emitGenerationStart rt _pconv = do
  emitEvent rt GenerationStarted

callWithRetryTimeout ::
  RuntimeArgs ->
  ModelConfig ->
  IO (Either LLMError a) ->
  IO (Either LLMError a)
callWithRetryTimeout rt mc invoke =
  maybeThrottle (mcThrottleDelay mc) $
    withTimeout (mcRequestTimeout mc) $
      withRetry (modelRetryPolicy mc) (onLog (rtHooks rt) Warn) invoke

withModelFallbacks ::
  RuntimeArgs ->
  ModelWithFallbacks ->
  (ModelConfig -> IO (Either LLMError a)) ->
  IO (Either GenerateError a)
withModelFallbacks rt models invokePerModel =
  case mfwToModelConfigs models of
    [] -> pure $ Left GErrAllModelsFailed
    modelConfigs -> loop modelConfigs Nothing
  where
    loop [] mLast =
      pure $
        Left $
          maybe GErrAllModelsFailed GErrLLM mLast
    loop (mc : rest) _ = do
      onLog (rtHooks rt) Info (formatTryingModel mc)
      r <- invokePerModel mc
      case r of
        Left Aborted -> pure $ Left GErrAborted
        Left err ->
          case rest of
            [] -> pure $ Left (GErrLLM err)
            _ -> do
              onLog (rtHooks rt) Warn (formatModelFallback mc err)
              loop rest (Just err)
        Right a -> pure $ Right a
