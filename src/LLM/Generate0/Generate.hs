module LLM.Generate0.Generate
  ( usageWithModelCost,
    generateTextLLM,
    streamTextLLM,
    generateTextWithFallbacks,
    streamTextWithFallbacks,
  )
where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import LLM.Core.Types
  ( ChatResponse (..),
    LLMGateway (..),
    LLMTextResult,
    StreamEvent (..),
    Turn (..),
  )
import LLM.Core.Usage (emptyUsage)
import LLM.Generate0.GenerateUtils
  ( callWithRetryTimeout,
    mkRequest,
    usageWithModelCost,
    withModelFallbacks,
  )
import LLM.Generate0.ModelConfig
  ( ModelConfig (..),
    ModelWithFallbacks (..),
  )
import LLM.Generate0.Types
  ( GenRequest (..),
    GenerateResult,
    StreamChunk (..),
  )

generateTextWithFallbacks ::
  GenRequest ->
  ModelWithFallbacks ->
  [Turn] ->
  IO (GenerateResult ChatResponse)
generateTextWithFallbacks gr models turns =
  withModelFallbacks gr models $ \mc -> do
    r <- generateTextLLM gr mc turns
    case r of
      Left err -> pure $ Left err
      Right resp ->
        let usage = usageWithModelCost mc (fromMaybe emptyUsage (respUsage resp))
         in pure $ Right resp {respUsage = Just usage}

streamTextWithFallbacks ::
  (StreamChunk -> IO ()) ->
  GenRequest ->
  ModelWithFallbacks ->
  [Turn] ->
  IO (GenerateResult ChatResponse)
streamTextWithFallbacks onChunk gr models turns =
  withModelFallbacks gr models $ \mc -> do
    r <- streamTextLLM onChunk gr mc turns
    case r of
      Left err -> pure $ Left err
      Right resp ->
        let usage = usageWithModelCost mc (fromMaybe emptyUsage (respUsage resp))
         in pure $ Right resp {respUsage = Just usage}

generateTextLLM :: GenRequest -> ModelConfig -> [Turn] -> IO LLMTextResult
generateTextLLM gr mc turns =
  callWithRetryTimeout gr mc $
    let request = mkRequest gr mc turns
     in gwGenerateText (mcGateway mc) (grLLMHooks gr) request

streamTextLLM :: (StreamChunk -> IO ()) -> GenRequest -> ModelConfig -> [Turn] -> IO LLMTextResult
streamTextLLM onChunk gr mc turns =
  callWithRetryTimeout gr mc $
    let request = mkRequest gr mc turns
     in do
          onProviderEvent <- mkProviderStreamCallback gr onChunk
          gwStreamText (mcGateway mc) (grLLMHooks gr) request onProviderEvent

data StreamChannel
  = AnswerChannel
  | PreambleChannel

mkProviderStreamCallback ::
  GenRequest ->
  (StreamChunk -> IO ()) ->
  IO (StreamEvent -> IO ())
mkProviderStreamCallback _gr onChunk = do
  -- TODO: this is not a good implementation
  channelRef <- newIORef AnswerChannel
  pure $ \case
    StreamDelta txt -> do
      channel <- readIORef channelRef
      case channel of
        AnswerChannel -> do
          let chunk = AnswerDelta txt
          onChunk chunk
        PreambleChannel -> onChunk (PreambleDelta txt)
    StreamToolCall tc -> do
      writeIORef channelRef PreambleChannel
      onChunk (StreamToolCallChunk tc)
