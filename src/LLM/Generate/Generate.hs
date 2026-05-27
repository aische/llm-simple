module LLM.Generate.Generate
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
  )
import LLM.Core.Usage (emptyUsage)
import LLM.Generate.GenerateUtils
  ( callWithRetryTimeout,
    mkRequest,
    usageWithModelCost,
    withModelFallbacks,
  )
import LLM.Generate.ModelConfig
  ( ModelConfig (..),
    ModelWithFallbacks (..),
  )
import LLM.Generate.Types
  ( GenRequest (..),
    GenerateResult,
    StreamChunk (..),
  )

generateTextWithFallbacks ::
  GenRequest ->
  ModelWithFallbacks ->
  IO (GenerateResult ChatResponse)
generateTextWithFallbacks gr models =
  withModelFallbacks gr models $ \mc -> do
    r <- generateTextLLM gr mc
    case r of
      Left err -> pure $ Left err
      Right resp ->
        let usage = usageWithModelCost mc (fromMaybe emptyUsage resp.respUsage)
         in pure $ Right resp {respUsage = Just usage}

streamTextWithFallbacks ::
  (StreamChunk -> IO ()) ->
  GenRequest ->
  ModelWithFallbacks ->
  IO (GenerateResult ChatResponse)
streamTextWithFallbacks onChunk gr models =
  withModelFallbacks gr models $ \mc -> do
    r <- streamTextLLM onChunk gr mc
    case r of
      Left err -> pure $ Left err
      Right resp ->
        let usage = usageWithModelCost mc (fromMaybe emptyUsage resp.respUsage)
         in pure $ Right resp {respUsage = Just usage}

generateTextLLM :: GenRequest -> ModelConfig -> IO LLMTextResult
generateTextLLM gr mc =
  callWithRetryTimeout gr mc $
    let request = mkRequest gr mc
     in mc.mcGateway.gwGenerateText gr.grLLMHooks request

streamTextLLM :: (StreamChunk -> IO ()) -> GenRequest -> ModelConfig -> IO LLMTextResult
streamTextLLM onChunk gr mc =
  callWithRetryTimeout gr mc $
    let request = mkRequest gr mc
     in do
          onProviderEvent <- mkProviderStreamCallback gr onChunk
          mc.mcGateway.gwStreamText gr.grLLMHooks request onProviderEvent

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
    StreamReasoningDelta txt -> onChunk (ReasoningDelta txt)
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
