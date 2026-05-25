module LLM.Generate.Generate
  ( mkToolContext,
    usageWithModelCost,
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
import LLM.Core.Usage (Usage (..), emptyUsage)
import LLM.Generate.Events (emitEvent)
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
import LLM.Generate.ToolUtils
  ( windowOffset,
  )
import LLM.Generate.Types
  ( Agent (..),
    GenerateEventDetail (..),
    RuntimeArgs (..),
    StreamChunk (..),
    ToolContext (..), GenerateResult,
  )

generateTextWithFallbacks ::
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (GenerateResult ChatResponse)
generateTextWithFallbacks agent models rt turns =
  withModelFallbacks rt models $ \mc -> do
    r <- generateTextLLM agent mc rt turns
    case r of
      Left err -> pure $ Left err
      Right resp ->
        let usage = usageWithModelCost mc (fromMaybe emptyUsage (respUsage resp))
         in pure $ Right resp {respUsage = Just usage}

streamTextWithFallbacks ::
  (StreamChunk -> IO ()) ->
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (GenerateResult ChatResponse)
streamTextWithFallbacks onChunk agent models rt turns =
  withModelFallbacks rt models $ \mc -> do
    r <- streamTextLLM onChunk agent mc rt turns
    case r of
      Left err -> pure $ Left err
      Right resp ->
        let usage = usageWithModelCost mc (fromMaybe emptyUsage (respUsage resp))
         in pure $ Right resp {respUsage = Just usage}

generateTextLLM :: Agent -> ModelConfig -> RuntimeArgs -> [Turn] -> IO LLMTextResult
generateTextLLM agent mc rt turns =
  callWithRetryTimeout rt mc $
    let request = mkRequest agent mc turns (rtReadonly rt)
     in gwGenerateText (mcGateway mc) (rtLLMHooks rt) request

streamTextLLM :: (StreamChunk -> IO ()) -> Agent -> ModelConfig -> RuntimeArgs -> [Turn] -> IO LLMTextResult
streamTextLLM onChunk agent mc rt turns =
  callWithRetryTimeout rt mc $
    let request = mkRequest agent mc turns (rtReadonly rt)
     in do
          onProviderEvent <- mkProviderStreamCallback rt onChunk
          gwStreamText (mcGateway mc) (rtLLMHooks rt) request onProviderEvent

mkToolContext ::
  Agent ->
  [Turn] ->
  Usage ->
  RuntimeArgs ->
  ToolContext
mkToolContext agent messages roundUsage rt =
  ToolContext
    { tcConversation = messages,
      tcUsage = roundUsage,
      tcWindowOffset = windowOffset (agContextWindow agent) messages,
      tcRuntimeArgs = rt
    }

data StreamChannel
  = AnswerChannel
  | PreambleChannel

mkProviderStreamCallback ::
  RuntimeArgs ->
  (StreamChunk -> IO ()) ->
  IO (StreamEvent -> IO ())
mkProviderStreamCallback rt onChunk = do
  -- TODO: this is not a good implementation
  channelRef <- newIORef AnswerChannel
  pure $ \case
    StreamDelta txt -> do
      channel <- readIORef channelRef
      case channel of
        AnswerChannel -> do
          let chunk = AnswerDelta (rtGenerationId rt) txt
          onChunk chunk
          emitEvent rt (MessageUpdated (rtGenerationId rt) txt)
        PreambleChannel -> onChunk (PreambleDelta txt)
    StreamToolCall tc -> do
      writeIORef channelRef PreambleChannel
      onChunk (StreamToolCallChunk tc)
