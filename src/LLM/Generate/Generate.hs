module LLM.Generate.Generate
  ( streamText,
    generateText,
    mkToolContext,
    usageWithModelCost,
    generateTextLLM,
    streamTextLLM,
    generateTextWithFallbacks,
    streamTextWithFallbacks,
  )
where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import LLM.Core.Abort (isAbortedMaybe)
import LLM.Core.Types
  ( ChatResponse (..),
    LLMGateway (..),
    LLMTextResult,
    StreamEvent (..),
    Turn (..),
  )
import LLM.Core.Usage (Usage (..), emptyUsage)
import LLM.Core.Utils (getToolCalls)
import LLM.Generate.Events (emitEvent)
import LLM.Generate.GenerateUtils
  ( callWithRetryTimeout,
    emitGenerationStart,
    mkRequest,
    usageWithModelCost,
    withModelFallbacks,
  )
import LLM.Generate.ModelConfig
  ( ModelConfig (..),
    ModelWithFallbacks (..),
  )
import LLM.Generate.ToolUtils
  ( executeToolsWithAbort,
    windowOffset,
  )
import LLM.Generate.Types
  ( Agent (..),
    GenerateError (..),
    GenerateErrorResult (..),
    GenerateEventDetail (..),
    GenerateResult (..),
    RuntimeArgs (..),
    StreamChunk (..),
    ToolContext (..),
  )

data StreamChannel
  = AnswerChannel
  | PreambleChannel

-- | Run the agent loop until a final or failed result.
generateText ::
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateResult)
generateText = agentLoop generateTextWithFallbacks

streamText ::
  (StreamChunk -> IO ()) ->
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateResult)
streamText onChunk = agentLoop (streamTextWithFallbacks onChunk)

agentLoop ::
  (Agent -> ModelWithFallbacks -> RuntimeArgs -> [Turn] -> IO (Either GenerateError ChatResponse)) ->
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateResult)
agentLoop call agent models rt initialTurns = do
  emitGenerationStart rt initialTurns
  go initialTurns [] emptyUsage 0
  where
    go :: [Turn] -> [Turn] -> Usage -> Int -> IO (Either GenerateErrorResult GenerateResult)
    go currentTurns newTurnsAcc currentUsage loopCount = do
      aborted <- isAbortedMaybe (rtAbortSignal rt)
      if aborted
        then do
          let errResult = GenerateErrorResult GErrAborted (rtGenerationId rt) newTurnsAcc currentUsage
          emitEvent rt (GenerationFailed GErrAborted errResult)
          pure $ Left errResult
        else
          if loopCount >= agMaxToolRounds agent
            then do
              let errResult = GenerateErrorResult GErrToolExceeded (rtGenerationId rt) newTurnsAcc currentUsage
              emitEvent rt (GenerationFailed GErrToolExceeded errResult)
              pure $ Left errResult
            else do
              result <- call agent models rt currentTurns
              case result of
                Left err -> do
                  let errResult = GenerateErrorResult err (rtGenerationId rt) newTurnsAcc currentUsage
                  emitEvent rt (GenerationFailed err errResult)
                  pure $ Left errResult
                Right resp -> do
                  let txt = respText resp
                      toolCalls = getToolCalls resp
                      roundUsage = fromMaybe emptyUsage (respUsage resp)
                      newUsage = currentUsage <> roundUsage

                  case toolCalls of
                    [] -> do
                      let finalTurn = AssistantTurn txt []
                          finalTurnsAcc = newTurnsAcc ++ [finalTurn]
                          successResult = GenerateResult (rtGenerationId rt) finalTurnsAcc txt newUsage
                      emitEvent rt (MessageFinalized finalTurn)
                      emitEvent rt (GenerationFinished successResult)
                      pure $ Right successResult
                    _ -> do
                      let assistantTurn = AssistantTurn txt toolCalls
                          toolContext = mkToolContext agent (currentTurns ++ [assistantTurn]) newUsage rt

                      emitEvent rt (MessageCreated assistantTurn)
                      emitEvent rt (ToolRoundStarted loopCount)

                      toolResultsE <- executeToolsWithAbort (rtAbortSignal rt) (rtHooks rt) toolContext (agTools agent) toolCalls

                      case toolResultsE of
                        Left err -> do
                          let errResult = GenerateErrorResult err (rtGenerationId rt) (newTurnsAcc ++ [assistantTurn]) newUsage
                          emitEvent rt (GenerationFailed err errResult)
                          pure $ Left errResult
                        Right toolResults -> do
                          let toolTurn = ToolTurn toolResults
                          emitEvent rt (MessageCreated toolTurn)
                          emitEvent rt (ToolRoundFinished loopCount)
                          let turnsToAdd = [assistantTurn, toolTurn]
                          go (currentTurns ++ turnsToAdd) (newTurnsAcc ++ turnsToAdd) newUsage (loopCount + 1)

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

generateTextWithFallbacks ::
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateError ChatResponse)
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
  IO (Either GenerateError ChatResponse)
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
