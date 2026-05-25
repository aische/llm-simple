module LLM.Generate.GenerateLoop
  ( streamText,
    generateText,
    usageWithModelCost,
  )
where

import Data.Maybe (fromMaybe)
import LLM.Core.Abort (isAbortedMaybe)
import LLM.Core.Types
  ( ChatResponse (..),
    Turn (..),
  )
import LLM.Core.Usage (Usage (..), emptyUsage)
import LLM.Core.Utils (getToolCalls)
import LLM.Generate.Events (emitEvent)
import LLM.Generate.Generate
  ( generateTextWithFallbacks,
    mkToolContext,
    streamTextWithFallbacks,
  )
import LLM.Generate.GenerateUtils
  ( emitGenerationStart,
    usageWithModelCost,
  )
import LLM.Generate.ModelConfig
  ( ModelWithFallbacks (..),
  )
import LLM.Generate.ToolUtils
  ( executeToolsWithAbort,
  )
import LLM.Generate.Types
  ( Agent (..),
    GenerateError (..),
    GenerateErrorResult (..),
    GenerateEventDetail (..),
    GenerateResult,
    GenerateTextResult (..),
    RuntimeArgs (..),
    StreamChunk (..),
  )

-- | Run the agent loop until a final or failed result.
generateText ::
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
generateText = agentLoop generateTextWithFallbacks

streamText ::
  (StreamChunk -> IO ()) ->
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
streamText onChunk = agentLoop (streamTextWithFallbacks onChunk)

agentLoop ::
  (Agent -> ModelWithFallbacks -> RuntimeArgs -> [Turn] -> IO (GenerateResult ChatResponse)) ->
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
agentLoop call agent models rt initialTurns = do
  emitGenerationStart rt initialTurns
  go initialTurns [] emptyUsage 0
  where
    go :: [Turn] -> [Turn] -> Usage -> Int -> IO (Either GenerateErrorResult GenerateTextResult)
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
                          successResult = GenerateTextResult (rtGenerationId rt) finalTurnsAcc txt newUsage
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
