{- HLINT ignore "Eta reduce" -}
module LLM.Agent.Generate
  ( streamText,
    generateText,
  )
where

import Data.Maybe (fromMaybe)
import LLM.Agent.Events (emitEvent)
import LLM.Agent.ToolUtils
  ( createGenRequest,
    createToolContext,
    executeToolsWithAbort,
    getResolvedTools,
  )
import LLM.Agent.Types
  ( Agent (agMaxToolRounds),
    GenerateEventDetail (..),
    RuntimeArgs (..),
  )
import LLM.Core.Abort (isAbortedMaybe)
import LLM.Core.Types
  ( ChatResponse (..),
    Turn (..),
  )
import LLM.Core.Usage (Usage (..), emptyUsage)
import LLM.Core.Utils (getToolCalls)
import LLM.Generate.Generate
  ( generateTextWithFallbacks,
    streamTextWithFallbacks,
  )
import LLM.Generate.ModelConfig
  ( ModelWithFallbacks (..),
  )
import LLM.Generate.Types
  ( GenerateError (..),
    GenerateErrorResult (..),
    GenerateResult,
    GenerateTextResult (..),
    StreamChunk (..),
  )

-- | Run the agent loop until a final or failed result.
generateText ::
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
generateText = agentLoop (\a m r t -> generateTextWithFallbacks (createGenRequest a r t) m)

streamText ::
  (StreamChunk -> IO ()) ->
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
streamText onChunk = agentLoop (\a m r t -> streamTextWithFallbacks onChunk (createGenRequest a r t) m)

agentLoop ::
  (Agent -> ModelWithFallbacks -> RuntimeArgs -> [Turn] -> IO (GenerateResult ChatResponse)) ->
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
agentLoop call agent models rt initialTurns = do
  emitEvent rt GenerationStarted
  go initialTurns [] emptyUsage 0
  where
    go :: [Turn] -> [Turn] -> Usage -> Int -> IO (Either GenerateErrorResult GenerateTextResult)
    go currentTurns newTurnsAcc currentUsage loopCount = do
      aborted <- isAbortedMaybe (rtAbortSignal rt)
      if aborted
        then do
          let errResult = GenerateErrorResult GErrAborted newTurnsAcc currentUsage
          emitEvent rt (GenerationFailed GErrAborted errResult)
          pure $ Left errResult
        else
          if loopCount >= agMaxToolRounds agent
            then do
              let errResult = GenerateErrorResult GErrToolExceeded newTurnsAcc currentUsage
              emitEvent rt (GenerationFailed GErrToolExceeded errResult)
              pure $ Left errResult
            else do
              result <- call agent models rt currentTurns
              case result of
                Left err -> do
                  let errResult = GenerateErrorResult err newTurnsAcc currentUsage
                  emitEvent rt (GenerationFailed err errResult)
                  pure $ Left errResult
                Right resp -> do
                  let txt = respText resp
                      toolCalls = getToolCalls resp
                      roundUsage = fromMaybe emptyUsage (respUsage resp)
                      newUsage = currentUsage <> roundUsage

                  case toolCalls of
                    [] -> do
                      let finalTurn = AssistantTurn txt (respReasoning resp) []
                          finalTurnsAcc = newTurnsAcc ++ [finalTurn]
                          successResult = GenerateTextResult (rtGenerationId rt) finalTurnsAcc txt newUsage
                      emitEvent rt (MessageFinalized finalTurn)
                      emitEvent rt (GenerationFinished successResult)
                      pure $ Right successResult
                    _ -> do
                      let assistantTurn = AssistantTurn txt (respReasoning resp) toolCalls
                          toolContext = createToolContext agent currentTurns newUsage rt
                          tools = getResolvedTools agent rt
                      emitEvent rt (MessageCreated assistantTurn)
                      emitEvent rt (ToolRoundStarted loopCount)

                      toolResultsE <- executeToolsWithAbort (rtAbortSignal rt) (rtHooks rt) toolContext tools toolCalls

                      case toolResultsE of
                        Left err -> do
                          let errResult = GenerateErrorResult err (newTurnsAcc ++ [assistantTurn]) newUsage
                          emitEvent rt (GenerationFailed err errResult)
                          pure $ Left errResult
                        Right toolResults -> do
                          let toolTurn = ToolTurn toolResults
                          emitEvent rt (MessageCreated toolTurn)
                          emitEvent rt (ToolRoundFinished loopCount)
                          let turnsToAdd = [assistantTurn, toolTurn]
                          go (currentTurns ++ turnsToAdd) (newTurnsAcc ++ turnsToAdd) newUsage (loopCount + 1)
