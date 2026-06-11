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
    ToolMap,
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
  ToolMap ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
generateText agent models toolMap rt initialTurns =
  agentLoop
    (\a m r t -> generateTextWithFallbacks (createGenRequest a toolMap r t) m)
    agent
    models
    toolMap
    rt
    initialTurns

streamText ::
  (StreamChunk -> IO ()) ->
  Agent ->
  ModelWithFallbacks ->
  ToolMap ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
streamText onChunk agent models toolMap rt initialTurns =
  agentLoop
    (\a m r t -> streamTextWithFallbacks onChunk (createGenRequest a toolMap r t) m)
    agent
    models
    toolMap
    rt
    initialTurns

agentLoop ::
  (Agent -> ModelWithFallbacks -> RuntimeArgs -> [Turn] -> IO (GenerateResult ChatResponse)) ->
  Agent ->
  ModelWithFallbacks ->
  ToolMap ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
agentLoop call agent models toolMap rt initialTurns = do
  emitEvent rt GenerationStarted
  go initialTurns [] emptyUsage 0
  where
    go :: [Turn] -> [Turn] -> Usage -> Int -> IO (Either GenerateErrorResult GenerateTextResult)
    go currentTurns newTurnsAcc currentUsage loopCount = do
      aborted <- isAbortedMaybe rt.rtAbortSignal
      if aborted
        then do
          let errResult = GenerateErrorResult GErrAborted newTurnsAcc currentUsage
          emitEvent rt (GenerationFailed GErrAborted errResult)
          pure $ Left errResult
        else
          if loopCount >= agent.agMaxToolRounds
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
                  let txt = resp.respText
                      toolCalls = getToolCalls resp
                      roundUsage = fromMaybe emptyUsage resp.respUsage
                      newUsage = currentUsage <> roundUsage

                  case toolCalls of
                    [] -> do
                      let finalTurn = AssistantTurn txt resp.respReasoning []
                          finalTurnsAcc = newTurnsAcc ++ [finalTurn]
                          successResult = GenerateTextResult rt.rtGenerationId finalTurnsAcc txt newUsage
                      emitEvent rt (MessageFinalized finalTurn)
                      emitEvent rt (GenerationFinished successResult)
                      pure $ Right successResult
                    _ -> do
                      let assistantTurn = AssistantTurn txt resp.respReasoning toolCalls
                          toolContext = createToolContext agent currentTurns newUsage rt
                          tools = getResolvedTools agent toolMap rt
                      emitEvent rt (MessageCreated assistantTurn)
                      emitEvent rt (ToolRoundStarted loopCount)

                      toolResultsE <- executeToolsWithAbort rt.rtAbortSignal rt.rtHooks toolContext tools toolCalls

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
