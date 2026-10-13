{- HLINT ignore "Eta reduce" -}
module LLM.Agent.Generate
  ( streamText,
    generateText,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
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
  ( generateTextWithFallbacksRotated,
    streamTextWithFallbacksRotated,
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

-- | Run the agent loop until the model returns final text or the run fails.
--
-- On success, 'gtrNewMessages' contains only the turns produced during this
-- call (assistant and tool turns), not the input conversation.
-- Tool rounds continue until the model responds without tool calls or
-- 'Agent.agMaxToolRounds' is exceeded.
generateText ::
  Agent ->
  ModelWithFallbacks ->
  ToolMap Text ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
generateText agent models toolMap rt initialTurns =
  agentLoop
    (\a m r t -> generateTextWithFallbacksRotated (createGenRequest id a toolMap r t) m)
    agent
    models
    toolMap
    rt
    initialTurns

-- | Like 'generateText', but streams token deltas via the callback.
--
-- The callback receives 'StreamChunk' values as the provider responds.
-- Tool rounds still run to completion; streaming applies to each model call.
-- Returns the same 'GenerateTextResult' / 'GenerateErrorResult' as 'generateText'.
streamText ::
  (StreamChunk -> IO ()) ->
  Agent ->
  ModelWithFallbacks ->
  ToolMap Text ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
streamText onChunk agent models toolMap rt initialTurns =
  agentLoop
    (\a m r t -> streamTextWithFallbacksRotated onChunk (createGenRequest id a toolMap r t) m)
    agent
    models
    toolMap
    rt
    initialTurns

-- | The tool loop. @call@ returns the model chain rotated so the model that
-- answered comes first; the next round starts with that chain, so a run that
-- fell back stays on the fallback instead of retrying a failing primary every
-- round. The next 'generateText' / 'streamText' call starts from the caller's
-- chain again.
agentLoop ::
  (Agent -> ModelWithFallbacks -> RuntimeArgs -> [Turn] -> IO (GenerateResult (ModelWithFallbacks, ChatResponse))) ->
  Agent ->
  ModelWithFallbacks ->
  ToolMap Text ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
agentLoop call agent models toolMap rt initialTurns = do
  emitEvent rt GenerationStarted
  go models initialTurns [] emptyUsage 0
  where
    go :: ModelWithFallbacks -> [Turn] -> [Turn] -> Usage -> Int -> IO (Either GenerateErrorResult GenerateTextResult)
    go chain currentTurns newTurnsAcc currentUsage loopCount = do
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
              result <- call agent chain rt currentTurns
              case result of
                Left err -> do
                  let errResult = GenerateErrorResult err newTurnsAcc currentUsage
                  emitEvent rt (GenerationFailed err errResult)
                  pure $ Left errResult
                Right (nextChain, resp) -> do
                  let txt = resp.respText
                      toolCalls = getToolCalls resp
                      roundUsage = fromMaybe emptyUsage resp.respUsage
                      newUsage = currentUsage <> roundUsage
                      -- Authoritative ordered parts from the provider response.
                      assistantMsg = AssistantMessage resp.respContent

                  case toolCalls of
                    [] -> do
                      let finalTurnsAcc = newTurnsAcc ++ [assistantMsg]
                          successResult = GenerateTextResult rt.rtGenerationId finalTurnsAcc txt newUsage
                      emitEvent rt (MessageFinalized assistantMsg)
                      emitEvent rt (GenerationFinished successResult)
                      pure $ Right successResult
                    _ -> do
                      let toolContext = createToolContext agent currentTurns newUsage rt
                          tools = getResolvedTools id agent toolMap rt
                      emitEvent rt (MessageCreated assistantMsg)
                      emitEvent rt (ToolRoundStarted loopCount)

                      toolResultsE <- executeToolsWithAbort rt.rtAbortSignal rt.rtHooks toolContext tools toolCalls

                      case toolResultsE of
                        Left err -> do
                          let errResult = GenerateErrorResult err (newTurnsAcc ++ [assistantMsg]) newUsage
                          emitEvent rt (GenerationFailed err errResult)
                          pure $ Left errResult
                        Right toolResults -> do
                          let toolTurn = ToolTurn toolResults
                          emitEvent rt (MessageCreated toolTurn)
                          emitEvent rt (ToolRoundFinished loopCount)
                          let turnsToAdd = [assistantMsg, toolTurn]
                          go nextChain (currentTurns ++ turnsToAdd) (newTurnsAcc ++ turnsToAdd) newUsage (loopCount + 1)
