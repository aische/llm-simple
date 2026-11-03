{- HLINT ignore "Eta reduce" -}
module LLM.Agent.Generate
  ( streamText,
    generateText,
    toolBudgetUsedUpPrompt,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import LLM.Agent.Events (emitEvent)
import LLM.Agent.ToolUtils
  ( createGenRequest,
    createToolContext,
    executeToolsWithAbort,
    ToolRoundOutcome (..),
    getResolvedTools,
    safeRuntimeArgs,
  )
import LLM.Agent.Types
  ( Agent (agMaxToolRounds, agSystemPrompt),
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
import LLM.Core.Utils (getToolCalls, toolResult)
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
-- 'Agent.agMaxToolRounds' rounds have run. Then one final model call asks
-- for a text answer ('toolBudgetUsedUpPrompt'); tool calls in that reply
-- are not run, and the run ends with 'GErrToolExceeded'.
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

-- | The line appended to the system prompt for the final model call, made
-- once 'Agent.agMaxToolRounds' tool rounds have run.
toolBudgetUsedUpPrompt :: Text
toolBudgetUsedUpPrompt =
  "The tool budget for this run is used up. Answer in text; do not call tools."

-- | Result text for a tool call in the final reply, which is not run.
toolNotRunText :: Text
toolNotRunText = "Not run: tool round limit reached"

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
agentLoop call agent models toolMap rt0 initialTurns = do
  emitEvent rt GenerationStarted
  go models initialTurns [] emptyUsage 0
  where
    rt = safeRuntimeArgs rt0
    finalAgent =
      agent
        { agSystemPrompt =
            Just (maybe toolBudgetUsedUpPrompt (<> "\n\n" <> toolBudgetUsedUpPrompt) agent.agSystemPrompt)
        }
    failWith err turns usage = do
      let errResult = GenerateErrorResult err turns usage
      emitEvent rt (GenerationFailed err errResult)
      pure $ Left errResult
    go :: ModelWithFallbacks -> [Turn] -> [Turn] -> Usage -> Int -> IO (Either GenerateErrorResult GenerateTextResult)
    go chain currentTurns newTurnsAcc currentUsage loopCount = do
      aborted <- isAbortedMaybe rt.rtAbortSignal
      let finalCall = loopCount >= agent.agMaxToolRounds
      if aborted
        then failWith GErrAborted newTurnsAcc currentUsage
        else do
          result <- call (if finalCall then finalAgent else agent) chain rt currentTurns
          case result of
            Left err -> failWith err newTurnsAcc currentUsage
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
                _
                  | finalCall -> do
                      -- The tool budget is used up: answer every call with a
                      -- placeholder so the history stays valid, run none.
                      let toolTurn = ToolTurn (map (`toolResult` toolNotRunText) toolCalls)
                      emitEvent rt (MessageCreated assistantMsg)
                      emitEvent rt (MessageCreated toolTurn)
                      failWith GErrToolExceeded (newTurnsAcc ++ [assistantMsg, toolTurn]) newUsage
                  | otherwise -> do
                      let toolContext = createToolContext agent currentTurns newUsage rt
                          tools = getResolvedTools id agent toolMap rt
                      emitEvent rt (MessageCreated assistantMsg)
                      emitEvent rt (ToolRoundStarted loopCount)

                      toolResultsE <- executeToolsWithAbort rt.rtAbortSignal rt.rtHooks toolContext tools toolCalls

                      case toolResultsE of
                        ToolRoundAborted done notRun -> do
                          -- Answer every tool call so the history stays valid
                          -- for the next request: real results for the calls
                          -- that ran (they may have had side effects), a
                          -- placeholder for the rest.
                          let toolTurn = ToolTurn (done ++ map (`toolResult` "Aborted before execution") notRun)
                          emitEvent rt (MessageCreated toolTurn)
                          failWith GErrAborted (newTurnsAcc ++ [assistantMsg, toolTurn]) newUsage
                        ToolRoundCompleted toolResults -> do
                          let toolTurn = ToolTurn toolResults
                          emitEvent rt (MessageCreated toolTurn)
                          emitEvent rt (ToolRoundFinished loopCount)
                          let turnsToAdd = [assistantMsg, toolTurn]
                          go nextChain (currentTurns ++ turnsToAdd) (newTurnsAcc ++ turnsToAdd) newUsage (loopCount + 1)
