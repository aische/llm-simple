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
    agentRuntimeArgs,
    validateAgent,
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
import LLM.Generate.Internal.Generate
  ( generateTextWithFallbacksRotatedWith,
    streamTextWithFallbacksRotatedWith,
  )
import LLM.Generate.Internal.GenerateUtils (TurnCheck (SkipTurnCheck), logInvalidTurns)
import LLM.Generate.Logger (Hooks (..), LogLevel (Warn))
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
-- On success, 'LLM.Generate.Types.gtrNewMessages' contains only the turns produced during this
-- call (assistant and tool turns), not the input conversation.
-- Tool rounds continue until the model responds without tool calls or
-- 'LLM.Agent.Types.agMaxToolRounds' rounds have run. Then one final model call asks
-- for a text answer ('toolBudgetUsedUpPrompt'); tool calls in that reply
-- are not run, and the run ends with 'GErrToolExceeded'.
--
-- The warnings of 'LLM.Agent.ToolUtils.validateAgent' go to 'LLM.Generate.Logger.onLog' at
-- the start of the run, and so do the problems of the input conversation
-- ('LLM.Core.Types.validateTurns', 'LLM.Core.Types.validateConversation'),
-- once per run, as @turn <n>: <reason>@ with @n@ counting from 0 in the
-- input conversation.
generateText ::
  Agent ->
  ModelWithFallbacks ->
  ToolMap Text ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
generateText agent models toolMap rt initialTurns =
  agentLoop
    (\a m r t -> generateTextWithFallbacksRotatedWith SkipTurnCheck (createGenRequest id a toolMap r t) m)
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
    (\a m r t -> streamTextWithFallbacksRotatedWith SkipTurnCheck onChunk (createGenRequest id a toolMap r t) m)
    agent
    models
    toolMap
    rt
    initialTurns

-- | The line appended to the system prompt for the final model call, made
-- once 'LLM.Agent.Types.agMaxToolRounds' tool rounds have run.
toolBudgetUsedUpPrompt :: Text
toolBudgetUsedUpPrompt =
  "The tool budget for this run is used up. Answer in text; do not call tools."

-- | Result text for a tool call in the final reply, which is not run.
toolNotRunText :: Text
toolNotRunText = "Not run: tool round limit reached"

-- | The tool loop. The caller's turns are checked once, here; @call@ must
-- not check them again (the turns the loop adds come from providers).
-- @call@ returns the model chain rotated so the model that
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
  mapM_ (rt.rtHooks.onLog Warn) (validateAgent agent toolMap)
  logInvalidTurns rt.rtHooks initialTurns
  emitEvent rt GenerationStarted
  go models initialTurns [] emptyUsage 0
  where
    rt = agentRuntimeArgs agent rt0
    finalAgent =
      agent
        { agSystemPrompt =
            Just (maybe toolBudgetUsedUpPrompt (<> "\n\n" <> toolBudgetUsedUpPrompt) agent.agSystemPrompt)
        }
    -- New turns are collected newest first and reversed once at the end.
    failWith err newRev usage = do
      let errResult = GenerateErrorResult err (reverse newRev) usage
      emitEvent rt (GenerationFailed err errResult)
      pure $ Left errResult
    go :: ModelWithFallbacks -> [Turn] -> [Turn] -> Usage -> Int -> IO (Either GenerateErrorResult GenerateTextResult)
    go chain currentTurns newRev currentUsage loopCount = do
      aborted <- isAbortedMaybe rt.rtAbortSignal
      let finalCall = loopCount >= agent.agMaxToolRounds
      if aborted
        then failWith GErrAborted newRev currentUsage
        else do
          result <- call (if finalCall then finalAgent else agent) chain rt currentTurns
          case result of
            Left err -> failWith err newRev currentUsage
            Right (nextChain, resp) -> do
              let txt = resp.respText
                  toolCalls = getToolCalls resp
                  roundUsage = fromMaybe emptyUsage resp.respUsage
                  newUsage = currentUsage <> roundUsage
                  -- Authoritative ordered parts from the provider response.
                  assistantMsg = AssistantMessage resp.respContent

              case toolCalls of
                [] -> do
                  let finalTurnsAcc = reverse (assistantMsg : newRev)
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
                      failWith GErrToolExceeded (toolTurn : assistantMsg : newRev) newUsage
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
                          failWith GErrAborted (toolTurn : assistantMsg : newRev) newUsage
                        ToolRoundCompleted toolResults -> do
                          let toolTurn = ToolTurn toolResults
                          emitEvent rt (MessageCreated toolTurn)
                          emitEvent rt (ToolRoundFinished loopCount)
                          let turnsToAdd = [assistantMsg, toolTurn]
                          go nextChain (currentTurns ++ turnsToAdd) (toolTurn : assistantMsg : newRev) newUsage (loopCount + 1)
