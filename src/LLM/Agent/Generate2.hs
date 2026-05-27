{- HLINT ignore "Eta reduce" -}
module LLM.Agent.Generate2
  ( streamText,
    generateText,
  )
where

import Data.Maybe (fromMaybe)
import LLM.Agent.ToolUtils
  ( createGenRequest,
    createToolContext,
    executeTool,
    getResolvedTools,
  )
import LLM.Agent.Types
  ( Agent (agMaxToolRounds),
    RuntimeArgs (..),
    Tool,
    ToolContext,
  )
import LLM.Core.Abort (isAbortedMaybe)
import LLM.Core.Types
  ( ChatResponse (..),
    ToolCall,
    ToolResult,
    Turn (..),
  )
import LLM.Core.Usage (Usage (..), emptyUsage)
import LLM.Core.Utils (getToolCalls)
import LLM.Generate.Generate
  ( generateTextWithFallbacks,
    streamTextWithFallbacks,
  )
import LLM.Generate.ModelConfig (ModelWithFallbacks (..))
import LLM.Generate.Types
  ( GenerateError (..),
    GenerateErrorResult (..),
    GenerateResult,
    GenerateTextResult (..),
    StreamChunk (..),
  )

-- | Agent loop as a free program over 'AgentF'.
generateText ::
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
generateText agent models rt turns =
  interpret env (buildProgram agent rt turns [] emptyUsage 0)
  where
    env =
      Env
        { envAgent = agent,
          envModels = models,
          envRt = rt,
          envCall = \a m r t -> generateTextWithFallbacks (createGenRequest a r t) m
        }

streamText ::
  (StreamChunk -> IO ()) ->
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
streamText onChunk agent models rt turns =
  interpret env (buildProgram agent rt turns [] emptyUsage 0)
  where
    env =
      Env
        { envAgent = agent,
          envModels = models,
          envRt = rt,
          envCall = \a m r t -> streamTextWithFallbacks onChunk (createGenRequest a r t) m
        }

-- ---------------------------------------------------------------------------
-- Step functor (free-monad style)
-- ---------------------------------------------------------------------------

data AgentF next
  = CallModel
      { stepCurrentTurns :: [Turn],
        stepAccTurns :: [Turn],
        stepUsage :: Usage,
        stepLoopCount :: Int,
        stepOnModelResult :: GenerateResult ChatResponse -> next
      }
  | RunTools
      { stepAgent :: Agent,
        stepRt :: RuntimeArgs,
        stepCurrentTurns :: [Turn],
        stepAccTurns :: [Turn],
        stepUsage :: Usage,
        stepLoopCount :: Int,
        stepAssistant :: Turn,
        stepToolCalls :: [ToolCall],
        stepOnToolsResult :: GenerateResult [ToolResult] -> next
      }

data Free f a = Pure a | Free (f (Free f a))

-- | Build the agent program (no abort or round checks here).
buildProgram ::
  Agent ->
  RuntimeArgs ->
  [Turn] ->
  [Turn] ->
  Usage ->
  Int ->
  Free AgentF (Either GenerateErrorResult GenerateTextResult)
buildProgram agent rt currentTurns accTurns usage loopCount =
  Free $
    CallModel
      { stepCurrentTurns = currentTurns,
        stepAccTurns = accTurns,
        stepUsage = usage,
        stepLoopCount = loopCount,
        stepOnModelResult = afterModel
      }
  where
    afterModel :: GenerateResult ChatResponse -> Free AgentF (Either GenerateErrorResult GenerateTextResult)
    afterModel (Left err) = Pure (Left (GenerateErrorResult err accTurns usage))
    afterModel (Right resp) =
      let txt = resp.respText
          toolCalls = getToolCalls resp
          roundUsage = fromMaybe emptyUsage resp.respUsage
          newUsage = usage <> roundUsage
       in case toolCalls of
            [] ->
              let finalTurn = AssistantTurn txt resp.respReasoning []
                  finalAcc = accTurns ++ [finalTurn]
               in Pure $
                    Right
                      GenerateTextResult
                        { gtrGenerationId = rt.rtGenerationId,
                          gtrNewMessages = finalAcc,
                          gtrText = txt,
                          gtrUsage = newUsage
                        }
            _ ->
              let assistantTurn = AssistantTurn txt resp.respReasoning toolCalls
               in Free $
                    RunTools
                      { stepAgent = agent,
                        stepRt = rt,
                        stepCurrentTurns = currentTurns,
                        stepAccTurns = accTurns,
                        stepUsage = newUsage,
                        stepLoopCount = loopCount,
                        stepAssistant = assistantTurn,
                        stepToolCalls = toolCalls,
                        stepOnToolsResult = afterTools assistantTurn newUsage
                      }

    afterTools ::
      Turn ->
      Usage ->
      GenerateResult [ToolResult] ->
      Free AgentF (Either GenerateErrorResult GenerateTextResult)
    afterTools assistantTurn newUsage = \case
      Left err ->
        Pure (Left (GenerateErrorResult err (accTurns ++ [assistantTurn]) newUsage))
      Right toolResults ->
        let toolTurn = ToolTurn toolResults
            turnsToAdd = [assistantTurn, toolTurn]
         in buildProgram
              agent
              rt
              (currentTurns ++ turnsToAdd)
              (accTurns ++ turnsToAdd)
              newUsage
              (loopCount + 1)

-- ---------------------------------------------------------------------------
-- Interpreter (abort + max rounds live here)
-- ---------------------------------------------------------------------------

data Env = Env
  { envAgent :: Agent,
    envModels :: ModelWithFallbacks,
    envRt :: RuntimeArgs,
    envCall ::
      Agent ->
      ModelWithFallbacks ->
      RuntimeArgs ->
      [Turn] ->
      IO (GenerateResult ChatResponse)
  }

interpret ::
  Env ->
  Free AgentF (Either GenerateErrorResult GenerateTextResult) ->
  IO (Either GenerateErrorResult GenerateTextResult)
interpret _ (Pure r) = pure r
interpret env (Free step) = case step of
  CallModel{stepCurrentTurns, stepAccTurns, stepUsage, stepLoopCount, stepOnModelResult} -> do
    checkAbort env stepAccTurns stepUsage >>= \case
      Left errResult -> pure (Left errResult)
      Right () ->
        checkMaxRounds env stepLoopCount stepAccTurns stepUsage >>= \case
          Left errResult -> pure (Left errResult)
          Right () -> do
            result <- env.envCall env.envAgent env.envModels env.envRt stepCurrentTurns
            interpret env (stepOnModelResult result)
  RunTools{stepAgent, stepRt, stepCurrentTurns, stepAccTurns, stepUsage, stepAssistant, stepToolCalls, stepOnToolsResult} -> do
    let accWithAssistant = stepAccTurns ++ [stepAssistant]
    checkAbort env accWithAssistant stepUsage >>= \case
      Left errResult -> pure (Left errResult)
      Right () -> do
        let toolContext = createToolContext stepAgent stepCurrentTurns stepUsage stepRt
            tools = getResolvedTools stepAgent stepRt
        toolResultsE <-
          runToolsWithAbortChecks env accWithAssistant stepUsage tools toolContext stepToolCalls
        interpret env (stepOnToolsResult toolResultsE)

checkAbort :: Env -> [Turn] -> Usage -> IO (Either GenerateErrorResult ())
checkAbort env acc usage = do
  aborted <- isAbortedMaybe env.envRt.rtAbortSignal
  if aborted
    then pure (Left (GenerateErrorResult GErrAborted acc usage))
    else pure (Right ())

checkMaxRounds ::
  Env ->
  Int ->
  [Turn] ->
  Usage ->
  IO (Either GenerateErrorResult ())
checkMaxRounds env loopCount acc usage =
  if loopCount >= env.envAgent.agMaxToolRounds
    then pure (Left (GenerateErrorResult GErrToolExceeded acc usage))
    else pure (Right ())

runToolsWithAbortChecks ::
  Env ->
  [Turn] ->
  Usage ->
  [Tool] ->
  ToolContext ->
  [ToolCall] ->
  IO (GenerateResult [ToolResult])
runToolsWithAbortChecks env partialAcc usage tools ctx toolCalls = go [] toolCalls
  where
    go acc [] = pure (Right (reverse acc))
    go acc (tc : rest) = do
      checkAbort env partialAcc usage >>= \case
        Left _ -> pure (Left GErrAborted)
        Right () -> do
          r <- executeTool env.envRt.rtHooks ctx tools tc
          go (r : acc) rest
