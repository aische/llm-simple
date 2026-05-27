{- HLINT ignore "Eta reduce" -}
module LLM.Agent.Generate3
  ( streamText,
    generateText,
    AgentStep (..),
    buildAgentStep,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import LLM.Agent.Events (emitEvent)
import LLM.Agent.ToolUtils
  ( createGenRequest,
    createToolContext,
    executeTool,
    getResolvedTools,
  )
import LLM.Agent.Types
  ( Agent (agMaxToolRounds),
    GenerateEventDetail (..),
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

-- | Run the agent loop until a final or failed result.
generateText ::
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
generateText agent models rt initialTurns = do
  emitEvent rt GenerationStarted
  runStep
    (mkEnv agent models rt (\a m r t -> generateTextWithFallbacks (createGenRequest a r t) m))
    (buildAgentStep agent rt initialTurns [] emptyUsage 0)

streamText ::
  (StreamChunk -> IO ()) ->
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
streamText onChunk agent models rt initialTurns = do
  emitEvent rt GenerationStarted
  runStep
    ( mkEnv
        agent
        models
        rt
        (\a m r t -> streamTextWithFallbacks onChunk (createGenRequest a r t) m)
    )
    (buildAgentStep agent rt initialTurns [] emptyUsage 0)

-- ---------------------------------------------------------------------------
-- Pure agent program (continuation-passing; no IO, abort, or logging)
-- ---------------------------------------------------------------------------

data AgentStep
  = -- | Send a request to the model. The interpreter chooses streaming vs
    -- non-streaming and runs fallbacks / hooks.
    CallModel
      { asCurrentTurns :: [Turn],
        asAccTurns :: [Turn],
        asUsage :: Usage,
        asLoopCount :: Int,
        asOnModelResult :: GenerateResult ChatResponse -> AgentStep
      }
  | -- | Execute tool calls for the current round.
    ExecTools
      { asLoopCount :: Int,
        asCalls :: [ToolCall],
        asRespText :: Text,
        asReasoning :: Maybe Text,
        asAgent :: Agent,
        asRt :: RuntimeArgs,
        asCurrentTurns :: [Turn],
        asAccTurns :: [Turn],
        asUsage :: Usage,
        asOnToolsResult :: GenerateResult [ToolResult] -> AgentStep
      }
  | -- | Terminal: success or failure.
    Done (Either GenerateErrorResult GenerateTextResult)

-- | Build a pure 'AgentStep' program from agent state (same logic as
-- 'LLM.Agent.Generate' / 'Generate2', zero IO).
buildAgentStep ::
  Agent ->
  RuntimeArgs ->
  [Turn] ->
  [Turn] ->
  Usage ->
  Int ->
  AgentStep
buildAgentStep agent rt currentTurns accTurns usage loopCount =
  CallModel
    { asCurrentTurns = currentTurns,
      asAccTurns = accTurns,
      asUsage = usage,
      asLoopCount = loopCount,
      asOnModelResult = afterModel
    }
  where
    afterModel :: GenerateResult ChatResponse -> AgentStep
    afterModel (Left err) = Done (Left (GenerateErrorResult err accTurns usage))
    afterModel (Right resp) =
      let txt = resp.respText
          toolCalls = getToolCalls resp
          roundUsage = fromMaybe emptyUsage resp.respUsage
          newUsage = usage <> roundUsage
       in case toolCalls of
            [] ->
              let finalTurn = AssistantTurn txt resp.respReasoning []
                  finalAcc = accTurns ++ [finalTurn]
               in Done $
                    Right
                      GenerateTextResult
                        { gtrGenerationId = rt.rtGenerationId,
                          gtrNewMessages = finalAcc,
                          gtrText = txt,
                          gtrUsage = newUsage
                        }
            _ ->
              ExecTools
                { asLoopCount = loopCount,
                  asCalls = toolCalls,
                  asRespText = txt,
                  asReasoning = resp.respReasoning,
                  asAgent = agent,
                  asRt = rt,
                  asCurrentTurns = currentTurns,
                  asAccTurns = accTurns,
                  asUsage = newUsage,
                  asOnToolsResult = afterTools txt resp.respReasoning toolCalls newUsage
                }

    afterTools ::
      Text ->
      Maybe Text ->
      [ToolCall] ->
      Usage ->
      GenerateResult [ToolResult] ->
      AgentStep
    afterTools txt reasoning toolCalls newUsage = \case
      Left err ->
        let assistantTurn = AssistantTurn txt reasoning toolCalls
         in Done (Left (GenerateErrorResult err (accTurns ++ [assistantTurn]) newUsage))
      Right toolResults ->
        let assistantTurn = AssistantTurn txt reasoning toolCalls
            toolTurn = ToolTurn toolResults
            turnsToAdd = [assistantTurn, toolTurn]
         in buildAgentStep
              agent
              rt
              (currentTurns ++ turnsToAdd)
              (accTurns ++ turnsToAdd)
              newUsage
              (loopCount + 1)

-- ---------------------------------------------------------------------------
-- Step interpreter (abort, max rounds, events, tool hooks)
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

mkEnv ::
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  (Agent -> ModelWithFallbacks -> RuntimeArgs -> [Turn] -> IO (GenerateResult ChatResponse)) ->
  Env
mkEnv agent models rt call =
  Env
    { envAgent = agent,
      envModels = models,
      envRt = rt,
      envCall = call
    }

runStep :: Env -> AgentStep -> IO (Either GenerateErrorResult GenerateTextResult)
runStep env step = case step of
  Done r -> case r of
    Left errResult -> do
      emitEvent env.envRt (GenerationFailed errResult.gerError errResult)
      pure (Left errResult)
    Right success -> do
      let finalTurn = case reverse success.gtrNewMessages of
            (t : _) -> t
            [] -> AssistantTurn success.gtrText Nothing []
      emitEvent env.envRt (MessageFinalized finalTurn)
      emitEvent env.envRt (GenerationFinished success)
      pure (Right success)
  CallModel{asCurrentTurns, asAccTurns, asUsage, asLoopCount, asOnModelResult} -> do
    checkAbort env asAccTurns asUsage >>= \case
      Left errResult -> failGeneration env errResult
      Right () ->
        checkMaxRounds env asLoopCount asAccTurns asUsage >>= \case
          Left errResult -> failGeneration env errResult
          Right () -> do
            result <- env.envCall env.envAgent env.envModels env.envRt asCurrentTurns
            case result of
              Left err -> failGeneration env (GenerateErrorResult err asAccTurns asUsage)
              Right resp -> runStep env (asOnModelResult (Right resp))
  ExecTools{asLoopCount, asCalls, asRespText, asReasoning, asAgent, asRt, asCurrentTurns, asAccTurns, asUsage, asOnToolsResult} -> do
    let assistantTurn = AssistantTurn asRespText asReasoning asCalls
        accWithAssistant = asAccTurns ++ [assistantTurn]
    checkAbort env accWithAssistant asUsage >>= \case
      Left errResult -> failGeneration env errResult
      Right () -> do
        emitEvent env.envRt (MessageCreated assistantTurn)
        emitEvent env.envRt (ToolRoundStarted asLoopCount)
        let toolContext = createToolContext asAgent asCurrentTurns asUsage asRt
            tools = getResolvedTools asAgent asRt
        toolResultsE <-
          runToolsWithAbortChecks env accWithAssistant asUsage tools toolContext asCalls
        case toolResultsE of
          Left err -> failGeneration env (GenerateErrorResult err accWithAssistant asUsage)
          Right toolResults -> do
            let toolTurn = ToolTurn toolResults
            emitEvent env.envRt (MessageCreated toolTurn)
            emitEvent env.envRt (ToolRoundFinished asLoopCount)
            runStep env (asOnToolsResult (Right toolResults))

-- | Emit failure event and return.
failGeneration ::
  Env ->
  GenerateErrorResult ->
  IO (Either GenerateErrorResult GenerateTextResult)
failGeneration env errResult = do
  emitEvent env.envRt (GenerationFailed errResult.gerError errResult)
  pure (Left errResult)

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
