module LLM.Workflow.Workflow where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM
  ( ChatResponse (..),
    GeneratableObject,
    GenerateError,
    GenerateErrorResult (..),
    GenerateResult,
    StreamChunk (..),
    ToolCall (..),
    ToolResult (..),
    Turn (..),
    Usage (..),
    emptyUsage,
    genObject,
    generateTextWithFallbacks,
    streamTextWithFallbacks,
  )
import LLM.Workflow.ToolUtils (createGenRequest, createToolContext, executeTool, getResolvedTools)
import LLM.Workflow.Types
  ( Agent (agName),
    AgentWithModels (agent, models),
    Kont (..),
    LoopContext (..),
    Pending (..),
    Prompt (Prompt, agent, history, prompt),
    PromptArgs (history, prompt),
    RuntimeArgs (rtHooks),
    Stack (..),
    Step (..),
    ToolOutcome (ToolReply, ToolWorkflow),
    Workflow (..),
  )
import LLM.Workflow.Utils (CatchFrame (CatchFrame), lookupHistory, mergePolicy, pendingToFinal, pendingToTurns, respToAssistantTurn, showKont, showStep, stackSize, transcriptPolicy, unwindToCatch, updateHistory)
import Unsafe.Coerce (unsafeCoerce)

callLLM :: (MonadIO m) => RuntimeArgs m -> Pending -> IO (GenerateResult ChatResponse)
callLLM rt pending = do
  let messages = pendingToTurns pending
  generateTextWithFallbacks (createGenRequest pending.prompt.agent.agent rt messages) pending.prompt.agent.models

streamLLM :: (MonadIO m) => (StreamChunk -> IO ()) -> RuntimeArgs m -> Pending -> IO (GenerateResult ChatResponse)
streamLLM onChunk rt pending = do
  let messages = pendingToTurns pending
  streamTextWithFallbacks onChunk (createGenRequest pending.prompt.agent.agent rt messages) pending.prompt.agent.models

showStreamChunk :: StreamChunk -> Text
showStreamChunk = \case
  AnswerDelta txt -> txt
  ReasoningDelta txt -> txt
  PreambleDelta txt -> txt
  StreamToolCallChunk toolCall -> T.pack (show toolCall)

callLLMO :: (GeneratableObject a, MonadIO m) => RuntimeArgs m -> Pending -> IO (GenerateResult (a, Usage))
callLLMO rt pending = do
  let messages = pendingToTurns pending
  r <- genObject (createGenRequest pending.prompt.agent.agent rt messages) pending.prompt.agent.models
  case r of
    Left errResult -> pure $ Left errResult.gerError
    Right (value, usage) -> pure $ Right (value, usage)

runWorkflow :: (MonadIO m) => RuntimeArgs m -> Workflow m i o -> i -> m (Either GenerateError o, Usage)
runWorkflow rt workflow i =
  loop rt (Stack emptyUsage (RunWorkflow workflow i) KEmpty)

usageCents :: Usage -> Int
usageCents u = round (u.usageTotalCost * 100)

loop :: (MonadIO m) => RuntimeArgs m -> Stack m (Either GenerateError o) -> m (Either GenerateError o, Usage)
loop rt stack = do
  stack' <- eval rt stack
  case isDone stack' of
    (Just result, usage) -> pure (result, usage)
    (Nothing, _usage) -> loop rt stack'

isDone :: Stack m (Either GenerateError o) -> (Maybe (Either GenerateError o), Usage)
isDone (Stack usage (RunFinish e) KEmpty) = (Just (unsafeCoerce e), usage)
isDone (Stack usage _ _) = (Nothing, usage)

eval :: (MonadIO m) => RuntimeArgs m -> Stack m (Either GenerateError o) -> m (Stack m (Either GenerateError o))
eval rt (Stack uAcc step konts) = do
  let space = T.replicate (stackSize konts) " "
  _ <- liftIO $ TIO.putStrLn $ space <> showStep step <> T.unwords (map (" : " <>) (showKont konts))
  _ <- liftIO $ TIO.putStrLn $ space <> "Usage (cents): " <> T.pack (show (usageCents uAcc))
  case step of
    RunObject pending -> do
      result <- liftIO (callLLMO rt pending)
      case result of
        Left err -> pure $ Stack uAcc (RunThrow err) konts
        Right (value, usage) -> pure $ Stack (uAcc <> usage) (RunReturn value) konts
    RunPrompt pending mcid -> do
      let agentName = pending.prompt.agent.agent.agName
      -- result <- liftIO (callLLM rt pending)
      _ <- liftIO (TIO.putStr $ space <> agentName <> ": ")
      result <- liftIO (streamLLM (TIO.putStr . showStreamChunk) rt pending)
      case result of
        Left err -> pure $ Stack uAcc (RunThrow err) konts
        Right resp -> do
          let (text, assistantTurn, toolCalls) = respToAssistantTurn resp
          case toolCalls of
            [] ->
              let h = pendingToTurns pending ++ [assistantTurn]
               in pure $
                    Stack
                      (uAcc <> fromMaybe emptyUsage resp.respUsage)
                      (RunReturn $ pendingToFinal pending text assistantTurn)
                      (maybe konts (\cid -> KUpdateHistory cid h konts) mcid)
            (toolCall : toolCalls') ->
              pure $
                Stack
                  (uAcc <> fromMaybe emptyUsage resp.respUsage)
                  (RunTool pending assistantTurn toolCall)
                  (KTool pending mcid assistantTurn toolCalls' [] toolCall konts)
    RunTool pending _assistantTurn toolCall -> do
      let ctx = createToolContext pending.prompt.agent.agent pending.prompt.history emptyUsage rt
          tools = getResolvedTools pending.prompt.agent.agent rt
      result <- liftIO (executeTool rt.rtHooks ctx tools toolCall)
      case result of
        ToolWorkflow workflow args -> do
          pure $ Stack uAcc (RunWorkflow workflow args) konts
        ToolReply text -> do
          pure $ Stack uAcc (RunReturn text) konts
    RunWorkflow workflow i -> case workflow of
      WPrompt a mbcid ->
        let h = maybe i.history (lookupHistory konts) mbcid
         in let pending = Pending {prompt = Prompt {agent = a, prompt = i.prompt, history = h}, toolRounds = []}
             in pure $ Stack uAcc (RunPrompt pending mbcid) konts
      WObject a ->
        let pending = Pending {prompt = Prompt {agent = a, prompt = i.prompt, history = i.history}, toolRounds = []}
         in pure $ Stack uAcc (RunObject pending) konts
      WSeq workflow1 workflow2 pol ->
        pure $ Stack uAcc (RunWorkflow workflow1 i) (KSeq1 workflow2 pol konts)
      WPar workflow1 workflow2 pol ->
        pure $ Stack uAcc (RunWorkflow workflow1 i) (KPar1 i workflow2 pol konts)
      WLift f -> do
        o <- f i
        pure $ Stack uAcc (RunReturn o) konts
      WMap workflow1 f ->
        pure $ Stack uAcc (RunWorkflow workflow1 i) (KMap f konts)
      WLoop n wf policy cids ->
        pure $ Stack uAcc (RunWorkflow wf i) (KLoop (n - 1) wf policy (Map.fromList [(cid, []) | cid <- cids]) konts)
      WLoopWhile maxIterations decider decisionPolicy cids policy wf ->
        pure $
          Stack
            uAcc
            (RunWorkflow wf i)
            (KLoopWhile maxIterations 1 wf policy decider decisionPolicy (Map.fromList [(cid, []) | cid <- cids]) i [] konts)
      WLiftW f -> do
        wf <- f (fst i)
        pure $ Stack uAcc (RunWorkflow wf (snd i)) konts
      WCatch o wf ->
        pure $ Stack uAcc (RunWorkflow wf i) (KCatch o konts)
    RunFinish _ ->
      pure $ Stack uAcc step konts
    RunThrow err ->
      case unwindToCatch konts of
        Just (CatchFrame caughtValue k) ->
          pure $ Stack uAcc (RunReturn caughtValue) k
        Nothing ->
          pure $ Stack uAcc (RunFinish (Left err)) KEmpty
    RunReturn o -> case konts of
      KEmpty -> pure $ Stack uAcc (RunFinish (Right (unsafeCoerce o))) KEmpty
      KTool pending mcid assistantTurn toolCalls toolResults toolCall k ->
        let tr = ToolResult toolCall.tcId toolCall.tcName o
            toolResults' = tr : toolResults
         in case toolCalls of
              (toolCall' : toolCalls') ->
                pure $
                  Stack uAcc (RunTool pending assistantTurn toolCall') (KTool pending mcid assistantTurn toolCalls' toolResults' toolCall' k)
              [] -> do
                let pending' = pending {toolRounds = pending.toolRounds ++ [assistantTurn, ToolTurn toolResults']}
                 in do
                      pure $ Stack uAcc (RunPrompt pending' mcid) k
      KSeq1 workflow2 pol k ->
        let o' = transcriptPolicy pol o
         in pure $ Stack uAcc (RunWorkflow workflow2 o') k
      KPar1 i workflow2 pol k ->
        pure $ Stack uAcc (RunWorkflow workflow2 i) (KPar2 o pol k)
      KPar2 x pol k ->
        pure $ Stack uAcc (RunReturn $ mergePolicy pol x o) k
      KMap pol k ->
        pure $ Stack uAcc (RunReturn $ transcriptPolicy pol o) k
      KLoop n workflow policy cids k ->
        if n < 1
          then
            pure $ Stack uAcc (RunReturn o) k
          else
            pure $ Stack uAcc (RunWorkflow workflow $ transcriptPolicy policy o) (KLoop (n - 1) workflow policy cids k)
      KLoopWhile maxIterations iteration workflow policy decider decisionPolicy cids currentInput outputsRev k -> do
        if iteration >= maxIterations
          then pure $ Stack uAcc (RunReturn o) k
          else do
            let nextInput = transcriptPolicy policy o
                outputsRev' = o : outputsRev
                ctx =
                  LoopContext
                    { lcIteration = iteration,
                      lcMaxIterations = maxIterations,
                      lcInput = currentInput,
                      lcNextInput = nextInput,
                      lcOutput = o,
                      lcOutputs = reverse outputsRev'
                    }
            pure $
              Stack
                uAcc
                (RunWorkflow decider ctx)
                (KLoopWhileDecision maxIterations iteration workflow policy decider decisionPolicy cids nextInput outputsRev' o k)
      KLoopWhileDecision maxIterations iteration workflow policy decider decisionPolicy cids nextInput outputsRev lastOutput k ->
        if transcriptPolicy decisionPolicy o
          then
            pure $
              Stack
                uAcc
                (RunWorkflow workflow nextInput)
                (KLoopWhile maxIterations (iteration + 1) workflow policy decider decisionPolicy cids nextInput outputsRev k)
          else pure $ Stack uAcc (RunReturn lastOutput) k
      KUpdateHistory cid history k -> do
        pure $ Stack uAcc step $ updateHistory cid history k
      KCatch _r k ->
        pure $ Stack uAcc step k
