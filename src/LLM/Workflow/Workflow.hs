module LLM.Workflow.Workflow where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM
  ( ChatResponse (..),
    GeneratableObject,
    GenerateErrorResult (..),
    GenerateResult,
    ToolCall (..),
    ToolResult (..),
    Turn (..),
    emptyUsage,
    genObject,
    generateTextWithFallbacks,
  )
import LLM.Workflow.ToolUtils (createGenRequest, executeTool, getResolvedTools)
import LLM.Workflow.Types
  ( AgentWithModels (agent, models),
    Kont (..),
    LoopContext (..),
    Pending (..),
    Prompt (Prompt, agent, history, prompt),
    PromptArgs (history, prompt),
    RuntimeArgs (rtHooks),
    Stack (..),
    Step (..),
    ToolContext (..),
    ToolOutcome (ToolReply, ToolWorkflow),
    Workflow (..),
  )
import LLM.Workflow.Utils (lookupHistory, mergePolicy, pendingToFinal, pendingToTurns, respToAssistantTurn, showKont, showStep, stackSize, transcriptPolicy, updateHistory)

callLLM :: (MonadIO m) => RuntimeArgs m -> Pending -> IO (GenerateResult ChatResponse)
callLLM rt pending = do
  let messages = pendingToTurns pending
  generateTextWithFallbacks (createGenRequest pending.prompt.agent.agent rt messages) pending.prompt.agent.models

callLLMO :: (GeneratableObject a, MonadIO m) => RuntimeArgs m -> Pending -> IO (GenerateResult a)
callLLMO rt pending = do
  let messages = pendingToTurns pending
  r <- genObject (createGenRequest pending.prompt.agent.agent rt messages) pending.prompt.agent.models
  case r of
    Left errResult -> pure $ Left errResult.gerError
    Right (value, _usage) -> pure $ Right value

runWorkflow :: (MonadIO m) => RuntimeArgs m -> Workflow m i o -> i -> m o
runWorkflow rt workflow i =
  loop rt (Stack (RunWorkflow workflow i) KEmpty)

loop :: (MonadIO m) => RuntimeArgs m -> Stack m o -> m o
loop rt stack = do
  stack' <- eval rt stack --
  maybe (loop rt stack') pure $ isDone stack'

isDone :: Stack m r -> Maybe r
isDone (Stack (RunReturn o) KEmpty) = Just o
isDone (Stack (RunThrow err) KEmpty) = error $ show err
isDone (Stack _ _) = Nothing

eval :: (MonadIO m) => RuntimeArgs m -> Stack m o -> m (Stack m o)
eval rt (Stack step konts) = do
  _ <- liftIO $ TIO.putStrLn $ T.replicate (stackSize konts) " " <> showStep step <> T.unwords (map (" : " <>) (showKont konts))
  case step of
    RunObject pending -> do
      result <- liftIO (callLLMO rt pending)
      case result of
        Left err -> pure $ Stack (RunThrow err) konts
        Right value -> pure $ Stack (RunReturn value) konts
    RunPrompt pending mcid -> do
      result <- liftIO (callLLM rt pending)
      case result of
        Left err -> pure $ Stack (RunThrow err) konts
        Right resp -> do
          let (text, assistantTurn, toolCalls) = respToAssistantTurn resp
          case toolCalls of
            [] ->
              let h = pendingToTurns pending ++ [assistantTurn]
               in pure $
                    Stack
                      (RunReturn $ pendingToFinal pending text assistantTurn)
                      (maybe konts (\cid -> KUpdateHistory cid h konts) mcid)
            (toolCall : toolCalls') ->
              pure $
                Stack
                  (RunTool pending assistantTurn toolCall)
                  (KTool pending mcid assistantTurn toolCalls' [] toolCall konts)
    RunTool pending assistantTurn toolCall -> do
      let ctx =
            ToolContext
              { tcConversation = pending.prompt.history ++ pending.toolRounds ++ [assistantTurn],
                tcUsage = emptyUsage,
                tcWindowOffset = 0,
                tcRuntimeArgs = rt
              }
          tools = getResolvedTools pending.prompt.agent.agent rt
      result <- executeTool rt.rtHooks ctx tools toolCall
      case result of
        ToolWorkflow workflow args -> do
          pure $ Stack (RunWorkflow workflow args) konts
        ToolReply text -> do
          pure $ Stack (RunReturn text) konts
    RunWorkflow workflow i -> case workflow of
      WPrompt a mbcid ->
        let h = maybe i.history (lookupHistory konts) mbcid
         in let pending = Pending {prompt = Prompt {agent = a, prompt = i.prompt, history = h}, toolRounds = []}
             in pure $ Stack (RunPrompt pending mbcid) konts
      WObject a ->
        let pending = Pending {prompt = Prompt {agent = a, prompt = i.prompt, history = i.history}, toolRounds = []}
         in pure $ Stack (RunObject pending) konts
      WSeq workflow1 workflow2 pol ->
        pure $ Stack (RunWorkflow workflow1 i) (KSeq1 workflow2 pol konts)
      WPar workflow1 workflow2 pol ->
        pure $ Stack (RunWorkflow workflow1 i) (KPar1 i workflow2 pol konts)
      WLift f -> do
        o <- f i
        pure $ Stack (RunReturn o) konts
      WMap workflow1 f ->
        pure $ Stack (RunWorkflow workflow1 i) (KMap f konts)
      WLoop n wf policy cids ->
        pure $ Stack (RunWorkflow wf i) (KLoop (n - 1) wf policy (Map.fromList [(cid, []) | cid <- cids]) konts)
      WLoopWhile maxIterations decider decisionPolicy cids policy wf ->
        pure $
          Stack
            (RunWorkflow wf i)
            (KLoopWhile maxIterations 1 wf policy decider decisionPolicy (Map.fromList [(cid, []) | cid <- cids]) i [] konts)
      WLiftW f -> do
        wf <- f (fst i)
        pure $ Stack (RunWorkflow wf (snd i)) konts
      WCatch o wf ->
        pure $ Stack (RunWorkflow wf i) (KCatch o konts)
    RunThrow {} ->
      case konts of
        KCatch o k ->
          pure $ Stack (RunReturn o) k
        _ ->
          pure $ Stack step konts
    RunReturn o -> case konts of
      KEmpty -> pure $ Stack step konts
      KTool pending mcid assistantTurn toolCalls toolResults toolCall k ->
        let tr = ToolResult toolCall.tcId toolCall.tcName o
            toolResults' = tr : toolResults
         in case toolCalls of
              (toolCall' : toolCalls') ->
                pure $
                  Stack (RunTool pending assistantTurn toolCall') (KTool pending mcid assistantTurn toolCalls' toolResults' toolCall' k)
              [] -> do
                let pending' = pending {toolRounds = pending.toolRounds ++ [assistantTurn, ToolTurn toolResults']}
                 in do
                      pure $ Stack (RunPrompt pending' mcid) k
      KSeq1 workflow2 pol k ->
        let o' = transcriptPolicy pol o
         in pure $ Stack (RunWorkflow workflow2 o') k
      KPar1 i workflow2 pol k ->
        pure $ Stack (RunWorkflow workflow2 i) (KPar2 o pol k)
      KPar2 x pol k ->
        pure $ Stack (RunReturn $ mergePolicy pol x o) k
      KMap pol k ->
        pure $ Stack (RunReturn $ transcriptPolicy pol o) k
      KLoop n workflow policy cids k ->
        if n < 1
          then
            pure $ Stack (RunReturn o) k
          else
            pure $ Stack (RunWorkflow workflow $ transcriptPolicy policy o) (KLoop (n - 1) workflow policy cids k)
      KLoopWhile maxIterations iteration workflow policy decider decisionPolicy cids currentInput outputsRev k -> do
        if iteration >= maxIterations
          then pure $ Stack (RunReturn o) k
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
                (RunWorkflow decider ctx)
                (KLoopWhileDecision maxIterations iteration workflow policy decider decisionPolicy cids nextInput outputsRev' o k)
      KLoopWhileDecision maxIterations iteration workflow policy decider decisionPolicy cids nextInput outputsRev lastOutput k ->
        if transcriptPolicy decisionPolicy o
          then
            pure $
              Stack
                (RunWorkflow workflow nextInput)
                (KLoopWhile maxIterations (iteration + 1) workflow policy decider decisionPolicy cids nextInput outputsRev k)
          else pure $ Stack (RunReturn lastOutput) k
      KUpdateHistory cid history k -> do
        pure $ Stack step $ updateHistory cid history k
      KCatch r k ->
        pure $ Stack (RunReturn r) k
