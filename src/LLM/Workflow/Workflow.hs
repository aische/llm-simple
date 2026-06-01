{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module LLM.Workflow.Workflow where

-- import Data.Map qualified as Map

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import LLM
  ( ChatResponse (..),
    GeneratableObject,
    GenerateErrorResult (..),
    GenerateResult,
    ToolCall (..),
    ToolResult (..),
    Turn (AssistantTurn, ToolTurn, UserTurn),
    emptyUsage,
    genObject,
    generateTextWithFallbacks,
    getToolCalls,
  )
import LLM.Workflow.ToolUtils (createGenRequest, executeTool, getResolvedTools)
import LLM.Workflow.Types

-- import Data.Text qualified as T
-- import LLM.Core
--   ( ChatResponse (..),
--     ToolCall (..),
--     ToolResult (..),
--     Turn (AssistantTurn, ToolTurn, UserTurn),
--     emptyUsage,
--     getToolCalls,
--   )
-- import LLM.Generate
--   ( Hooks (..),
--     LogLevel (Debug, Info),
--     generateTextWithFallbacks,
--     -- streamTextWithFallbacks,
--   )
-- import LLM.Workflow.ToolUtils (executeTool, getResolvedTools, windowOffset, createGenRequest)

respToAssistantTurn :: ChatResponse -> (Turn, [ToolCall])
respToAssistantTurn cr = (AssistantTurn cr.respText cr.respReasoning toolCalls, toolCalls)
  where
    toolCalls = getToolCalls cr

callLLM :: (MonadIO m) => RuntimeArgs m -> Pending -> IO (GenerateResult ChatResponse)
callLLM rt pending = do
  let messages = pending.prompt.history ++ [UserTurn pending.prompt.prompt] ++ pending.toolRounds
  generateTextWithFallbacks (createGenRequest pending.prompt.agent.agent rt messages) pending.prompt.agent.models

callLLMO :: (GeneratableObject a, MonadIO m) => RuntimeArgs m -> Pending -> IO (GenerateResult a)
callLLMO rt pending = do
  let messages = pending.prompt.history ++ [UserTurn pending.prompt.prompt] ++ pending.toolRounds
  r <- genObject (createGenRequest pending.prompt.agent.agent rt messages) pending.prompt.agent.models
  case r of
    Left errResult -> pure $ Left errResult.gerError
    Right (value, _usage) -> pure $ Right value

mkFinal :: Pending -> Turn -> Final
mkFinal pending assistantTurn =
  Final
    { prompt = pending.prompt,
      history = pending.prompt.history,
      newMessages = [UserTurn pending.prompt.prompt] ++ pending.toolRounds ++ [assistantTurn],
      text = assistantTurnText assistantTurn
    }

assistantTurnText :: Turn -> Text
assistantTurnText (AssistantTurn text _ _) = text
assistantTurnText _ = ""

pendingHistory :: Pending -> [Turn]
pendingHistory pending = pending.prompt.history ++ [UserTurn pending.prompt.prompt] ++ pending.toolRounds

-- executeTool :: (MonadIO m) => Hooks -> ToolContext -> [Tool] -> ToolCall -> m (ToolOutcome m)
-- executeTool _hooks _ctx _tools _tc = undefined

runWorkflow :: (MonadIO m) => RuntimeArgs m -> Workflow m i o -> i -> m o
runWorkflow rt workflow i =
  loop rt (Stack (SWorkflow workflow i) KEmpty)

loop :: (MonadIO m) => RuntimeArgs m -> Stack m o -> m o
loop rt stack = do
  stack' <- eval rt stack --
  maybe (loop rt stack') pure $ isDone stack'

isDone :: Stack m r -> Maybe r
isDone (Stack (SReturn o) KEmpty) = Just o
isDone (Stack (SThrow err) _) = error $ show err
isDone (Stack _ _) = Nothing

eval :: (MonadIO m) => RuntimeArgs m -> Stack m o -> m (Stack m o)
eval rt (Stack step konts) = do
  _ <- liftIO $ TIO.putStrLn $ "eval: " <> showStep step
  _ <- liftIO $ TIO.putStrLn $ "konts: " <> showKont konts
  case step of
    SPromptO pending -> do
      result <- liftIO (callLLMO rt pending)
      case result of
        Left err -> pure $ Stack (SThrow err) konts
        Right value -> pure $ Stack (SReturn value) konts
    SPrompt pending mcid -> do
      result <- liftIO (callLLM rt pending)
      case result of
        Left err -> pure $ Stack (SThrow err) konts
        Right resp -> do
          let (assistantTurn, toolCalls) = respToAssistantTurn resp
          case toolCalls of
            [] ->
              let h = pendingHistory pending ++ [assistantTurn]
               in pure $
                    Stack
                      (SReturn $ mkFinal pending assistantTurn)
                      (maybe konts (\cid -> KUpdateHistory cid h konts) mcid)
            (toolCall : toolCalls') ->
              pure $
                Stack
                  (STool pending assistantTurn toolCall)
                  (KTool pending mcid assistantTurn toolCalls' [] toolCall konts)
    STool pending assistantTurn toolCall -> do
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
          liftIO $ print $ "ToolWorkflow: " <> show args
          pure $ Stack (SWorkflow workflow args) konts
        ToolReply text -> do
          liftIO $ print $ "ToolReply: " <> text
          pure $ Stack (SReturn text) konts
    SWorkflow workflow i -> case workflow of
      WPrompt a mbcid ->
        let h = maybe [] (lookupHistory konts) mbcid -- what happens to i.history?
         in let pending = Pending {prompt = Prompt {agent = a, prompt = i.prompt, history = h}, toolRounds = []}
             in pure $ Stack (SPrompt pending mbcid) konts
      WPromptO a ->
        let pending = Pending {prompt = Prompt {agent = a, prompt = i.prompt, history = i.history}, toolRounds = []}
         in pure $ Stack (SPromptO pending) konts
      WSeq workflow1 workflow2 pol ->
        pure $ Stack (SWorkflow workflow1 i) (KSeq1 workflow2 pol konts)
      WPar workflow1 workflow2 mergePolicy ->
        pure $ Stack (SWorkflow workflow1 i) (KPar1 i workflow2 mergePolicy konts)
      WLift f -> do
        o <- f i
        pure $ Stack (SReturn o) konts
      WMap workflow1 f ->
        pure $ Stack (SWorkflow workflow1 i) (KMap f konts)
      WLoop n wf policy cids ->
        pure $ Stack (SWorkflow wf i) (KLoop (n - 1) wf policy (Map.fromList [(cid, []) | cid <- cids]) konts)
    SThrow {} -> pure $ Stack step konts
    SReturn o -> case konts of
      KEmpty -> pure $ Stack step konts
      KTool pending mcid assistantTurn toolCalls toolResults toolCall k ->
        let tr = ToolResult toolCall.tcId toolCall.tcName o
            toolResults' = tr : toolResults
         in case toolCalls of
              (toolCall' : toolCalls') ->
                pure $
                  Stack (STool pending assistantTurn toolCall') (KTool pending mcid assistantTurn toolCalls' toolResults' toolCall' k)
              [] -> do
                let pending' = pending {toolRounds = pending.toolRounds ++ [assistantTurn, ToolTurn toolResults']}
                 in do
                      liftIO $ print $ "SPrompt pending': " <> show pending'
                      pure $ Stack (SPrompt pending' mcid) k
      KSeq1 workflow2 pol k ->
        let o' = transcriptPolicy pol o
         in pure $ Stack (SWorkflow workflow2 o') k
      KPar1 i workflow2 mergePolicy k ->
        pure $ Stack (SWorkflow workflow2 i) (KPar2 o mergePolicy k)
      KPar2 x mergePolicy k ->
        pure $ Stack (SReturn $ mergePolicy x o) k
      KMap pol k ->
        pure $ Stack (SReturn $ transcriptPolicy pol o) k
      KLoop n workflow policy cids k ->
        if n < 1
          then
            pure $ Stack (SReturn o) k
          else
            pure $ Stack (SWorkflow workflow $ transcriptPolicy policy o) (KLoop (n - 1) workflow policy cids k)
      KUpdateHistory cid history k -> do
        pure $ Stack step $ updateHistory cid history k

-- _ -> pure $ Stack step konts

--   pure (SWorkflow workflow args, KTool pending assistantTurn toolCalls' [] toolCall : konts)
-- pure (STool, KTool pending assistantTurn toolCalls [] : konts)
-- _ -> undefined

-- KontToolCall prompt _pcs toolCall ->
--   let tr = ToolResult toolCall.tcId toolCall.tcName text
--   in pure (step, konts')

-- lookupHistory :: Kont m o r -> CID -> [Turn]
-- lookupHistory [] _cid = []
-- lookupHistory k cid = case k of
--   KLoop _n _wf _policy _cids k ->
--     case Map.lookup cid _cids of
--       Nothing -> lookupHistory konts cid
--       Just h -> h
--   _ -> lookupHistory konts cid

-- updateHistory :: CID -> [Turn] -> [Kont] -> [Kont]
-- updateHistory cid history konts = case konts of
--   [] -> []
--   (k : konts') -> case k of
--     KontLoop n wf policy m ->
--       case Map.lookup cid m of
--         Nothing -> k : updateHistory cid history konts'
--         Just h ->
--           let m' = Map.insert cid (history ++ h) m
--            in KontLoop n wf policy m' : konts'
--     _ -> k : updateHistory cid history konts'

-- WPrompt a mbcid -> do
--   let h = maybe [] (lookupHistory konts) mbcid
--   pure
--     ( RunPrompt
--         ( Prompt
--             { agentWithModels = a,
--               history = args.history ++ h,
--               prompt = args.prompt
--             }
--         )
--         $ PromptStatePending [],
--       maybe konts (\cid -> KontUpdate cid : konts) mbcid
--     )
