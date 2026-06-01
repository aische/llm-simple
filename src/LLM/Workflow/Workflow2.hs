{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module LLM.Workflow.Workflow2 where

-- import Data.Map qualified as Map

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import LLM
  ( ChatResponse (..),
    GeneratableObject,
    GenerateError (..),
    GenerateErrorResult (..),
    GenerateResult,
    Hooks (..),
    ToolCall (..),
    ToolResult (..),
    Turn (AssistantTurn, UserTurn),
    emptyUsage,
    genObject,
    generateTextWithFallbacks,
    getToolCalls,
  )
import LLM.Workflow.ToolUtils (createGenRequest, getResolvedTools)
import LLM.Workflow.Types
  ( AgentWithModels (..),
    RuntimeArgs (..),
    Tool,
    ToolContext (..),
  )

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

callLLM :: RuntimeArgs -> Pending -> IO (GenerateResult ChatResponse)
callLLM rt pending = do
  let messages = pending.prompt.history ++ [UserTurn pending.prompt.prompt] ++ pending.toolRounds
  generateTextWithFallbacks (createGenRequest pending.prompt.agent.agent rt messages) pending.prompt.agent.models

callLLMO :: (GeneratableObject a) => RuntimeArgs -> Pending -> IO (GenerateResult a)
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

executeTool :: (MonadIO m) => Hooks -> ToolContext -> [Tool] -> ToolCall -> m (ToolOutcome m)
executeTool _hooks _ctx _tools _tc = undefined

data ToolOutcome m
  = ToolReply Text
  | ToolWorkflow (Workflow m PromptArgs Text) PromptArgs

data PromptArgs = PromptArgs
  { history :: [Turn],
    prompt :: Text
  }
  deriving (Show)

data Prompt = Prompt
  { agent :: AgentWithModels,
    prompt :: Text,
    history :: [Turn]
  }

data Pending = Pending
  { prompt :: Prompt,
    toolRounds :: [Turn]
  }

data Final = Final
  { prompt :: Prompt,
    history :: [Turn],
    newMessages :: [Turn],
    text :: Text
  }

data TranscriptPolicy2 i o where
  TranscriptPolicy2 :: (i -> o) -> TranscriptPolicy2 i o
  TranscriptFinalText :: TranscriptPolicy2 Final PromptArgs

transcriptPolicy2 :: TranscriptPolicy2 i o -> i -> o
transcriptPolicy2 (TranscriptPolicy2 f) i = f i
transcriptPolicy2 TranscriptFinalText final = PromptArgs {history = [], prompt = final.text}

type TranscriptPolicy i o = i -> o

type MergePolicy o1 o2 o = o1 -> o2 -> o

data Workflow m i o where
  WPrompt :: AgentWithModels -> Workflow m PromptArgs Final
  WPromptO :: (GeneratableObject a) => AgentWithModels -> Workflow m PromptArgs a
  WSeq :: Workflow m i x -> Workflow m y o -> TranscriptPolicy x y -> Workflow m i o
  WPar :: Workflow m i x -> Workflow m i y -> MergePolicy x y o -> Workflow m i o
  WLift :: (i -> m o) -> Workflow m i o
  WMap :: Workflow m i o -> TranscriptPolicy o o' -> Workflow m i o'

-- WLoop :: Int -> Workflow m i o -> TranscriptPolicy o i -> Workflow m i o

data Step m o where
  SPrompt :: Pending -> Step m Final
  SPromptO :: (GeneratableObject a) => Pending -> Step m a
  SReturn :: o -> Step m o
  STool :: Pending -> Turn -> ToolCall -> Step m Text
  SThrow :: GenerateError -> Step m o
  SWorkflow :: Workflow m i o -> i -> Step m o

-- RunWorkflow :: Workflow m o -> Step m o

data Kont m o r where
  KEmpty :: Kont m o o
  KTool :: Pending -> Turn -> [ToolCall] -> [ToolResult] -> ToolCall -> Kont m Final r -> Kont m Text r
  KSeq1 :: Workflow m y o -> TranscriptPolicy x y -> Kont m o r -> Kont m x r
  KPar1 :: i -> Workflow m i y -> MergePolicy x y o -> Kont m o r -> Kont m x r
  KPar2 :: x -> MergePolicy x y o -> Kont m o r -> Kont m y r
  KMap :: TranscriptPolicy o o' -> Kont m o' r -> Kont m o r

-- KPar :: x -> Workflow m i y -> MergePolicy x y o -> Kont m y r -> Kont m i r
-- KLoop :: Int -> Workflow m i o -> TranscriptPolicy o i -> [CID] -> Kont m o r -> Kont m i r

data Stack m r where
  Stack :: (Step m o) -> (Kont m o r) -> Stack m r

runWorkflow :: (MonadIO m) => RuntimeArgs -> Workflow m i o -> i -> m o
runWorkflow rt workflow i =
  loop rt (Stack (SWorkflow workflow i) KEmpty)

loop :: (MonadIO m) => RuntimeArgs -> Stack m o -> m o
loop rt stack = do
  stack' <- eval rt stack --
  maybe (loop rt stack') pure $ isDone stack'

isDone :: Stack m r -> Maybe r
isDone (Stack (SReturn o) KEmpty) = Just o
isDone (Stack _ _) = Nothing

eval :: (MonadIO m) => RuntimeArgs -> Stack m o -> m (Stack m o)
eval rt (Stack step konts) = case step of
  SPromptO pending -> do
    result <- liftIO (callLLMO rt pending)
    case result of
      Left err -> pure $ Stack (SThrow err) konts
      Right value -> pure $ Stack (SReturn value) konts
  SPrompt pending -> do
    result <- liftIO (callLLM rt pending)
    case result of
      Left err -> pure $ Stack (SThrow err) konts
      Right resp -> do
        let (assistantTurn, toolCalls) = respToAssistantTurn resp
        case toolCalls of
          [] ->
            pure $ Stack (SReturn $ mkFinal pending assistantTurn) konts
          (toolCall : toolCalls') ->
            pure $
              Stack
                (STool pending assistantTurn toolCall)
                (KTool pending assistantTurn toolCalls' [] toolCall konts)
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
      ToolWorkflow workflow args -> pure $ Stack (SWorkflow workflow args) konts
      ToolReply text -> pure $ Stack (SReturn text) konts
  SWorkflow workflow i -> case workflow of
    WPrompt a ->
      let pending = Pending {prompt = Prompt {agent = a, prompt = i.prompt, history = i.history}, toolRounds = []}
       in pure $ Stack (SPrompt pending) konts
    WPromptO a ->
      let pending = Pending {prompt = Prompt {agent = a, prompt = i.prompt, history = i.history}, toolRounds = []}
       in pure $ Stack (SPromptO pending) konts
    WSeq workflow1 workflow2 transcriptPolicy ->
      pure $ Stack (SWorkflow workflow1 i) (KSeq1 workflow2 transcriptPolicy konts)
    WPar workflow1 workflow2 mergePolicy ->
      pure $ Stack (SWorkflow workflow1 i) (KPar1 i workflow2 mergePolicy konts)
    WLift f -> do
      o <- f i
      pure $ Stack (SReturn o) konts
    WMap workflow1 f ->
      pure $ Stack (SWorkflow workflow1 i) (KMap f konts)
  SThrow {} -> pure $ Stack step konts
  SReturn o -> case konts of
    KEmpty -> pure $ Stack step konts
    KTool pending assistantTurn toolCalls toolResults toolCall k ->
      let tr = ToolResult toolCall.tcId toolCall.tcName o
          toolResults' = tr : toolResults
       in case toolCalls of
            (toolCall' : toolCalls') ->
              pure $
                Stack (STool pending assistantTurn toolCall') (KTool pending assistantTurn toolCalls' toolResults' toolCall' k)
            [] -> pure $ Stack (SReturn $ mkFinal pending assistantTurn) k
    KSeq1 workflow2 transcriptPolicy k ->
      let o' = transcriptPolicy o
       in pure $ Stack (SWorkflow workflow2 o') k
    KPar1 i workflow2 mergePolicy k ->
      pure $ Stack (SWorkflow workflow2 i) (KPar2 o mergePolicy k)
    KPar2 x mergePolicy k ->
      pure $ Stack (SReturn $ mergePolicy x o) k
    KMap f k ->
      pure $ Stack (SReturn $ f o) k

-- _ -> pure $ Stack step konts

--   pure (SWorkflow workflow args, KTool pending assistantTurn toolCalls' [] toolCall : konts)
-- pure (STool, KTool pending assistantTurn toolCalls [] : konts)
-- _ -> undefined

-- KontToolCall prompt _pcs toolCall ->
--   let tr = ToolResult toolCall.tcId toolCall.tcName text
--   in pure (step, konts')