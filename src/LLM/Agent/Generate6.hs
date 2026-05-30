module LLM.Agent.Generate6 where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import LLM.Agent.ToolUtils (getResolvedTools, windowOffset)
import LLM.Agent.Types (Agent (..), RuntimeArgs (..), Tool (..))
import LLM.Core
  ( ChatResponse (..),
    ToolCall (..),
    ToolResult (..),
    Turn (AssistantTurn, ToolTurn, UserTurn),
    getToolCalls,
  )
import LLM.Generate (GenRequest (..), ModelWithFallbacks, generateTextWithFallbacks)

data AgentWithModels = AgentWithModels
  { agent :: Agent,
    models :: ModelWithFallbacks
  }

instance Show AgentWithModels where
  show AgentWithModels {agent} = "AgentWithModels {agent = " <> show agent.agName <> "}"

data PromptArgs = PromptArgs
  { history :: [Turn],
    prompt :: Text
  }
  deriving (Show)

data Prompt = Prompt
  { agentWithModels :: AgentWithModels,
    history :: [Turn],
    prompt :: Text
  }
  deriving (Show)

data PromptState
  = PromptStatePending [Turn]
  | PromptStateFinal FinalResult
  | PromptStateToolCalls PromptToolCalling
  deriving (Show)

data FinalResult = FinalResult
  { history :: [Turn],
    prompt :: Text,
    toolTurns :: [Turn],
    assistantTurn :: Turn,
    text :: Text
  }
  deriving (Show)

newMessages :: FinalResult -> [Turn]
newMessages FinalResult {prompt, toolTurns, assistantTurn} =
  UserTurn prompt : toolTurns ++ [assistantTurn]

data PromptToolCalling = PromptToolCalling
  { toolRounds :: [Turn],
    answer :: Turn,
    toolCalls :: [ToolCall],
    toolResults :: [ToolResult]
  }
  deriving (Show)

data Step
  = RunPrompt Prompt PromptState
  | RunWorkflow Workflow PromptArgs
  deriving (Show)

data Kont
  = KontToolCall Prompt PromptToolCalling ToolCall
  | KontSeq Workflow TranscriptPolicy
  | KontPar1 Workflow PromptArgs MergePolicy
  | KontPar2 FinalResult MergePolicy
  | KontLoop Int Workflow TranscriptPolicy (Map CID [Turn])
  | KontUpdate CID
  deriving (Show)

newtype CID = CID Text
  deriving (Eq, Ord, Show)

data Workflow
  = WPrompt AgentWithModels (Maybe CID)
  | WSeq Workflow Workflow TranscriptPolicy
  | WPar Workflow Workflow MergePolicy
  | WLoop Int Workflow TranscriptPolicy [CID]
  deriving (Show)

data TranscriptPolicy = TranscriptPolicy
  deriving (Show)

transcript :: TranscriptPolicy -> FinalResult -> PromptArgs
transcript policy result = case policy of
  TranscriptPolicy -> PromptArgs {history = [], prompt = result.text}

data MergePolicy = MergePolicy
  deriving (Show)

merge :: MergePolicy -> FinalResult -> FinalResult -> FinalResult
merge policy fr1 fr2 = case policy of
  MergePolicy -> FinalResult {history = [], prompt = prompt, toolTurns = toolTurns, assistantTurn = assistantTurn, text = text}
    where
      prompt = fr1.prompt <> fr2.prompt
      toolTurns = []
      assistantTurn = AssistantTurn (fr1.text <> fr2.text) (Just "") []
      text = fr1.text <> fr2.text

toPromptArgs :: FinalResult -> PromptArgs
toPromptArgs FinalResult {history, prompt, toolTurns, assistantTurn, text} = PromptArgs {history = messages, prompt = text}
  where
    messages = history ++ (UserTurn prompt : toolTurns) ++ [assistantTurn]

loop :: RuntimeArgs -> Step -> [Kont] -> IO FinalResult
loop _rt (RunPrompt _prompt (PromptStateFinal fr)) [] = pure fr
loop rt step konts = do
  (nextStep, nextKonts) <- eval rt step konts
  loop rt nextStep nextKonts

eval :: RuntimeArgs -> Step -> [Kont] -> IO (Step, [Kont])
eval rt step konts = do
  print (show step)
  print (show konts)
  case step of
    RunPrompt prompt state ->
      let next s = pure (RunPrompt prompt s, konts)
       in case state of
            PromptStatePending toolrounds -> do
              let messages = prompt.history ++ (UserTurn prompt.prompt : toolrounds)
              result <-
                generateTextWithFallbacks
                  (createGenRequest prompt.agentWithModels.agent rt messages)
                  prompt.agentWithModels.models
              case result of
                Left _err -> error "" -- pure (step, konts)
                Right resp -> do
                  let (assistantTurn, toolCalls) = respToAssistantTurn resp
                  if null toolCalls
                    then do
                      next $
                        PromptStateFinal
                          FinalResult
                            { history = prompt.history,
                              prompt = prompt.prompt,
                              toolTurns = toolrounds,
                              assistantTurn,
                              text = resp.respText
                            }
                    else
                      next $
                        PromptStateToolCalls
                          PromptToolCalling
                            { toolRounds = toolrounds,
                              answer = assistantTurn,
                              toolCalls,
                              toolResults = []
                            }
            PromptStateFinal finalResult -> case konts of
              [] -> pure (step, [])
              (k : konts') -> case k of
                KontToolCall
                  p
                  PromptToolCalling
                    { toolRounds,
                      answer,
                      toolCalls,
                      toolResults
                    }
                  toolCall ->
                    let tr =
                          ToolResult
                            { trCallId = toolCall.tcId,
                              trName = toolCall.tcName,
                              trContent = finalResult.text
                            }
                        s =
                          RunPrompt p $
                            PromptStateToolCalls
                              PromptToolCalling
                                { toolRounds,
                                  answer,
                                  toolCalls,
                                  toolResults = tr : toolResults
                                }
                     in pure (s, konts')
                KontSeq w2 policy ->
                  let args = transcript policy finalResult
                      s = RunWorkflow w2 args
                   in pure (s, konts')
                KontPar1 w2 args policy ->
                  pure (RunWorkflow w2 args, KontPar2 finalResult policy : konts')
                KontPar2 fr policy ->
                  let s = RunPrompt prompt $ PromptStateFinal $ merge policy fr finalResult
                   in pure (s, konts')
                KontLoop n wf policy m -> do
                  print n
                  if n < 1
                    then
                      pure (step, konts')
                    else
                      let nextArgs = transcript policy finalResult
                       in pure (RunWorkflow wf nextArgs, KontLoop (n - 1) wf policy m : konts')
                KontUpdate cid -> do
                  let konts'' = updateHistory cid (newMessages finalResult) konts' --
                   in pure (step, konts'')
            PromptStateToolCalls
              PromptToolCalling
                { toolRounds,
                  answer,
                  toolCalls,
                  toolResults
                } -> case toolCalls of
                [] -> do
                  next $ PromptStatePending (toolRounds ++ [answer, ToolTurn toolResults])
                (toolCall : toolCalls') -> do
                  result <- undefined
                  case result of
                    Left text ->
                      let tr =
                            ToolResult
                              { trCallId = toolCall.tcId,
                                trName = toolCall.tcName,
                                trContent = text
                              }
                       in next $
                            PromptStateToolCalls $
                              PromptToolCalling toolRounds answer toolCalls' $
                                tr : toolResults
                    Right (workflow, args) ->
                      let k =
                            KontToolCall
                              prompt
                              PromptToolCalling
                                { toolRounds,
                                  answer,
                                  toolCalls,
                                  toolResults
                                }
                              toolCall
                       in pure $
                            (RunWorkflow workflow args, k : konts)
    RunWorkflow workflow args -> case workflow of
      WPrompt a mbcid -> do
        let h = maybe [] (lookupHistory konts) mbcid
        pure
          ( RunPrompt
              ( Prompt
                  { agentWithModels = a,
                    history = args.history ++ h,
                    prompt = args.prompt
                  }
              )
              $ PromptStatePending [],
            maybe konts (\cid -> KontUpdate cid : konts) mbcid
          )
      WSeq workflow1 workflow2 transcriptPolicy -> do
        pure (RunWorkflow workflow1 args, KontSeq workflow2 transcriptPolicy : konts)
      WPar workflow1 workflow2 mergePolicy -> do
        pure (RunWorkflow workflow1 args, KontPar1 workflow2 args mergePolicy : konts)
      WLoop n workflow1 policy cids -> do
        let m = Map.fromList [(cid, []) | cid <- cids]
        pure (RunWorkflow workflow1 args, KontLoop (n - 1) workflow1 policy m : konts)

lookupHistory :: [Kont] -> CID -> [Turn]
lookupHistory [] _cid = []
lookupHistory (k : konts) cid = case k of
  KontLoop _n _wf _policy m ->
    case Map.lookup cid m of
      Nothing -> lookupHistory konts cid
      Just h -> h
  _ -> lookupHistory konts cid

-- lookupHistory cid [] = []
-- lookupHistory cid (k : konts) = case k of
--  KontLoop n wf policy -> if cid == cid then k : lookupHistory cid konts else lookupHistory cid konts
--  _ -> lookupHistory cid konts

updateHistory :: CID -> [Turn] -> [Kont] -> [Kont]
updateHistory cid history konts = case konts of
  [] -> []
  (k : konts') -> case k of
    KontLoop n wf policy m ->
      case Map.lookup cid m of
        Nothing -> k : updateHistory cid history konts'
        Just h ->
          let m' = Map.insert cid (history ++ h) m
           in KontLoop n wf policy m' : konts'
    _ -> k : updateHistory cid history konts'

createGenRequest :: Agent -> RuntimeArgs -> [Turn] -> GenRequest
createGenRequest agent rt messages =
  let offset = windowOffset agent.agContextWindow messages
      tools = getResolvedTools agent rt
   in GenRequest
        { grSystemPrompt = agent.agSystemPrompt,
          grTools = map (\x -> x.toolDef) tools,
          grMessages = drop offset messages,
          grAbortSignal = rt.rtAbortSignal,
          grLLMHooks = rt.rtLLMHooks,
          grHooks = rt.rtHooks
        }

respToAssistantTurn :: ChatResponse -> (Turn, [ToolCall])
respToAssistantTurn cr = (AssistantTurn cr.respText cr.respReasoning toolCalls, toolCalls)
  where
    toolCalls = getToolCalls cr
