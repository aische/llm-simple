module LLM.Agent.Generate6 where

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

data PromptArgs = PromptArgs
  { history :: [Turn],
    prompt :: Text
  }

data Prompt = Prompt
  { agentWithModels :: AgentWithModels,
    history :: [Turn],
    prompt :: Text
  }

data PromptState
  = PromptStatePending [Turn]
  | PromptStateFinal FinalResult
  | PromptStateToolCalls PromptToolCalling

data FinalResult = FinalResult
  { history :: [Turn],
    prompt :: Text,
    toolTurns :: [Turn],
    assistantTurn :: Turn,
    text :: Text
  }

data PromptToolCalling = PromptToolCalling
  { toolRounds :: [Turn],
    answer :: Turn,
    toolCalls :: [ToolCall],
    toolResults :: [ToolResult]
  }

data Step
  = RunPrompt Prompt PromptState
  | RunWorkflow Workflow PromptArgs

data Kont
  = KontToolCall Prompt PromptToolCalling ToolCall
  | KontSeq Workflow TranscriptPolicy
  | KontPar1 Workflow PromptArgs MergePolicy
  | KontPar2 FinalResult MergePolicy

data Workflow
  = WPrompt AgentWithModels
  | WSeq Workflow Workflow TranscriptPolicy
  | WPar Workflow Workflow MergePolicy

data TranscriptPolicy = TranscriptPolicy

transcript :: TranscriptPolicy -> FinalResult -> PromptArgs
transcript policy result = case policy of
  TranscriptPolicy -> PromptArgs {history = [], prompt = result.text}

data MergePolicy = MergePolicy

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

eval :: RuntimeArgs -> Step -> [Kont] -> IO (Step, [Kont])
eval rt step konts =
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
      WPrompt a -> do
        pure
          ( RunPrompt
              ( Prompt
                  { agentWithModels = a,
                    history = args.history,
                    prompt = args.prompt
                  }
              )
              $ PromptStatePending [],
            konts
          )
      WSeq workflow1 workflow2 transcriptPolicy -> do
        pure (RunWorkflow workflow1 args, KontSeq workflow2 transcriptPolicy : konts)
      WPar workflow1 workflow2 mergePolicy -> do
        pure (RunWorkflow workflow1 args, KontPar1 workflow2 args mergePolicy : konts)

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
