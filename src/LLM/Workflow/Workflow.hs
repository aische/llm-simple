module LLM.Workflow.Workflow where

import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core
  ( ChatResponse (..),
    ToolCall (..),
    ToolResult (..),
    Turn (AssistantTurn, ToolTurn, UserTurn),
    emptyUsage,
    getToolCalls,
  )
import LLM.Generate
  ( Hooks (..),
    LogLevel (Debug, Info),
    generateTextWithFallbacks,
    -- streamTextWithFallbacks,
  )
import LLM.Workflow.ToolUtils (executeTool, getResolvedTools, windowOffset, createGenRequest)
import LLM.Workflow.Types

newMessages :: FinalResult -> [Turn]
newMessages FinalResult {prompt, toolTurns, assistantTurn} =
  UserTurn prompt : toolTurns ++ [assistantTurn]

allMessages :: FinalResult -> [Turn]
allMessages FinalResult {history, prompt, toolTurns, assistantTurn} =
  history ++ [UserTurn prompt] ++ toolTurns ++ [assistantTurn]

transcript :: TranscriptPolicy -> FinalResult -> PromptArgs
transcript policy result = case policy of
  TranscriptPolicy -> PromptArgs {history = [], prompt = result.text}
  TranscriptSummaryOnly -> PromptArgs {history = [], prompt = showTurnsAsText $ allMessages result}

showTurnsAsText :: [Turn] -> Text
showTurnsAsText turns = T.unlines (map showTurn turns)
  where
    showTurn turn = case turn of
      UserTurn text -> "User: " <> text
      AssistantTurn text _ _ -> "Assistant: " <> text
      ToolTurn toolResults -> "Tool: " <> T.unwords (map (\x -> x.trName) toolResults)

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
  -- print (show step)
  -- print (show konts)
  case step of
    RunPrompt prompt state ->
      let next s = pure (RunPrompt prompt s, konts)
       in case state of
            PromptStatePending toolrounds -> do
              let messages = prompt.history ++ (UserTurn prompt.prompt : toolrounds)
              rt.rtHooks.onLog Debug (showLLMCall prompt konts)
              result <-
                generateTextWithFallbacks
                  -- streamTextWithFallbacks
                  -- print
                  (createGenRequest prompt.agentWithModels.agent rt messages)
                  prompt.agentWithModels.models
              case result of
                Left _err -> error "" -- pure (step, konts)
                Right resp -> do
                  let (assistantTurn, toolCalls) = respToAssistantTurn resp
                  if null toolCalls
                    then do
                      let name = prompt.agentWithModels.agent.agName
                      rt.rtHooks.onLog Info ("<<<" <> name <> ">>>: " <> resp.respText)
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
                KontToolCall p ptc toolCall ->
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
                              { toolRounds = ptc.toolRounds,
                                answer = ptc.answer,
                                toolCalls = ptc.toolCalls,
                                toolResults = tr : ptc.toolResults
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
            PromptStateToolCalls ptc ->
              case ptc.toolCalls of
                [] -> do
                  next $ PromptStatePending (ptc.toolRounds ++ [ptc.answer, ToolTurn ptc.toolResults])
                (toolCall : toolCalls') -> do
                  let msgs = prompt.history ++ ptc.toolRounds ++ [ptc.answer]
                      ctx =
                        ToolContext
                          { tcConversation = msgs,
                            tcUsage = emptyUsage,
                            tcWindowOffset = windowOffset prompt.agentWithModels.agent.agContextWindow msgs,
                            tcRuntimeArgs = rt
                          }
                      tools = getResolvedTools prompt.agentWithModels.agent rt
                  rt.rtHooks.onLog Debug (showToolCall prompt toolCall konts)
                  result <- executeTool rt.rtHooks ctx tools toolCall
                  case result of
                    ToolReply text ->
                      let tr =
                            ToolResult
                              { trCallId = toolCall.tcId,
                                trName = toolCall.tcName,
                                trContent = text
                              }
                       in next $
                            PromptStateToolCalls $
                              PromptToolCalling ptc.toolRounds ptc.answer toolCalls' $
                                tr : ptc.toolResults
                    ToolWorkflow workflow args ->
                      let k =
                            KontToolCall
                              prompt
                              PromptToolCalling
                                { toolRounds = ptc.toolRounds,
                                  answer = ptc.answer,
                                  toolCalls = ptc.toolCalls,
                                  toolResults = ptc.toolResults
                                }
                              toolCall
                       in pure
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

respToAssistantTurn :: ChatResponse -> (Turn, [ToolCall])
respToAssistantTurn cr = (AssistantTurn cr.respText cr.respReasoning toolCalls, toolCalls)
  where
    toolCalls = getToolCalls cr

showLLMCall :: Prompt -> [Kont] -> Text
showLLMCall prompt konts = "<<<" <> name <> ">>>" <> showKonts konts
  where
    name = prompt.agentWithModels.agent.agName

showToolCall :: Prompt -> ToolCall -> [Kont] -> Text
showToolCall prompt toolCall konts = "<<<" <> name <> ">>> (" <> toolName <> ")" <> showKonts konts
  where
    name = prompt.agentWithModels.agent.agName
    toolName = toolCall.tcName

showKonts :: [Kont] -> Text
showKonts konts = T.unwords (map showKont konts)

showKont :: Kont -> Text
showKont kont = case kont of
  KontToolCall prompt _pcs toolCall -> "| " <> prompt.agentWithModels.agent.agName <> ": " <> toolCall.tcName
  KontSeq {} -> "| KontSeq"
  KontPar1 {} -> "| KontPar1"
  KontPar2 {} -> "| KontPar2"
  KontLoop {} -> "| KontLoop"
  KontUpdate {} -> "| KontUpdate"

-- stepCallLLM :: Prompt -> RuntimeArgs -> [Turn] -> (PromptState -> IO b) -> IO b
-- stepCallLLM prompt rt toolrounds next = do
--   let messages = prompt.history ++ (UserTurn prompt.prompt : toolrounds)
--   result <-
--     generateTextWithFallbacks
--       -- streamTextWithFallbacks
--       -- print
--       (createGenRequest prompt.agentWithModels.agent rt messages)
--       prompt.agentWithModels.models
--   case result of
--     Left _err -> error "" -- pure (step, konts)
--     Right resp -> do
--       let (assistantTurn, toolCalls) = respToAssistantTurn resp
--       if null toolCalls
--         then do
--           next $
--             PromptStateFinal
--               FinalResult
--                 { history = prompt.history,
--                   prompt = prompt.prompt,
--                   toolTurns = toolrounds,
--                   assistantTurn,
--                   text = resp.respText
--                 }
--         else
--           next $
--             PromptStateToolCalls
--               PromptToolCalling
--                 { toolRounds = toolrounds,
--                   answer = assistantTurn,
--                   toolCalls,
--                   toolResults = []
--                 }

-- stepReturnFinalResult :: Prompt -> Step -> [Kont] -> FinalResult -> IO (Step, [Kont])
-- stepReturnFinalResult prompt step konts finalResult = case konts of
--   [] -> pure (step, [])
--   (k : konts') -> case k of
--     KontToolCall
--       p
--       PromptToolCalling
--         { toolRounds,
--           answer,
--           toolCalls,
--           toolResults
--         }
--       toolCall ->
--         let tr =
--               ToolResult
--                 { trCallId = toolCall.tcId,
--                   trName = toolCall.tcName,
--                   trContent = finalResult.text
--                 }
--             s =
--               RunPrompt p $
--                 PromptStateToolCalls
--                   PromptToolCalling
--                     { toolRounds,
--                       answer,
--                       toolCalls,
--                       toolResults = tr : toolResults
--                     }
--          in pure (s, konts')
--     KontSeq w2 policy ->
--       let args = transcript policy finalResult
--           s = RunWorkflow w2 args
--        in pure (s, konts')
--     KontPar1 w2 args policy ->
--       pure (RunWorkflow w2 args, KontPar2 finalResult policy : konts')
--     KontPar2 fr policy ->
--       let s = RunPrompt prompt $ PromptStateFinal $ merge policy fr finalResult
--        in pure (s, konts')
--     KontLoop n wf policy m -> do
--       print n
--       if n < 1
--         then
--           pure (step, konts')
--         else
--           let nextArgs = transcript policy finalResult
--            in pure (RunWorkflow wf nextArgs, KontLoop (n - 1) wf policy m : konts')
--     KontUpdate cid -> do
--       let konts'' = updateHistory cid (newMessages finalResult) konts' --
--        in pure (step, konts'')

-- foo :: RuntimeArgs -> (PromptState -> IO (Step, [Kont])) -> [Kont] -> Prompt -> PromptToolCalling -> IO (Step, [Kont])
-- foo
--   rt
--   next
--   konts
--   prompt
--   PromptToolCalling
--     { toolRounds,
--       answer,
--       toolCalls,
--       toolResults
--     } = case toolCalls of
--     [] -> do
--       next $ PromptStatePending (toolRounds ++ [answer, ToolTurn toolResults])
--     (toolCall : toolCalls') -> do
--       let ctx =
--             ToolContext
--               { tcConversation = prompt.history ++ toolRounds ++ [answer],
--                 tcUsage = emptyUsage,
--                 tcWindowOffset = windowOffset prompt.agentWithModels.agent.agContextWindow (prompt.history ++ toolRounds ++ [answer]),
--                 tcRuntimeArgs = rt
--               }
--           tools = getResolvedTools prompt.agentWithModels.agent rt
--       result <- executeTool rt.rtHooks ctx tools toolCall
--       -- result <- undefined
--       case result of
--         ToolReply text ->
--           let tr =
--                 ToolResult
--                   { trCallId = toolCall.tcId,
--                     trName = toolCall.tcName,
--                     trContent = text
--                   }
--            in next $
--                 PromptStateToolCalls $
--                   PromptToolCalling toolRounds answer toolCalls' $
--                     tr : toolResults
--         ToolWorkflow workflow args ->
--           let k =
--                 KontToolCall
--                   prompt
--                   PromptToolCalling
--                     { toolRounds,
--                       answer,
--                       toolCalls,
--                       toolResults
--                     }
--                   toolCall
--            in pure
--                 (RunWorkflow workflow args, k : konts)