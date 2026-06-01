module LLM.Workflow.Utils where

import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import LLM (getToolCalls)
import LLM.Core.Types
  ( ChatResponse (..),
    ToolCall (..),
    ToolResult (..),
    Turn (AssistantTurn, ToolTurn, UserTurn),
  )
import LLM.Workflow.Types
  ( Agent (..),
    AgentWithModels (..),
    CID,
    Final (..),
    Kont (..),
    MergePolicy (..),
    Pending (prompt, toolRounds),
    Prompt (..),
    PromptArgs (PromptArgs, history, prompt),
    Step (..),
    TranscriptPolicy (..),
  )

-- * Conversation mapping functions -----------------------------------------

pendingToFinal :: Pending -> Text -> Turn -> Final
pendingToFinal pending text assistantTurn =
  Final
    { prompt = Just pending.prompt,
      history = pending.prompt.history,
      newMessages = [UserTurn pending.prompt.prompt] ++ pending.toolRounds ++ [assistantTurn],
      text = text
    }

pendingToTurns :: Pending -> [Turn]
pendingToTurns pending = pending.prompt.history ++ [UserTurn pending.prompt.prompt] ++ pending.toolRounds

turnsToConversationText :: [Turn] -> Text
turnsToConversationText turns = T.unlines (map showTurn turns)
  where
    showTurn turn = case turn of
      UserTurn text -> "User: " <> text <> "\n"
      AssistantTurn text _ _ -> "Assistant: " <> text <> "\n"
      ToolTurn toolResults -> "Tool: " <> T.unwords (map (\x -> x.trName) toolResults) <> "\n"

respToAssistantTurn :: ChatResponse -> (Text, Turn, [ToolCall])
respToAssistantTurn cr = (cr.respText, AssistantTurn cr.respText cr.respReasoning toolCalls, toolCalls)
  where
    toolCalls = getToolCalls cr

emptyFinal :: Text -> Final
emptyFinal text =
  Final
    { prompt = Nothing,
      history = [],
      newMessages = [],
      text = text
    }

-- * Transcript policy functions --------------------------------------------

transcriptPolicy :: TranscriptPolicy i o -> i -> o
transcriptPolicy (TranscriptPolicyFunc f) i = f i
transcriptPolicy TranscriptFinalToPromptArgs final = PromptArgs {history = [], prompt = final.text}
transcriptPolicy TranscriptFinalText final = final.text
transcriptPolicy TranscriptSummaryText final = "Summary: " <> turnsToConversationText allMessages
  where
    allMessages = final.history ++ final.newMessages

mergePolicy :: MergePolicy o1 o2 o -> o1 -> o2 -> o
mergePolicy (MergePolicyFunc f) o1 o2 = f o1 o2
mergePolicy MergePolicyFinalToPromptArgs final1 final2 = PromptArgs {history = [], prompt = final1.text <> "\n\n\n" <> final2.text}

-- * Lookup and update history functions ------------------------------------

lookupHistory :: Kont m o r -> CID -> [Turn]
lookupHistory kont cid = case kont of
  KEmpty -> []
  KTool _pending _mcid _assistantTurn _toolCalls _toolResults _toolCall k -> lookupHistory k cid
  KSeq1 _workflow2 _pol k -> lookupHistory k cid
  KPar1 _i _workflow2 _mergePolicy k -> lookupHistory k cid
  KPar2 _x _mergePolicy k -> lookupHistory k cid
  KMap _pol k -> lookupHistory k cid
  KUpdateHistory _cid _history k -> lookupHistory k cid
  KLoop _n _workflow _policy cids k ->
    case Map.lookup cid cids of
      Nothing -> lookupHistory k cid
      Just history -> history
  KCatch _o k -> lookupHistory k cid

updateHistory :: CID -> [Turn] -> Kont m o r -> Kont m o r
updateHistory cid history kont = case kont of
  KEmpty -> KEmpty
  KTool pending mcid assistantTurn toolCalls toolResults toolCall k ->
    KTool pending mcid assistantTurn toolCalls toolResults toolCall (updateHistory cid history k)
  KSeq1 workflow2 pol k ->
    KSeq1 workflow2 pol (updateHistory cid history k)
  KPar1 i workflow2 pol k ->
    KPar1 i workflow2 pol (updateHistory cid history k)
  KPar2 x pol k ->
    KPar2 x pol (updateHistory cid history k)
  KMap pol k ->
    KMap pol (updateHistory cid history k)
  KLoop n workflow pol cids k -> case Map.lookup cid cids of
    Nothing -> KLoop n workflow pol cids (updateHistory cid history k)
    Just _h -> KLoop n workflow pol (Map.insert cid history cids) k
  KUpdateHistory c h k ->
    KUpdateHistory c h (updateHistory cid history k)
  KCatch o k -> KCatch o (updateHistory cid history k)

stackSize :: Kont m o r -> Int
stackSize kont = case kont of
  KEmpty -> 0
  KTool _pending _mcid _assistantTurn _toolCalls _toolResults _toolCall k -> 1 + stackSize k
  KSeq1 _workflow2 _pol k -> 1 + stackSize k
  KPar1 _i _workflow2 _mergePolicy k -> 1 + stackSize k
  KPar2 _x _mergePolicy k -> 1 + stackSize k
  KMap _pol k -> 1 + stackSize k
  KLoop _n _workflow _policy _cids k -> 1 + stackSize k
  KUpdateHistory _cid _history k -> 1 + stackSize k
  KCatch _o k -> 1 + stackSize k

-- * Show functions for debugging -------------------------------------------

showAgent :: Pending -> Text
showAgent pending = pending.prompt.agent.agent.agName

showStep :: Step m o -> Text
showStep step =
  case step of
    RunPrompt pending _mcid -> "RunPrompt " <> showAgent pending
    RunObject pending -> "RunObject " <> showAgent pending
    RunReturn _o -> "RunReturn"
    RunTool _pending _assistantTurn toolCall -> "RunTool " <> toolCall.tcName
    RunThrow _err -> "RunThrow " <> T.pack (show _err)
    RunWorkflow _workflow _i -> "RunWorkflow"

showKont :: Kont m o r -> [Text]
showKont kont =
  case kont of
    KEmpty -> []
    KTool pending _mcid _assistantTurn _toolCalls _toolResults toolCall k -> "KTool " <> toolCall.tcName <> " (" <> showAgent pending <> ")" : showKont k
    KSeq1 _workflow2 _pol k -> "KSeq1" : showKont k
    KPar1 _i _workflow2 _mergePolicy k -> "KPar1" : showKont k
    KPar2 _x _mergePolicy k -> "KPar2" : showKont k
    KMap _pol k -> "KMap" : showKont k
    KLoop _n _workflow _policy _cids k -> "KLoop " <> T.pack (show _n) : showKont k
    KUpdateHistory cid _history k -> "KUpdateHistory " <> T.pack (show cid) : showKont k
    KCatch _o k -> "KCatch" : showKont k
