module LLM.Agent.ToolUtils
  ( executeTool,
    executeTools,
    executeToolsWithAbort,
    getSchema,
    toTool,
    filterReadonlyTools,
    windowOffset,
    createGenRequest,
  )
where

import Autodocodec qualified as AC
import Autodocodec.Schema (jsonSchemaVia)
import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON)
import Data.Aeson qualified as AE
import Data.Text qualified as T
import LLM.Agent.Types
  ( Agent (..),
    RuntimeArgs (..),
    Tool (Tool, toolDef, toolExecute),
    ToolContext,
  )
import LLM.Core.Abort (AbortSignal, isAborted)
import LLM.Core.Types
  ( ToolCall (..),
    ToolDef (..),
    ToolResult (..),
    Turn (..),
    TypedTool (TypedTool),
  )
import LLM.Core.Utils (toolResult)
import LLM.Generate.Logger (Hooks (..))
import LLM.Generate.Types
  ( GenRequest (..),
    GenerateError (..),
    GenerateResult,
  )

-- | Execute a single tool call by looking it up in the tool list
executeTool :: Hooks -> ToolContext -> [Tool] -> ToolCall -> IO ToolResult
executeTool hooks ctx tools tc = case lookup (tcName tc) toolMap of
  Nothing -> pure $ toolResult tc ("Unknown tool: " <> tcName tc)
  Just exec -> do
    onToolCall hooks (tcName tc) (AE.toJSON (tcArguments tc))
    result <- try (exec ctx (tcArguments tc))
    case result of
      Right text -> do
        onToolResult hooks (tcName tc) text
        pure $ toolResult tc text
      Left (e :: SomeException) -> do
        onToolError hooks (tcName tc) (T.pack (show e))
        pure $ toolResult tc ("Tool error: " <> T.pack (show e))
  where
    toolMap = [(toolName (toolDef t), toolExecute t) | t <- tools]

-- | Execute all tool calls from a response
executeTools :: Hooks -> ToolContext -> [Tool] -> [ToolCall] -> IO [ToolResult]
executeTools hooks ctx tools = mapM (executeTool hooks ctx tools)

-- | Execute tool calls one at a time, checking the abort signal between each.
-- Returns @Left Aborted@ if the signal fires before all calls finish.
executeToolsWithAbort :: Maybe AbortSignal -> Hooks -> ToolContext -> [Tool] -> [ToolCall] -> IO (GenerateResult [ToolResult])
executeToolsWithAbort Nothing hooks ctx tools tcs = Right <$> executeTools hooks ctx tools tcs
executeToolsWithAbort (Just sig) hooks ctx tools tcs = go [] tcs
  where
    go acc [] = pure (Right (reverse acc))
    go acc (tc : rest) = do
      aborted <- isAborted sig
      if aborted
        then pure (Left GErrAborted)
        else do
          r <- executeTool hooks ctx tools tc
          go (r : acc) rest

getSchema :: (AC.HasCodec t, FromJSON t) => TypedTool ToolContext t -> AC.JSONCodec t
getSchema _ = AC.codec

toTool :: (AC.HasCodec t, FromJSON t) => TypedTool ToolContext t -> Tool
toTool t@(TypedTool name descr readonly exec) =
  Tool
    { toolDef =
        ToolDef
          { toolName = name,
            toolDescription = descr,
            toolReadonly = readonly,
            toolParameters = AE.toJSON $ jsonSchemaVia $ getSchema t
          },
      toolExecute = \ctx argsvalue ->
        case AE.fromJSON argsvalue of
          AE.Error e -> pure $ "Error: Parsing arguments failed " <> T.pack (show e)
          AE.Success args -> exec ctx args
    }

filterReadonlyTools :: Bool -> [Tool] -> [Tool]
filterReadonlyTools False tools = tools
filterReadonlyTools True tools = filter (toolReadonly . toolDef) tools

-- | Compute the index where the visible window starts.
-- The window includes the last @n@ user messages and all turns that follow
-- each of them (assistant replies, tool rounds, etc.).
-- Returns 0 (no windowing) when the window is 'Nothing' or the conversation
-- contains fewer than @n@ user messages.
windowOffset :: Maybe Int -> [Turn] -> Int
windowOffset Nothing _ = 0
windowOffset (Just n) conv = findNthUserFromEnd n conv

-- | Find the index of the Nth 'UserTurn' from the end of a conversation.
-- Returns 0 if there are fewer than @n@ user messages.
findNthUserFromEnd :: Int -> [Turn] -> Int
findNthUserFromEnd 0 _conv = 0
findNthUserFromEnd n conv = go (length conv - 1) n
  where
    go idx remaining
      | idx < 0 = 0
      | remaining <= 0 = idx + 1
      | otherwise = case conv !! idx of
          UserTurn _ -> go (idx - 1) (remaining - 1)
          _ -> go (idx - 1) remaining

createGenRequest :: Agent -> RuntimeArgs -> [Turn] -> GenRequest
createGenRequest agent rt messages =
  let offset = windowOffset (agContextWindow agent) messages
   in GenRequest
        { grSystemPrompt = agSystemPrompt agent,
          grTools = map toolDef $ filterReadonlyTools (rtReadonly rt) (agTools agent),
          grMessages = drop offset messages,
          grAbortSignal = rtAbortSignal rt,
          grLLMHooks = rtLLMHooks rt,
          grHooks = rtHooks rt
        }
