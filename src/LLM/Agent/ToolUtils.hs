module LLM.Agent.ToolUtils
  ( executeTool,
    executeTools,
    executeToolsWithAbort,
    ToolRoundOutcome (..),
    createToolContext,
    getSchema,
    toTool,
    filterReadonlyTools,
    getResolvedTools,
    windowOffset,
    createGenRequest,
    createGenRequestNoTools,
    embedTextTool,
    safeRuntimeArgs,
    validateAgent,
  )
where

import Autodocodec qualified as AC
import Autodocodec.Schema (jsonSchemaVia)
import Control.Exception (IOException, SomeException, fromException)
import Control.Monad (mfilter)
import Data.Containers.ListUtils (nubOrd)
import Data.List (nub)
import Data.Aeson (FromJSON)
import Data.Aeson qualified as AE
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Agent.Tools.HistoryTool (historyToolTyped)
import LLM.Agent.Types
  ( Agent (..),
    RuntimeArgs (..),
    Tool (Tool, toolDef, toolExecute),
    ToolContext (..),
    ToolMap,
  )
import LLM.Core.Abort (AbortSignal, isAborted)
import LLM.Core.Types
  ( ToolCall (..),
    ToolDef (..),
    ToolResult (..),
    Turn (..),
    TypedTool (TypedTool),
  )
import LLM.Core.Usage (Usage)
import LLM.Core.Utils (interruptOnAbort, toolResult, trySync)
import LLM.Generate.Logger (Hooks (..), safeHooks, safeLLMHooks)
import LLM.Generate.Types (GenRequest (..))
import LLM.Tools.FsConfig (SandboxViolation, formatSandboxViolation)
import GHC.IO.Exception (IOException (ioe_filename))
import System.FilePath (isRelative)

-- | Execute a single tool call by looking it up in the tool list
executeTool :: Hooks -> ToolContext -> [Tool Text] -> ToolCall -> IO ToolResult
executeTool hooks ctx tools tc =
  -- Without a signal the call cannot be interrupted, so the default is unused.
  fromMaybe (toolResult tc "Aborted") <$> executeToolInterruptible Nothing hooks ctx tools tc

-- | Like 'executeTool', but the tool is interrupted when the signal fires.
-- Returns 'Nothing' if it was; 'onToolError' then reports @\"Aborted\"@.
executeToolInterruptible :: Maybe AbortSignal -> Hooks -> ToolContext -> [Tool Text] -> ToolCall -> IO (Maybe ToolResult)
executeToolInterruptible mSig hooks ctx tools tc = case lookup tc.tcName toolMap of
  Nothing -> do
    let msg = "Unknown tool: " <> tc.tcName
    hooks.onToolError tc.tcName msg
    pure $ Just $ toolResult tc msg
  Just tool -> do
    if ctx.tcRuntimeArgs.rtReadonly && not tool.toolDef.toolReadonly
      then do
        let msg = "Error: tool is not available in readonly mode: " <> tc.tcName
        hooks.onToolError tc.tcName msg
        pure $ Just $ toolResult tc msg
      else do
        hooks.onToolCall tc.tcName (AE.toJSON tc.tcArguments)
        result <- interruptOnAbort mSig (trySync (tool.toolExecute ctx tc.tcArguments))
        case result of
          Nothing -> do
            hooks.onToolError tc.tcName "Aborted"
            pure Nothing
          Just (Right text) -> do
            hooks.onToolResult tc.tcName text
            pure $ Just $ toolResult tc text
          Just (Left (e :: SomeException)) -> do
            let msg = formatToolException e
            hooks.onToolError tc.tcName msg
            pure $ Just $ toolResult tc ("Tool error: " <> msg)
  where
    toolMap = [(t.toolDef.toolName, t) | t <- tools]

-- | Execute all tool calls from a response
executeTools :: Hooks -> ToolContext -> [Tool Text] -> [ToolCall] -> IO [ToolResult]
executeTools hooks ctx tools = mapM (executeTool hooks ctx tools)

-- | Outcome of a tool round run by 'executeToolsWithAbort'.
data ToolRoundOutcome
  = -- | Every call ran.
    ToolRoundCompleted [ToolResult]
  | -- | The abort signal fired: results of the calls that finished, and the
    -- calls that did not. The first of those may have been interrupted while
    -- running, so its side effects may have partly happened.
    ToolRoundAborted [ToolResult] [ToolCall]
  deriving (Show, Eq)

-- | Execute tool calls one at a time. The abort signal is checked before
-- each call, and a call that is running when it fires is interrupted.
executeToolsWithAbort :: Maybe AbortSignal -> Hooks -> ToolContext -> [Tool Text] -> [ToolCall] -> IO ToolRoundOutcome
executeToolsWithAbort Nothing hooks ctx tools tcs = ToolRoundCompleted <$> executeTools hooks ctx tools tcs
executeToolsWithAbort (Just sig) hooks ctx tools tcs = go [] tcs
  where
    go acc [] = pure (ToolRoundCompleted (reverse acc))
    go acc remaining@(tc : rest) = do
      aborted <- isAborted sig
      r <-
        if aborted
          then pure Nothing
          else executeToolInterruptible (Just sig) hooks ctx tools tc
      case r of
        Nothing -> pure (ToolRoundAborted (reverse acc) remaining)
        Just res -> go (res : acc) rest

createToolContext ::
  Agent ->
  [Turn] ->
  Usage ->
  RuntimeArgs ->
  ToolContext
createToolContext agent messages roundUsage rt =
  ToolContext
    { tcConversation = messages,
      tcUsage = roundUsage,
      tcWindowOffset = windowOffset agent.agContextWindow messages,
      tcRuntimeArgs = rt
    }

getSchema :: (AC.HasCodec t, FromJSON t) => m ToolContext t -> AC.JSONCodec t
getSchema _ = AC.codec

-- | Convert a 'TypedTool' into a 'Tool' with JSON Schema parameters.
--
-- The executor receives parsed arguments (Autodocodec 'FromJSON'); invalid
-- JSON from the model returns an error string to the model without throwing.
toTool :: (AC.HasCodec args, FromJSON args) => TypedTool ToolContext args -> Tool Text
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

filterReadonlyTools :: Bool -> [Tool result] -> [Tool result]
filterReadonlyTools False tools = tools
filterReadonlyTools True tools = filter (\x -> x.toolDef.toolReadonly) tools

-- | Compute the index where the visible window starts.
-- The window includes the last @n@ user messages and all turns that follow
-- each of them (assistant replies, tool rounds, etc.).
-- Returns 0 (no windowing) when the window is 'Nothing' or @'Just' n@ with
-- @n <= 0@, or when the conversation contains fewer than @n@ user messages.
windowOffset :: Maybe Int -> [Turn] -> Int
windowOffset Nothing _ = 0
windowOffset (Just n) conv = findNthUserFromEnd n conv

-- | Find the index of the Nth 'UserTurn' from the end of a conversation.
-- Returns 0 if there are fewer than @n@ user messages, or if @n <= 0@.
findNthUserFromEnd :: Int -> [Turn] -> Int
findNthUserFromEnd n conv
  | n <= 0 = 0
  | otherwise = go (length conv - 1) n
  where
    go idx remaining
      | idx < 0 = 0
      | remaining <= 0 = idx + 1
      | otherwise = case conv !! idx of
          UserMessage _ -> go (idx - 1) (remaining - 1)
          _ -> go (idx - 1) remaining

-- | Build a 'GenRequest' from agent configuration and runtime state.
--
-- Applies context windowing ('windowOffset'), resolves tools from the
-- 'ToolMap' (including auto-injected @get_history@ when windowed), and
-- respects 'RuntimeArgs.rtReadonly' when selecting tool definitions.
createGenRequest :: (Text -> result) -> Agent -> ToolMap result -> RuntimeArgs -> [Turn] -> GenRequest
createGenRequest embed agent toolMap rt messages =
  let offset = windowOffset agent.agContextWindow messages
      tools = getResolvedTools embed agent toolMap rt
   in GenRequest
        { grSystemPrompt = agent.agSystemPrompt,
          grTools = map (\x -> x.toolDef) tools,
          grMessages = drop offset messages,
          grAbortSignal = rt.rtAbortSignal,
          grLLMHooks = rt.rtLLMHooks,
          grHooks = rt.rtHooks
        }

-- | Like 'createGenRequest', but never attaches tool definitions.
--
-- Still applies 'agContextWindow' truncation to 'grMessages'. Use this for
-- structured-output paths ('generateObject' / 'generateObjectUntyped'), which
-- cannot execute tools and must not advertise @get_history@.
createGenRequestNoTools :: Agent -> RuntimeArgs -> [Turn] -> GenRequest
createGenRequestNoTools agent rt messages =
  let offset = windowOffset agent.agContextWindow messages
   in GenRequest
        { grSystemPrompt = agent.agSystemPrompt,
          grTools = [],
          grMessages = drop offset messages,
          grAbortSignal = rt.rtAbortSignal,
          grLLMHooks = rt.rtLLMHooks,
          grHooks = rt.rtHooks
        }

-- | The tools sent to the model: the 'Agent.agTools' names found in the
-- 'ToolMap', each once (the first occurrence counts), read-only ones only
-- when 'rtReadonly' is set, followed by the built-in @get_history@ when
-- windowing is on. A caller tool named @get_history@ is dropped while
-- windowing is on. Names missing from the map are skipped;
-- 'validateAgent' reports them.
getResolvedTools :: (Text -> result) -> Agent -> ToolMap result -> RuntimeArgs -> [Tool result]
getResolvedTools embed agent toolMap rt =
  filterReadonlyTools rt.rtReadonly (getToolsFromMap toolMap callerNames)
    ++ fmap (embedTextTool embed) historyTools
  where
    historyTools = getHistoryTool agent
    callerNames
      | null historyTools = agent.agTools
      | otherwise = filter (/= historyToolName) agent.agTools

getToolsFromMap :: ToolMap result -> [Text] -> [Tool result]
getToolsFromMap toolMap toolNames = nubOrd toolNames >>= lookupTool
  where
    lookupTool name = case Map.lookup name toolMap of
      Just tool -> [tool]
      Nothing -> []

getHistoryTool :: Agent -> [Tool Text]
getHistoryTool agent
  | windowingOn agent = [toTool historyToolTyped]
  | otherwise = []

windowingOn :: Agent -> Bool
windowingOn agent = case agent.agContextWindow of
  Just n -> n > 0
  Nothing -> False

historyToolName :: Text
historyToolName = "get_history"

-- | Warnings about an agent configuration that the agent loop works around:
--
-- * a name listed more than once in 'agTools' (only the first counts);
-- * a name in 'agTools' that the 'ToolMap' lacks (it is not sent);
-- * a caller tool named @get_history@ while windowing is on (the built-in
--   one is sent instead);
-- * @'agContextWindow' = 'Just' n@ with @n <= 0@ (no windowing).
--
-- 'LLM.Agent.generateText' and 'LLM.Agent.streamText' send each warning
-- to 'onLog' at 'LLM.Generate.Logger.Warn' once at the start of a run.
-- An empty list means none apply.
validateAgent :: Agent -> ToolMap result -> [Text]
validateAgent agent toolMap =
  [ "agTools lists " <> name <> " more than once; only the first is used"
  | name <- nub (duplicates agent.agTools)
  ]
    ++ [ "agTools lists " <> name <> ", which is not in the tool map; it is not sent to the model"
       | name <- nubOrd agent.agTools,
         not (Map.member name toolMap),
         not (name == historyToolName && windowingOn agent)
       ]
    ++ [ "agTools lists " <> historyToolName <> ", which is built in while agContextWindow is set; the caller's tool of that name is not used"
       | windowingOn agent,
         historyToolName `elem` agent.agTools,
         Map.member historyToolName toolMap
       ]
    ++ [ "agContextWindow = Just " <> T.pack (show n) <> " is not positive; the whole conversation is sent"
       | Just n <- [agent.agContextWindow],
         n <= 0
       ]
  where
    duplicates names = [n | (i, n) <- zip [0 :: Int ..] names, n `elem` take i names]

embedTextTool :: (Text -> result) -> Tool Text -> Tool result
embedTextTool embed tool =
  tool
    { toolExecute = \ctx args -> do
        result <- tool.toolExecute ctx args
        pure $ embed result
    }

-- | Model-facing text for an exception a tool threw.
--
-- A 'SandboxViolation' shows its path relative to the workspace. An
-- 'IOException' keeps its file name only if that is relative: the bundled
-- filesystem tools make their paths workspace-relative
-- ('LLM.Tools.FsConfig.withWorkspacePaths'), and an absolute path, from
-- any tool, is dropped so the host's directory layout does not reach the
-- model. The operation and the error description are kept.
formatToolException :: SomeException -> Text
formatToolException e
  | Just (sv :: SandboxViolation) <- fromException e = formatSandboxViolation sv
  | Just (ioe :: IOException) <- fromException e =
      T.pack (show ioe {ioe_filename = mfilter isRelative (ioe_filename ioe)})
  | otherwise = T.pack (show e)

-- | The runtime arguments with their hooks wrapped by 'safeHooks' and
-- 'safeLLMHooks', so an exception thrown by a hook cannot fail a run whose
-- side effects (a tool call, say) already happened. Applied by the agent
-- entry points.
safeRuntimeArgs :: RuntimeArgs -> RuntimeArgs
safeRuntimeArgs rt =
  rt
    { rtHooks = safeHooks rt.rtHooks,
      rtLLMHooks = safeLLMHooks rt.rtLLMHooks
    }
