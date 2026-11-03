module LLM.Agent.Tools.HistoryTool
  ( HistoryToolArgs (..),
    getHistoryExecTyped,
    historyToolTyped,
    countUserTurns,
    isUserTurn,
    chunkBackward,
    findNthUserBack,
    slice,
    formatChunk,
    formatTurn,
  )
where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Agent.Types (ToolContext (..))
import LLM.Tools.FsLimits (maxOutputBytes)
import LLM.Tools.Internal.WalkBudget (textPrefixBytes, utf8Length)
import LLM.Core.Types
  ( ToolCall (tcName),
    ToolResult (trContent, trName),
    Turn (..),
    TypedTool (..),
    projectReasoning,
    projectText,
    turnToolCalls,
  )

newtype HistoryToolArgs = HistoryToolArgs
  { _historyChunk :: Int
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec HistoryToolArgs)

instance AC.HasCodec HistoryToolArgs where
  codec :: AC.JSONCodec HistoryToolArgs
  codec =
    AC.object "get conversation history" $
      HistoryToolArgs <$> AC.requiredField "chunk" "0 = most recent hidden chunk, 1 = the one before that, etc." AC..= (\x -> x._historyChunk)

getHistoryExecTyped :: ToolContext -> HistoryToolArgs -> IO Text
getHistoryExecTyped ctx args = do
  let chunkIdx = args._historyChunk
      hidden = take ctx.tcWindowOffset ctx.tcConversation
  if null hidden
    then pure "(no earlier history)"
    else do
      let -- Count user messages in the visible window to determine page size
          nUserMessages = countUserTurns (drop ctx.tcWindowOffset ctx.tcConversation)
          -- Chunk the hidden prefix into pages of N user messages each
          chunks = chunkBackward nUserMessages hidden
      if chunkIdx < 0 || chunkIdx >= length chunks
        then pure "(no more history)"
        else pure $ formatChunkWithin maxOutputBytes (chunks !! chunkIdx)

historyToolTyped :: TypedTool ToolContext HistoryToolArgs
historyToolTyped =
  TypedTool
    { ttoolName = "get_history",
      ttoolDescription =
        "Retrieve earlier conversation history that is not in your current context window. "
          <> "Pass chunk=0 for the most recent hidden history, chunk=1 for the one before that, etc. "
          <> "Returns \"(no earlier history)\" when nothing is hidden, and \"(no more history)\" "
          <> "when the chunk index is out of range. A page is at most "
          <> T.pack (show maxOutputBytes)
          <> " bytes: past that its oldest turns are left out (the last line says how many), "
          <> "and a single turn over the cap is cut and marked.",
      ttoolReadonly = True,
      ttoolExecute = getHistoryExecTyped
    }

-- | Count the number of 'UserTurn's in a conversation.
countUserTurns :: [Turn] -> Int
countUserTurns = length . filter isUserTurn

isUserTurn :: Turn -> Bool
isUserTurn (UserMessage _) = True
isUserTurn _ = False

-- | Split a conversation into pages of @n@ user messages each, working
-- backward from the end. Each page starts at a 'UserTurn'.
-- Chunk 0 is the most recent page, chunk 1 the one before, etc.
-- The oldest chunk (highest index) may contain fewer than @n@ user messages.
--
-- When @n <= 0@ (e.g. the visible window has no user turns), paging is
-- disabled and the whole conversation is returned as a single chunk.
-- This avoids an infinite loop in the page walker when @start == end@.
--
-- Turns before the first user message belong to the oldest page, or form a
-- page of their own when the user messages divide into whole pages.
chunkBackward :: Int -> [Turn] -> [[Turn]]
chunkBackward _ [] = []
chunkBackward n conv
  | n <= 0 = [conv]
  | otherwise = reverse (oldestPage ++ map concat (chunksOf newer))
  where
    (leading, rest) = break isUserTurn conv
    -- Each segment is a user message and the turns up to the next one.
    segments = userSegments rest
    (oldest, newer) = splitAt (length segments `mod` n) segments
    oldestPage
      | null oldest = [leading | not (null leading)]
      | otherwise = [leading ++ concat oldest]
    chunksOf [] = []
    chunksOf xs = let (page, more) = splitAt n xs in page : chunksOf more
    userSegments [] = []
    userSegments (t : ts) = let (body, more) = break isUserTurn ts in (t : body) : userSegments more

-- | Find the start index for a page containing @n@ user messages,
-- scanning backward from the end of the given prefix.
-- Returns 0 if fewer than @n@ user messages remain.
findNthUserBack :: Int -> [Turn] -> Int
findNthUserBack n conv = go (reverse (zip [0 ..] conv)) n
  where
    go [] _ = 0
    go ((idx, turn) : older) remaining
      | remaining <= 0 = idx + 1
      | isUserTurn turn = go older (remaining - 1)
      | otherwise = go older remaining

-- | Extract a slice [start, end) from a list.
slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start) . drop start

-- | Format a chunk of conversation turns as readable text.
formatChunk :: [Turn] -> Text
formatChunk = T.intercalate "\n" . map formatTurn

-- | 'formatChunk' within @cap@ UTF-8 bytes. A page over the cap keeps
-- its newest turns and ends with a line saying how many older turns of
-- the page were left out. If the newest turn alone is over the cap, its
-- text is cut and marked instead, so the page is never empty.
formatChunkWithin :: Int -> [Turn] -> Text
formatChunkWithin cap turns
  | utf8Length full <= cap = full
  | otherwise = T.intercalate "\n" (reverse keptNewestFirst ++ note)
  where
    full = formatChunk turns
    rendered = map formatTurn turns
    -- Room for the note and the cut marker.
    budget = max 0 (cap - 256)
    keptNewestFirst = case keepNewest 0 (reverse rendered) of
      [] -> [cutTurn (last rendered)]
      kept -> kept
    dropped = length turns - length keptNewestFirst
    note =
      [ "["
          <> T.pack (show dropped)
          <> " earlier turn(s) of this page left out: the page is over "
          <> T.pack (show cap)
          <> " bytes]"
        | dropped > 0
      ]
    keepNewest _ [] = []
    keepNewest used (l : ls)
      | used + size > budget = []
      | otherwise = l : keepNewest (used + size) ls
      where
        size = utf8Length l + 1
    cutTurn l =
      textPrefixBytes budget l
        <> " [turn cut: "
        <> T.pack (show (utf8Length l))
        <> " bytes, first "
        <> T.pack (show budget)
        <> " shown]"

formatTurn :: Turn -> Text
formatTurn (UserMessage parts) =
  "[User] " <> projectText parts
formatTurn (AssistantMessage parts) =
  let t = projectText parts
      mReasoning = projectReasoning parts
      calls = turnToolCalls parts
   in "[Assistant] "
        <> t
        <> maybe "" (\r -> " [reasoning: " <> T.take 200 r <> "]") mReasoning
        <> if null calls
          then ""
          else " [called: " <> T.intercalate ", " (map (\x -> x.tcName) calls) <> "]"
formatTurn (ToolTurn results) =
  "[Tool results] "
    <> T.intercalate ", " [r.trName <> ": " <> T.take 200 r.trContent | r <- results]

-- parseChunk :: Value -> Parser Int
-- parseChunk = withObject "args" (.: "chunk")
