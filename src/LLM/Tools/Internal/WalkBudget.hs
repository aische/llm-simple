-- | Bounds on the total work of one filesystem tool call: a budget of
-- directory entries a walk may look at, and a byte cap on the lines a tool
-- returns. Shared by the walking and listing tools.
module LLM.Tools.Internal.WalkBudget
  ( WalkBudget,
    newWalkBudget,
    takeEntries,
    budgetExhausted,
    budgetNote,
    capLines,
    utf8Length,
    utf8Prefix,
    textPrefixBytes,
  )
where

import Data.ByteString qualified as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

-- | How many more directory entries a walk may look at, and whether it was
-- denied any.
data WalkBudget = WalkBudget Int (IORef (Int, Bool))

-- | A budget of @n@ entries.
newWalkBudget :: Int -> IO WalkBudget
newWalkBudget n = WalkBudget n <$> newIORef (n, False)

-- | Charge the budget for a directory's entries. Returns the entries the
-- walk may look at: all of them while the budget lasts, then a prefix,
-- then none. Once an entry is refused, 'budgetExhausted' is true.
takeEntries :: WalkBudget -> [a] -> IO [a]
takeEntries (WalkBudget _ ref) xs = atomicModifyIORef' ref $ \(left, cut) ->
  let allowed = take left xs
      n = length allowed
      cut' = cut || n < length (take (left + 1) xs)
   in ((left - n, cut'), allowed)

-- | True once the walk was refused entries because the budget ran out.
budgetExhausted :: WalkBudget -> IO Bool
budgetExhausted (WalkBudget _ ref) = snd <$> readIORef ref

-- | The note a tool adds to its output when the budget ran out.
budgetNote :: WalkBudget -> Text
budgetNote (WalkBudget n _) =
  "stopped after visiting " <> T.pack (show n) <> " entries; narrow the path or pattern"

-- | Keep lines while their UTF-8 size (each plus a newline) stays within
-- @cap@ bytes. Also says whether any line was dropped.
capLines :: Int -> [Text] -> ([Text], Bool)
capLines cap = go 0 []
  where
    go _ acc [] = (reverse acc, False)
    go used acc (l : ls)
      | used + size > cap = (reverse acc, True)
      | otherwise = go (used + size) (l : acc) ls
      where
        size = utf8Length l + 1

-- | Size of a text in UTF-8 bytes.
utf8Length :: Text -> Int
utf8Length = BS.length . encodeUtf8

-- | At most @n@ bytes from the start, without splitting a UTF-8 sequence.
utf8Prefix :: Int -> BS.ByteString -> BS.ByteString
utf8Prefix n bs
  | BS.length bs <= n = bs
  | otherwise = BS.take (boundary n) bs
  where
    -- Back up while the byte at the cut is a continuation byte (10xxxxxx),
    -- so the cut falls before the lead byte of its sequence.
    boundary i
      | i > 0 && BS.index bs i >= 0x80 && BS.index bs i < 0xC0 = boundary (i - 1)
      | otherwise = i

-- | At most @n@ UTF-8 bytes from the start of a text, ending on a whole
-- character.
textPrefixBytes :: Int -> Text -> Text
textPrefixBytes n = decodeUtf8 . utf8Prefix n . encodeUtf8
