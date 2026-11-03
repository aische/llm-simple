-- | Text edits shared by @replace_in_file@ and @multi_replace_in_file@.
module LLM.Tools.Internal.TextEdit
  ( countOccurrences,
    replaceFirst,
  )
where

import Data.Text (Text)
import Data.Text qualified as T

-- | Count non-overlapping occurrences of needle in haystack.
countOccurrences :: Text -> Text -> Int
countOccurrences needle haystack
  | T.null needle = 0
  | otherwise = go 0 haystack
  where
    go !n hay = case T.breakOn needle hay of
      (_, rest)
        | T.null rest -> n
        | otherwise -> go (n + 1) (T.drop (T.length needle) rest)

-- | Replace the first occurrence of needle with replacement.
replaceFirst :: Text -> Text -> Text -> Text
replaceFirst needle replacement haystack =
  let (before, after) = T.breakOn needle haystack
   in before <> replacement <> T.drop (T.length needle) after
