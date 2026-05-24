module LLM.Tools.ReplaceInFile (replaceInFileToolTyped) where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, sandboxPath)

data ReplaceInFileToolArgs = ReplaceInFileToolArgs
  { _rifPath :: Text,
    _rifOld :: Text,
    _rifNew :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec ReplaceInFileToolArgs)

instance AC.HasCodec ReplaceInFileToolArgs where
  codec :: AC.JSONCodec ReplaceInFileToolArgs
  codec =
    AC.object "replace a string in a file" $
      ReplaceInFileToolArgs
        <$> AC.requiredField "path" "Relative file path to write to" AC..= _rifPath
        <*> AC.requiredField "old" "The text content to be replaced in the file" AC..= _rifOld
        <*> AC.requiredField "new" "The replacement text content to write to the file" AC..= _rifNew

replaceInFileToolTyped :: FsConfig -> TypedTool ctx ReplaceInFileToolArgs
replaceInFileToolTyped cfg =
  TypedTool
    { ttoolName = "replace_in_file",
      ttoolDescription =
        "Replace the first occurrence of a string in a file. "
          <> "The 'old' string must appear exactly once in the file. "
          <> "Returns an error if the string is not found or appears more than once.",
      ttoolReadonly = False,
      ttoolExecute = const (replaceExecTyped cfg)
    }

replaceExecTyped :: FsConfig -> ReplaceInFileToolArgs -> IO Text
replaceExecTyped cfg args = do
  let ReplaceInFileToolArgs {_rifPath, _rifOld, _rifNew} = args
      p = _rifPath
  resolved <- sandboxPath cfg (T.unpack p)
  content <- TIO.readFile resolved
  let occurrences = countOccurrences _rifOld content
  case occurrences of
    0 -> pure "Error: the 'old' string was not found in the file"
    1 -> do
      let replaced = replaceFirst _rifOld _rifNew content
      TIO.writeFile resolved replaced
      pure $ "Successfully replaced text in " <> p
    n ->
      pure $
        "Error: the 'old' string was found "
          <> T.pack (show n)
          <> " times; it must appear exactly once"

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
