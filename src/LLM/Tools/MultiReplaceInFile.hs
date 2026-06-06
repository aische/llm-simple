module LLM.Tools.MultiReplaceInFile (multiReplaceInFileToolTyped, MultiReplaceInFileToolArgs (..)) where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, sandboxPath)

data Replacement = Replacement
  { _repOld :: Text,
    _repNew :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec Replacement)

instance AC.HasCodec Replacement where
  codec :: AC.JSONCodec Replacement
  codec =
    AC.object "Replacement" $
      Replacement
        <$> AC.requiredField "old" "The text content to be replaced" AC..= (._repOld)
        <*> AC.requiredField "new" "The replacement text content" AC..= (._repNew)

data MultiReplaceInFileToolArgs = MultiReplaceInFileToolArgs
  { _mrifPath :: Text,
    _mrifReplacements :: [Replacement]
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec MultiReplaceInFileToolArgs)

instance AC.HasCodec MultiReplaceInFileToolArgs where
  codec :: AC.JSONCodec MultiReplaceInFileToolArgs
  codec =
    AC.object "apply multiple replacements to a file" $
      MultiReplaceInFileToolArgs
        <$> AC.requiredField "path" "Relative file path to edit" AC..= (._mrifPath)
        <*> AC.requiredField "replacements" "List of replacements to apply in order" AC..= (._mrifReplacements)

multiReplaceInFileToolTyped :: FsConfig -> TypedTool ctx MultiReplaceInFileToolArgs
multiReplaceInFileToolTyped cfg =
  TypedTool
    { ttoolName = "multi_replace_in_file",
      ttoolDescription =
        "Apply multiple text replacements to a file in order. "
          <> "Each 'old' string must appear exactly once in the file at the time it is applied. "
          <> "Returns an error if any string is not found or appears more than once.",
      ttoolReadonly = False,
      ttoolExecute = const (replaceExecTyped cfg)
    }

replaceExecTyped :: FsConfig -> MultiReplaceInFileToolArgs -> IO Text
replaceExecTyped cfg args = do
  let MultiReplaceInFileToolArgs {_mrifPath, _mrifReplacements} = args
  resolved <- sandboxPath cfg (T.unpack _mrifPath)
  content <- TIO.readFile resolved
  case applyReplacements _mrifReplacements content of
    Left err -> pure err
    Right result -> do
      TIO.writeFile resolved result
      pure $ "Successfully applied " <> T.pack (show (length _mrifReplacements)) <> " replacement(s) in " <> _mrifPath

-- | Apply a list of replacements sequentially, failing on the first error.
applyReplacements :: [Replacement] -> Text -> Either Text Text
applyReplacements [] content = Right content
applyReplacements (Replacement {_repOld, _repNew} : rest) content =
  case countOccurrences _repOld content of
    0 -> Left $ "Error: 'old' string not found in file: " <> _repOld
    1 -> applyReplacements rest (replaceFirst _repOld _repNew content)
    n ->
      Left $
        "Error: 'old' string found "
          <> T.pack (show n)
          <> " times (must appear exactly once): "
          <> _repOld

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
