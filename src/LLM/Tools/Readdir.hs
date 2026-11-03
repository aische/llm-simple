module LLM.Tools.Readdir (readdirToolTyped, ReaddirToolArgs (..)) where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, isFileHidden, isSymlink, sandboxPath, withWorkspacePaths)
import LLM.Tools.FsLimits (maxOutputBytes, maxTreeEntries)
import LLM.Tools.Internal.WalkBudget (capLines)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

newtype ReaddirToolArgs = ReaddirToolArgs
  { _rdPath :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec ReaddirToolArgs)

instance AC.HasCodec ReaddirToolArgs where
  codec :: AC.JSONCodec ReaddirToolArgs
  codec =
    AC.object "list a directory" $
      ReaddirToolArgs <$> AC.requiredField "path" "Relative directory path to list" AC..= (._rdPath)

readdirToolTyped :: FsConfig -> TypedTool ctx ReaddirToolArgs
readdirToolTyped fsConfig =
  TypedTool
    { ttoolName = "readdir",
      ttoolDescription =
        "List the contents of a directory (relative to the workspace). "
          <> "Returns one entry per line. Directories are suffixed with '/', "
          <> "symbolic links with '@' (links are not followed). "
          <> "Use path '.' to list the workspace root. "
          <> "At most "
          <> T.pack (show maxTreeEntries)
          <> " entries are listed; a final line says if more were left out.",
      ttoolReadonly = True,
      ttoolExecute = const (withWorkspacePaths fsConfig . readdirExecTyped fsConfig)
    }

readdirExecTyped :: FsConfig -> ReaddirToolArgs -> IO Text
readdirExecTyped cfg args = do
  let relPath = T.unpack args._rdPath
  resolved <- sandboxPath cfg relPath
  visible <- sort . filter (not . isFileHidden) <$> listDirectory resolved
  annotated <- mapM (annotateEntry resolved) (take maxTreeEntries visible)
  -- Leave room for the note line.
  let (shown, _) = capLines (maxOutputBytes - 200) annotated
      total = length visible
      note
        | length shown < total =
            [ "[showing "
                <> T.pack (show (length shown))
                <> " of "
                <> T.pack (show total)
                <> " entries; use find_files to search the rest]"
            ]
        | otherwise = []
  pure $ T.intercalate "\n" (shown ++ note)

-- | Annotate a directory entry without following symlinks.
-- Symbolic links are tagged with @\@@ and never treated as directories,
-- even if they happen to point at one (possibly outside the sandbox).
annotateEntry :: FilePath -> FilePath -> IO Text
annotateEntry parent name = do
  let full = parent </> name
  link <- isSymlink full
  if link
    then pure $ T.pack name <> "@"
    else do
      isDir <- doesDirectoryExist full
      pure $
        if isDir
          then T.pack name <> "/"
          else T.pack name
