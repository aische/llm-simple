module LLM.Tools.Readdir (readdirToolTyped, ReaddirToolArgs (..)) where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, isFileHidden, isSymlink, sandboxPath)
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
          <> "Use path '.' to list the workspace root.",
      ttoolReadonly = True,
      ttoolExecute = const (readdirExecTyped fsConfig)
    }

readdirExecTyped :: FsConfig -> ReaddirToolArgs -> IO Text
readdirExecTyped cfg args = do
  let relPath = T.unpack args._rdPath
  resolved <- sandboxPath cfg relPath
  entries <- sort <$> listDirectory resolved
  annotated <- mapM (annotateEntry resolved) $ filter (not . isFileHidden) entries
  pure $ T.intercalate "\n" annotated

-- | Annotate a directory entry without following symlinks.
-- Symbolic links are tagged with '@' and never treated as directories,
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
