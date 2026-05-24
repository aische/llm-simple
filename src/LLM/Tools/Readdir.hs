module LLM.Tools.Readdir (readdirToolTyped) where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, isFileHidden, sandboxPath)
import System.Directory (doesDirectoryExist, listDirectory)

newtype ReaddirToolArgs = ReaddirToolArgs
  { _rdPath :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec ReaddirToolArgs)

instance AC.HasCodec ReaddirToolArgs where
  codec :: AC.JSONCodec ReaddirToolArgs
  codec =
    AC.object "list a directory" $
      ReaddirToolArgs <$> AC.requiredField "path" "Relative directory path to list" AC..= _rdPath

readdirToolTyped :: FsConfig -> TypedTool ctx ReaddirToolArgs
readdirToolTyped fsConfig =
  TypedTool
    { ttoolName = "read_dir",
      ttoolDescription =
        "List the contents of a directory (relative to the workspace). "
          <> "Returns one entry per line. Directories are suffixed with '/'. "
          <> "Use path '.' or omit it to list the workspace root.",
      ttoolReadonly = True,
      ttoolExecute = const (readdirExecTyped fsConfig)
    }

readdirExecTyped :: FsConfig -> ReaddirToolArgs -> IO Text
readdirExecTyped cfg args = do
  let relPath = T.unpack $ _rdPath args
  resolved <- sandboxPath cfg relPath
  entries <- sort <$> listDirectory resolved
  annotated <- mapM (annotateEntry resolved) $ filter (not . isFileHidden) entries
  pure $ T.intercalate "\n" annotated

annotateEntry :: FilePath -> FilePath -> IO Text
annotateEntry parent name = do
  isDir <- doesDirectoryExist (parent <> "/" <> name)
  pure $
    if isDir
      then T.pack name <> "/"
      else T.pack name
