module LLM.Tools.RemoveDirectory (removeDirectoryToolTyped, RemoveDirectoryToolArgs (..)) where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, sandboxPath)
import System.Directory (removeDirectoryRecursive)

newtype RemoveDirectoryToolArgs = RemoveDirectoryToolArgs
  { _rdPath :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec RemoveDirectoryToolArgs)

instance AC.HasCodec RemoveDirectoryToolArgs where
  codec =
    AC.object "remove a directory" $
      RemoveDirectoryToolArgs
        <$> AC.requiredField "path" "Relative path of the directory to remove" AC..= (\x -> x._rdPath)

removeDirectoryToolTyped :: FsConfig -> TypedTool ctx RemoveDirectoryToolArgs
removeDirectoryToolTyped cfg =
  TypedTool
    { ttoolName = "remove_directory",
      ttoolDescription =
        "Recursively delete a directory and all its contents at the given path (relative to the workspace). "
          <> "Use with caution — this operation is irreversible.",
      ttoolReadonly = False,
      ttoolExecute = const (removeDirectoryExecTyped cfg)
    }

removeDirectoryExecTyped :: FsConfig -> RemoveDirectoryToolArgs -> IO Text
removeDirectoryExecTyped cfg args = do
  let p = args._rdPath
  resolved <- sandboxPath cfg (T.unpack p)
  removeDirectoryRecursive resolved
  pure $ "Successfully removed directory " <> p
