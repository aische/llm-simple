module LLM.Tools.RemoveFile (removeFileToolTyped, RemoveFileToolArgs (..)) where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, isSandboxRoot, sandboxPath, sandboxSymlink)
import System.Directory (doesDirectoryExist, removeFile)

newtype RemoveFileToolArgs = RemoveFileToolArgs
  { _rftPath :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec RemoveFileToolArgs)

instance AC.HasCodec RemoveFileToolArgs where
  codec =
    AC.object "remove a file" $
      RemoveFileToolArgs <$> AC.requiredField "path" "Relative path of the file to remove" AC..= (._rftPath)

removeFileToolTyped :: FsConfig -> TypedTool ctx RemoveFileToolArgs
removeFileToolTyped cfg =
  TypedTool
    { ttoolName = "remove_file",
      ttoolDescription =
        "Delete a file at the given path (relative to the workspace). "
          <> "Fails if the path does not exist or is a directory. "
          <> "A symbolic link is removed itself; its target is left unchanged.",
      ttoolReadonly = False,
      ttoolExecute = const (removeFileExecTyped cfg)
    }

removeFileExecTyped :: FsConfig -> RemoveFileToolArgs -> IO Text
removeFileExecTyped cfg args = do
  let p = args._rftPath
  link <- sandboxSymlink cfg (T.unpack p)
  case link of
    Just l -> do
      removeFile l
      pure $ "Successfully removed symbolic link " <> p <> " (its target was left unchanged)"
    Nothing -> do
      resolved <- sandboxPath cfg (T.unpack p)
      isDir <- doesDirectoryExist resolved
      if isSandboxRoot cfg resolved
        then pure $ "Error: refusing to remove the workspace root: " <> p
        else
          if isDir
            then pure $ "Error: path is a directory, not a file (use remove_directory): " <> p
            else do
              removeFile resolved
              pure $ "Successfully removed " <> p
