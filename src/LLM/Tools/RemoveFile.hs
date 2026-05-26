module LLM.Tools.RemoveFile (removeFileToolTyped, RemoveFileToolArgs (..)) where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, sandboxPath)
import System.Directory (removeFile)

newtype RemoveFileToolArgs = RemoveFileToolArgs
  { _rftPath :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec RemoveFileToolArgs)

instance AC.HasCodec RemoveFileToolArgs where
  codec =
    AC.object "remove a file" $
      RemoveFileToolArgs <$> AC.requiredField "path" "Relative path of the file to remove" AC..= _rftPath

removeFileToolTyped :: FsConfig -> TypedTool ctx RemoveFileToolArgs
removeFileToolTyped cfg =
  TypedTool
    { ttoolName = "remove_file",
      ttoolDescription =
        "Delete a file at the given path (relative to the workspace). "
          <> "Fails if the path does not exist or is a directory.",
      ttoolReadonly = False,
      ttoolExecute = const (removeFileExecTyped cfg)
    }

removeFileExecTyped :: FsConfig -> RemoveFileToolArgs -> IO Text
removeFileExecTyped cfg args = do
  let p = _rftPath args
  resolved <- sandboxPath cfg (T.unpack p)
  removeFile resolved
  pure $ "Successfully removed " <> p
