module LLM.Tools.MoveFile (moveFileToolTyped) where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, sandboxPath, sandboxWritePath)
import System.Directory (renameFile)

data MoveFileToolArgs = MoveFileToolArgs
  { _mfSrc :: Text,
    _mfDst :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec MoveFileToolArgs)

instance AC.HasCodec MoveFileToolArgs where
  codec =
    AC.object "move source to destination" $
      MoveFileToolArgs
        <$> AC.requiredField "source" "Relative path of the file to move" AC..= _mfSrc
        <*> AC.requiredField "destination" "Relative destination path (including filename)" AC..= _mfDst

moveFileToolTyped :: FsConfig -> TypedTool ctx MoveFileToolArgs
moveFileToolTyped cfg =
  TypedTool
    { ttoolName = "move_file",
      ttoolDescription =
        "Move (rename) a file from source to destination (both paths relative to the workspace). "
          <> "Creates parent directories at the destination as needed.",
      ttoolReadonly = False,
      ttoolExecute = const (moveFileExecTyped cfg)
    }

moveFileExecTyped :: FsConfig -> MoveFileToolArgs -> IO Text
moveFileExecTyped cfg args = do
  let src = _mfSrc args
      dst = _mfDst args
  srcResolved <- sandboxPath cfg (T.unpack src)
  dstResolved <- sandboxWritePath cfg (T.unpack dst)
  renameFile srcResolved dstResolved
  pure $ "Successfully moved " <> src <> " to " <> dst
