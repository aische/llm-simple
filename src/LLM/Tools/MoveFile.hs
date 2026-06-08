module LLM.Tools.MoveFile (moveFileToolTyped, MoveFileToolArgs (..)) where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, isSymlink, sandboxPath, sandboxWritePath)
import System.Directory (doesDirectoryExist, doesFileExist, doesPathExist, renameFile)

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
        <$> AC.requiredField "source" "Relative path of the file to move" AC..= (._mfSrc)
        <*> AC.requiredField "destination" "Relative destination path (including filename)" AC..= (._mfDst)

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
  let src = args._mfSrc
      dst = args._mfDst
  srcResolved <- sandboxPath cfg (T.unpack src)
  -- Refuse to operate on symbolic-link sources: this would either move
  -- the link itself (surprising) or, after `sandboxPath` resolution,
  -- potentially target a file outside the sandbox.
  srcIsLink <- isSymlink srcResolved
  if srcIsLink
    then pure $ "Error: refusing to move symbolic link source: " <> src
    else do
      srcExists <- doesPathExist srcResolved
      if not srcExists
        then pure $ "Error: source does not exist: " <> src
        else do
          srcIsFile <- doesFileExist srcResolved
          srcIsDir <- doesDirectoryExist srcResolved
          case (srcIsFile, srcIsDir) of
            (False, True) -> pure $ "Error: source is a directory, not a regular file: " <> src
            (False, False) -> pure $ "Error: source is not a regular file: " <> src
            _ -> do
              dstResolved <- sandboxWritePath cfg (T.unpack dst)
              renameFile srcResolved dstResolved
              pure $ "Successfully moved " <> src <> " to " <> dst
