module LLM.Tools.MoveFile (moveFileToolTyped, MoveFileToolArgs (..)) where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, isSandboxRoot, sandboxPath, sandboxSymlink, sandboxWritePath, withWorkspacePaths)
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
          <> "Creates parent directories at the destination as needed. "
          <> "Refuses to overwrite: if anything exists at the destination, remove it first.",
      ttoolReadonly = False,
      ttoolExecute = const (withWorkspacePaths cfg . moveFileExecTyped cfg)
    }

moveFileExecTyped :: FsConfig -> MoveFileToolArgs -> IO Text
moveFileExecTyped cfg args = do
  let src = args._mfSrc
      dst = args._mfDst
  -- Refuse to operate on symbolic-link sources: this would either move
  -- the link itself (surprising) or, after `sandboxPath` resolution,
  -- move the link's target. Checked before resolving, since
  -- `sandboxPath` follows the link.
  srcLink <- sandboxSymlink cfg (T.unpack src)
  srcResolved <- sandboxPath cfg (T.unpack src)
  if isSandboxRoot cfg srcResolved
    then pure $ "Error: refusing to move the workspace root: " <> src
    else
      if isJust srcLink
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
                  -- Anything at the destination counts, a link included
                  -- (even a dangling one, which 'sandboxPath' would follow
                  -- to a missing target). Checked before 'sandboxWritePath'
                  -- creates parent directories. The check and the rename
                  -- are not atomic; another process can still create the
                  -- destination in between.
                  dstLink <- sandboxSymlink cfg (T.unpack dst)
                  dstExists <-
                    if isJust dstLink
                      then pure True
                      else sandboxPath cfg (T.unpack dst) >>= doesPathExist
                  if dstExists
                    then pure $ "Error: destination already exists: " <> dst <> "; remove it first"
                    else do
                      dstResolved <- sandboxWritePath cfg (T.unpack dst)
                      renameFile srcResolved dstResolved
                      pure $ "Successfully moved " <> src <> " to " <> dst
