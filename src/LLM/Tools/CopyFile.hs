module LLM.Tools.CopyFile (copyFileToolTyped, CopyFileToolArgs (..)) where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, isSymlink, sandboxPath, sandboxWritePath)
import System.Directory (copyFile, doesDirectoryExist, doesFileExist, doesPathExist)

data CopyFileToolArgs = CopyFileToolArgs
  { _cfSrc :: Text,
    _cfDst :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec CopyFileToolArgs)

instance AC.HasCodec CopyFileToolArgs where
  codec =
    AC.object "copy source to destination" $
      CopyFileToolArgs
        <$> AC.requiredField "source" "Relative path of the file to copy" AC..= (._cfSrc)
        <*> AC.requiredField "destination" "Relative destination path (including filename)" AC..= (._cfDst)

copyFileToolTyped :: FsConfig -> TypedTool ctx CopyFileToolArgs
copyFileToolTyped cfg =
  TypedTool
    { ttoolName = "copy_file",
      ttoolDescription =
        "Copy a file from source to destination (both paths relative to the workspace). "
          <> "Overwrites the destination if it already exists. "
          <> "Creates parent directories at the destination as needed.",
      ttoolReadonly = False,
      ttoolExecute = const (copyFileExecTyped cfg)
    }

copyFileExecTyped :: FsConfig -> CopyFileToolArgs -> IO Text
copyFileExecTyped cfg args = do
  let src = args._cfSrc
      dst = args._cfDst
  srcResolved <- sandboxPath cfg (T.unpack src)
  -- Refuse symbolic-link sources: copying would either dereference the
  -- link (potentially pulling content from outside the sandbox) or be
  -- ambiguous about whether the link itself should be reproduced.
  srcIsLink <- isSymlink srcResolved
  if srcIsLink
    then pure $ "Error: refusing to copy symbolic link source: " <> src
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
              copyFile srcResolved dstResolved
              pure $ "Successfully copied " <> src <> " to " <> dst
