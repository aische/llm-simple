module LLM.Tools.RemoveDirectory (removeDirectoryToolTyped, RemoveDirectoryToolArgs (..)) where

import Autodocodec qualified as AC
import Control.Monad (forM_)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, isSandboxRoot, isSymlink, sandboxPath, sandboxSymlink)
import System.Directory
  ( doesDirectoryExist,
    listDirectory,
    removeDirectory,
    removeFile,
  )
import System.FilePath ((</>))

newtype RemoveDirectoryToolArgs = RemoveDirectoryToolArgs
  { _rdPath :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec RemoveDirectoryToolArgs)

instance AC.HasCodec RemoveDirectoryToolArgs where
  codec =
    AC.object "remove a directory" $
      RemoveDirectoryToolArgs
        <$> AC.requiredField "path" "Relative path of the directory to remove" AC..= (._rdPath)

removeDirectoryToolTyped :: FsConfig -> TypedTool ctx RemoveDirectoryToolArgs
removeDirectoryToolTyped cfg =
  TypedTool
    { ttoolName = "remove_directory",
      ttoolDescription =
        "Recursively delete a directory and all its contents at the given path (relative to the workspace). "
          <> "Use with caution — this operation is irreversible. "
          <> "A symbolic link is removed itself; its target is left unchanged.",
      ttoolReadonly = False,
      ttoolExecute = const (removeDirectoryExecTyped cfg)
    }

removeDirectoryExecTyped :: FsConfig -> RemoveDirectoryToolArgs -> IO Text
removeDirectoryExecTyped cfg args = do
  let p = args._rdPath
  link <- sandboxSymlink cfg (T.unpack p)
  case link of
    Just l -> do
      removeFile l
      pure $ "Successfully removed symbolic link " <> p <> " (its target was left unchanged)"
    Nothing -> do
      resolved <- sandboxPath cfg (T.unpack p)
      if isSandboxRoot cfg resolved
        then pure $ "Error: refusing to remove the workspace root: " <> p
        else do
          safeRemoveDirectoryRecursive resolved
          pure $ "Successfully removed directory " <> p

-- | Like 'System.Directory.removeDirectoryRecursive', but refuses to
-- follow directory symlinks. A symbolic link encountered during the
-- walk is unlinked (its target is left intact), so a symlink inside
-- the sandbox pointing at an external directory cannot be used to
-- delete files outside the sandbox.
--
-- Note: a top-level link is handled by the caller via 'sandboxSymlink',
-- and the argument has been resolved through 'sandboxPath', so it is a
-- real directory inside the sandbox at the moment of the call. We still
-- re-check it defensively in case of an unusual TOCTOU race.
safeRemoveDirectoryRecursive :: FilePath -> IO ()
safeRemoveDirectoryRecursive dir = do
  topIsLink <- isSymlink dir
  if topIsLink
    then removeFile dir
    else do
      entries <- listDirectory dir
      forM_ entries $ \entry -> do
        let child = dir </> entry
        childIsLink <- isSymlink child
        if childIsLink
          then removeFile child
          else do
            childIsDir <- doesDirectoryExist child
            if childIsDir
              then safeRemoveDirectoryRecursive child
              else removeFile child
      removeDirectory dir
