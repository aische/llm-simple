module LLM.Tools.DirectoryTree (directoryTreeToolTyped, DirectoryTreeToolArgs (..)) where

import Autodocodec qualified as AC
import Control.Monad (forM)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, isFileHidden, isSymlink, sandboxPath)
import System.Directory
  ( doesDirectoryExist,
    listDirectory,
  )
import System.FilePath ((</>))

newtype DirectoryTreeToolArgs = DirectoryTreeToolArgs
  { _dtPath :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec DirectoryTreeToolArgs)

instance AC.HasCodec DirectoryTreeToolArgs where
  codec :: AC.JSONCodec DirectoryTreeToolArgs
  codec =
    AC.object "show a directory tree and its subdirectories" $
      DirectoryTreeToolArgs <$> AC.requiredField "path" "Relative directory path to show the tree of" AC..= (._dtPath)

directoryTreeToolTyped :: FsConfig -> TypedTool ctx DirectoryTreeToolArgs
directoryTreeToolTyped fsConfig =
  TypedTool
    { ttoolName = "directory_tree",
      ttoolDescription =
        "Show the directory tree (and its subdirectories) of a directory (relative to the workspace). "
          <> "Returns the directory tree as a string. "
          <> "Use path '.' or omit it to list the workspace root.",
      ttoolReadonly = True,
      ttoolExecute = const (directoryTreeExecTyped fsConfig)
    }

directoryTreeExecTyped :: FsConfig -> DirectoryTreeToolArgs -> IO Text
directoryTreeExecTyped cfg args = do
  let relPath = T.unpack args._dtPath
  resolved <- sandboxPath cfg relPath
  let displayRoot = if null relPath then "." else relPath
  drawTree resolved displayRoot

drawTree :: FilePath -> FilePath -> IO T.Text
drawTree path displayRoot = do
  let rootName = T.pack displayRoot
  linesOfTree <- drawTreeHelper "" path
  pure $ T.unlines (rootName : linesOfTree)

drawTreeHelper :: T.Text -> FilePath -> IO [T.Text]
drawTreeHelper = aux
  where
    aux prefix path = do
      -- Don't follow symlinks: they could point outside the sandbox.
      isLink <- isSymlink path
      isDir <- doesDirectoryExist path
      if isDir && not isLink
        then do
          contents <- listDirectory path
          let validContents = filter (`notElem` [".", ".."]) contents
              count = length validContents
          nestedLines <- forM (zip [1 .. count] validContents) $ \(index, name) ->
            if isFileHidden name
              then pure []
              else do
                let isLast = index == count
                    textName = T.pack name
                    connector = if isLast then "└── " else "├── "
                    currentLine = prefix <> connector <> textName
                    newPrefix = prefix <> if isLast then "    " else "│   "
                subLines <- aux newPrefix (path </> name)
                pure (currentLine : subLines)
          pure (concat nestedLines)
        else pure []
