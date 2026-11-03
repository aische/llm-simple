module LLM.Tools.DirectoryTree (directoryTreeToolTyped, DirectoryTreeToolArgs (..)) where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, isFileHidden, isSymlink, sandboxPath, withWorkspacePaths)
import LLM.Tools.FsLimits (cheapWalkDepth, maxOutputBytes, maxTreeEntries, maxVisitedEntries)
import LLM.Tools.Internal.WalkBudget (WalkBudget, budgetExhausted, budgetNote, capLines, newWalkBudget, takeEntries, utf8Length)
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
          <> "Use path '.' or omit it to list the workspace root. "
          <> "Traversal is capped at depth "
          <> T.pack (show cheapWalkDepth)
          <> " and "
          <> T.pack (show maxTreeEntries)
          <> " entries shown, "
          <> T.pack (show maxVisitedEntries)
          <> " entries looked at, and "
          <> T.pack (show maxOutputBytes)
          <> " bytes of output; truncation is reported in the header.",
      ttoolReadonly = True,
      ttoolExecute = const (withWorkspacePaths fsConfig . directoryTreeExecTyped fsConfig)
    }

directoryTreeExecTyped :: FsConfig -> DirectoryTreeToolArgs -> IO Text
directoryTreeExecTyped cfg args = do
  let relPath = T.unpack args._dtPath
  resolved <- sandboxPath cfg relPath
  let displayRoot = if null relPath then "." else relPath
  drawTree resolved displayRoot

-- | Result of a (sub)tree walk: rendered lines, total entries consumed
-- so far (against the global cap), and whether the global caps cut
-- the walk short.
data WalkState = WalkState
  { wsLines :: [Text],
    wsEntries :: Int,
    wsTruncated :: Bool,
    wsDepthCut :: Bool
  }

drawTree :: FilePath -> FilePath -> IO Text
drawTree path displayRoot = do
  let rootName = T.pack displayRoot
  budget <- newWalkBudget maxVisitedEntries
  result <- walkDir budget "" 0 0 path
  stopped <- budgetExhausted budget
  let (shown, cut) = capLines (maxOutputBytes - utf8Length rootName - 1) result.wsLines
      budgetNotes = [budgetNote budget | stopped]
      header = renderHeader (length shown) result.wsTruncated cut result.wsDepthCut budgetNotes
  pure $ T.unlines (header ++ rootName : shown)
  where
    renderHeader n trunc cut depthCut extra =
      let notes =
            ([T.pack ("truncated at " ++ show maxTreeEntries ++ " entries") | trunc])
              ++ ([T.pack ("output truncated at " ++ show maxOutputBytes ++ " bytes") | cut])
              ++ ([T.pack ("depth cut at " ++ show cheapWalkDepth) | depthCut])
              ++ extra
       in ( [ "=== directory_tree | "
                <> T.pack (show n)
                <> " entr"
                <> (if n == 1 then "y" else "ies")
                <> " | "
                <> T.intercalate ", " notes
                <> " ==="
              | not (null notes)
            ]
          )

-- | Walk a directory non-recursively in the outer caller's sense:
-- emits lines for visible children, recursing into subdirectories until
-- either depth or entry caps are hit. Both caps are global to the whole
-- walk (entries is a running total across siblings).
walkDir :: WalkBudget -> Text -> Int -> Int -> FilePath -> IO WalkState
walkDir budget prefix depth entriesSoFar path
  | depth > cheapWalkDepth =
      pure (WalkState [] entriesSoFar False True)
  | entriesSoFar >= maxTreeEntries =
      pure (WalkState [] entriesSoFar True False)
  | otherwise = do
      isLink <- isSymlink path
      isDir <- doesDirectoryExist path
      if not (isDir && not isLink)
        then pure (WalkState [] entriesSoFar False False)
        else do
          contents <- listDirectory path
          validContents <-
            takeEntries budget $
              filter (\n -> n `notElem` [".", ".."] && not (isFileHidden n)) contents
          let count = length validContents
          stepChildren budget prefix depth entriesSoFar path (zip [1 .. count] validContents) count

stepChildren ::
  WalkBudget ->
  Text ->
  Int ->
  Int ->
  FilePath ->
  [(Int, FilePath)] ->
  Int ->
  IO WalkState
stepChildren _ _ _ entries _ [] _ =
  pure (WalkState [] entries False False)
stepChildren budget prefix depth entries dir ((idx, name) : rest) count
  | entries >= maxTreeEntries =
      pure (WalkState [] entries True False)
  | otherwise = do
      let isLast = idx == count
          textName = T.pack name
          connector = if isLast then "└── " else "├── "
          currentLine = prefix <> connector <> textName
          newPrefix = prefix <> if isLast then "    " else "│   "
          entries' = entries + 1
      sub <- walkDir budget newPrefix (depth + 1) entries' (dir </> name)
      siblings <- stepChildren budget prefix depth sub.wsEntries dir rest count
      pure
        WalkState
          { wsLines = currentLine : sub.wsLines ++ siblings.wsLines,
            wsEntries = siblings.wsEntries,
            wsTruncated = sub.wsTruncated || siblings.wsTruncated,
            wsDepthCut = sub.wsDepthCut || siblings.wsDepthCut
          }
