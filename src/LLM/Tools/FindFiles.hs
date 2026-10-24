module LLM.Tools.FindFiles
  ( findFilesToolTyped,
    FindFilesToolArgs (..),
  )
where

import Autodocodec qualified as AC
import Control.Exception (IOException, try)
import Data.Aeson (FromJSON)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, isFileHidden, isSymlink, sandboxPath)
import LLM.Tools.FsLimits (cheapWalkDepth)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

-- | Default cap on returned paths.
defaultMaxResults :: Int
defaultMaxResults = 200

-- | Hard cap to protect the model's context window.
maxResultsCap :: Int
maxResultsCap = 2000

-- | Maximum directory depth from the search root. Sourced from
-- 'LLM.Tools.FsLimits.cheapWalkDepth' because @find_files@ only stats
-- entries, so it can afford a deeper walk than @grep@.
maxDepth :: Int
maxDepth = cheapWalkDepth

-- | What kind of filesystem entries to return.
data EntryFilter = AnyEntry | FileOnly | DirectoryOnly
  deriving (Eq, Show)

parseEntryFilter :: Text -> EntryFilter
parseEntryFilter t = case T.toLower (T.strip t) of
  "file" -> FileOnly
  "directory" -> DirectoryOnly
  "dir" -> DirectoryOnly
  _ -> AnyEntry

data FindFilesToolArgs = FindFilesToolArgs
  { _ffPattern :: Text,
    _ffPath :: Text,
    _ffType :: Text,
    _ffIncludeHidden :: Bool,
    _ffMaxResults :: Int
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec FindFilesToolArgs)

instance AC.HasCodec FindFilesToolArgs where
  codec :: AC.JSONCodec FindFilesToolArgs
  codec =
    AC.object "find files by glob pattern" $
      FindFilesToolArgs
        <$> AC.requiredField
          "pattern"
          "Glob pattern matched against paths relative to 'path'. Supports '**' (any number of path segments, including zero), '*' (any chars except '/'), and '?' (any single char except '/'). Examples: '**/*.hs', 'src/**/Tools/*.hs', '*.cabal'."
          AC..= (._ffPattern)
        <*> AC.optionalFieldWithDefault
          "path"
          "."
          "Relative directory to search under (default '.')"
          AC..= (._ffPath)
        <*> AC.optionalFieldWithDefault
          "type"
          "any"
          "Entry kind to return: 'file', 'directory', or 'any' (default 'any')"
          AC..= (._ffType)
        <*> AC.optionalFieldWithDefault
          "includeHidden"
          False
          "Include dotfiles and dot-directories (default false)"
          AC..= (._ffIncludeHidden)
        <*> AC.optionalFieldWithDefault
          "maxResults"
          defaultMaxResults
          "Maximum number of paths to return (default 200, hard cap 2000)"
          AC..= (._ffMaxResults)

findFilesToolTyped :: FsConfig -> TypedTool ctx FindFilesToolArgs
findFilesToolTyped cfg =
  TypedTool
    { ttoolName = "find_files",
      ttoolDescription =
        "Find files and directories by glob pattern under a workspace-relative root. "
          <> "Supports '**' (any number of path segments, including zero), '*' (any chars except '/'), and '?'. "
          <> "Returns one path per line, relative to the search root, sorted. "
          <> "Skips hidden entries unless 'includeHidden' is true. "
          <> "Use 'type' to restrict to files or directories. "
          <> "Output is capped at 'maxResults' (default 200, hard cap 2000); "
          <> "the header reports whether results were truncated and how many entries were scanned.",
      ttoolReadonly = True,
      ttoolExecute = const (execute cfg)
    }

execute :: FsConfig -> FindFilesToolArgs -> IO Text
execute cfg args = do
  let rel = T.unpack args._ffPath
      pat = args._ffPattern
      kind = parseEntryFilter args._ffType
      includeHidden = args._ffIncludeHidden
      cap = max 1 (min maxResultsCap args._ffMaxResults)
  if T.null pat
    then pure "Error: pattern must be non-empty"
    else do
      resolved <- sandboxPath cfg rel
      isDir <- doesDirectoryExist resolved
      if not isDir
        then pure $ "Error: path is not a directory: " <> args._ffPath
        else do
          let segs = splitGlob pat
          (matches, scanned, truncated) <- walk resolved includeHidden kind segs cap
          pure $ renderResults args._ffPath pat matches scanned truncated

-- ---------------------------------------------------------------------------
-- Filesystem walk
-- ---------------------------------------------------------------------------

-- | Result of the walk: matched relative paths (sorted within each directory),
-- count of entries inspected, and whether the cap truncated the output.
walk ::
  FilePath ->
  Bool ->
  EntryFilter ->
  [Text] ->
  Int ->
  IO ([FilePath], Int, Bool)
walk root includeHidden kind pat cap = do
  WalkState acc _ scanned truncated <- go root [] 0 (WalkState [] 0 0 False)
  pure (reverse acc, scanned, truncated)
  where
    -- relSegs is the path from root to current dir, in reverse order
    -- depth is current directory depth (0 = root)
    go dir relSegs depth st@(WalkState acc n scanned _)
      | n >= cap = pure (WalkState acc n scanned True)
      | depth > maxDepth = pure st
      | otherwise = do
          entries <- sort <$> safeListDirectory dir
          let visible =
                if includeHidden
                  then entries
                  else filter (not . isFileHidden) entries
          step visible dir relSegs depth st

    step [] _ _ _ st = pure st
    step (name : rest) dir relSegs depth (WalkState acc n scanned truncated)
      | n >= cap = pure (WalkState acc n (scanned + 1) True)
      | otherwise = do
          let full = dir </> name
              entryRel = reverse (T.pack name : relSegs)
              relPath = T.unpack (T.intercalate "/" entryRel)
          -- Skip symlinks: they could point outside the sandbox.
          isLink <- isSymlink full
          if isLink
            then step rest dir relSegs depth (WalkState acc n (scanned + 1) truncated)
            else do
              isDir <- doesDirectoryExist full
              let entryKind = if isDir then DirectoryOnly else FileOnly
                  kindOk = case kind of
                    AnyEntry -> True
                    FileOnly -> entryKind == FileOnly
                    DirectoryOnly -> entryKind == DirectoryOnly
                  matched = kindOk && matchGlob pat entryRel
                  st'
                    | matched = WalkState (relPath : acc) (n + 1) (scanned + 1) truncated
                    | otherwise = WalkState acc n (scanned + 1) truncated
              st'' <-
                if isDir
                  then go full (T.pack name : relSegs) (depth + 1) st'
                  else pure st'
              step rest dir relSegs depth st''

-- | Walk state: matches so far (newest first), their count, entries
-- inspected, and whether the cap truncated the output.
data WalkState = WalkState [FilePath] Int Int Bool

safeListDirectory :: FilePath -> IO [FilePath]
safeListDirectory path = do
  r <- try (listDirectory path) :: IO (Either IOException [FilePath])
  case r of
    Right xs -> pure xs
    Left _ -> pure []

-- ---------------------------------------------------------------------------
-- Glob matcher
-- ---------------------------------------------------------------------------

-- | Split a glob into path segments. Leading "./" is dropped so callers
-- can write either "**/*.hs" or "./**/*.hs".
splitGlob :: Text -> [Text]
splitGlob pat =
  let stripped = fromMaybe pat (T.stripPrefix "./" pat)
   in filter (not . T.null) (T.splitOn "/" stripped)

-- | Match a list of glob segments against a list of path segments.
-- '**' may match zero or more path segments; other segments must match
-- exactly one segment via 'matchSegment'.
matchGlob :: [Text] -> [Text] -> Bool
matchGlob [] [] = True
matchGlob [] _ = False
matchGlob ("**" : ps) ss =
  -- '**' matches zero segments, or one+ segments then continues
  matchGlob ps ss
    || case ss of
      [] -> False
      (_ : rest) -> matchGlob ("**" : ps) rest
matchGlob _ [] = False
matchGlob (p : ps) (s : ss) = matchSegment (T.unpack p) (T.unpack s) && matchGlob ps ss

-- | Match a single glob segment (no '/' allowed in either side).
-- Supports '*' (any run of chars, possibly empty) and '?' (one char).
-- On a mismatch only the most recent '*' takes one more char, which is
-- enough for this glob syntax and keeps matching O(pattern * name) instead
-- of exponential in the number of stars.
matchSegment :: String -> String -> Bool
matchSegment pat0 str0 = go pat0 str0 Nothing
  where
    -- lastStar: the pattern after the most recent '*' and the input it
    -- was last retried from.
    go ('*' : ps) ss _ = go ps ss (Just (ps, ss))
    go (p : ps) (s : ss) lastStar
      | p == '?' || p == s = go ps ss lastStar
    go [] [] _ = True
    go _ _ (Just (ps, _ : ss)) = go ps ss (Just (ps, ss))
    go _ _ _ = False

-- ---------------------------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------------------------

renderResults :: Text -> Text -> [FilePath] -> Int -> Bool -> Text
renderResults rootDisplay pat matches scanned truncated =
  let header =
        "=== find_files "
          <> T.pack (show pat)
          <> " in "
          <> rootDisplay
          <> " | "
          <> T.pack (show (length matches))
          <> " match(es) across "
          <> T.pack (show scanned)
          <> " entr"
          <> (if scanned == 1 then "y" else "ies")
          <> (if truncated then " | truncated (raise maxResults to see more)" else "")
          <> " ==="
      body = map T.pack matches
   in T.unlines (header : body)
