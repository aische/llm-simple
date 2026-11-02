module LLM.Tools.Grep
  ( grepToolTyped,
    GrepToolArgs (..),
  )
where

import Autodocodec qualified as AC
import Control.Exception (IOException, try)
import Data.Aeson (FromJSON)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, isFileHidden, isSymlink, sandboxPath, withWorkspacePaths)
import LLM.Tools.FsLimits (expensiveWalkDepth)
import LLM.Tools.Internal.TextFile (readLenient, withTextHandle)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getFileSize,
    listDirectory,
  )
import System.FilePath (takeExtension, takeFileName, (</>))
import System.IO
  ( IOMode (ReadMode),
    withBinaryFile,
  )

-- | Default cap on the number of match lines returned.
defaultMaxResults :: Int
defaultMaxResults = 50

-- | Hard cap on results to protect the model's context window.
maxResultsCap :: Int
maxResultsCap = 500

-- | Skip files larger than this (bytes). Big files are usually generated
-- artifacts (lock files, fixtures) that aren't useful to grep through.
maxFileSizeBytes :: Integer
maxFileSizeBytes = 1024 * 1024 -- 1 MB

-- | Maximum directory depth from the search root. Sourced from
-- 'LLM.Tools.FsLimits.expensiveWalkDepth' because @grep@ reads every
-- encountered file.
maxDepth :: Int
maxDepth = expensiveWalkDepth

-- | Truncate matched line text in output to keep results compact.
maxLineLength :: Int
maxLineLength = 240

data GrepToolArgs = GrepToolArgs
  { _grepPattern :: Text,
    _grepPath :: Text,
    _grepCaseInsensitive :: Bool,
    _grepExtensions :: [Text],
    _grepMaxResults :: Int
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec GrepToolArgs)

instance AC.HasCodec GrepToolArgs where
  codec :: AC.JSONCodec GrepToolArgs
  codec =
    AC.object "search files for a literal substring" $
      GrepToolArgs
        <$> AC.requiredField "pattern" "Literal substring to search for (not a regex)"
          AC..= (._grepPattern)
        <*> AC.optionalFieldWithDefault
          "path"
          "."
          "Relative directory or file to search under (default '.')"
          AC..= (._grepPath)
        <*> AC.optionalFieldWithDefault
          "caseInsensitive"
          False
          "Match case-insensitively (default false)"
          AC..= (._grepCaseInsensitive)
        <*> AC.optionalFieldWithDefault
          "extensions"
          []
          "Restrict to files with these extensions, without the dot, e.g. [\"hs\", \"cabal\"]. Empty means all text files."
          AC..= (._grepExtensions)
        <*> AC.optionalFieldWithDefault
          "maxResults"
          defaultMaxResults
          "Maximum number of match lines to return (default 50, hard cap 500)"
          AC..= (._grepMaxResults)

grepToolTyped :: FsConfig -> TypedTool ctx GrepToolArgs
grepToolTyped cfg =
  TypedTool
    { ttoolName = "grep",
      ttoolDescription =
        "Recursively search files for a literal substring (not a regex). "
          <> "Returns matches as 'relative/path:line: snippet', sorted by file. "
          <> "Skips hidden files, binary files, and files larger than 1 MB. "
          <> "Use 'extensions' to restrict by file type (e.g. [\"hs\"]). "
          <> "Output is capped at 'maxResults' lines (default 50, hard cap 500); "
          <> "the header reports whether results were truncated and how many files were scanned.",
      ttoolReadonly = True,
      ttoolExecute = const (withWorkspacePaths cfg . execute cfg)
    }

execute :: FsConfig -> GrepToolArgs -> IO Text
execute cfg args = do
  let rel = T.unpack args._grepPath
      pat = args._grepPattern
      ci = args._grepCaseInsensitive
      exts = normalizeExtensions args._grepExtensions
      cap = max 1 (min maxResultsCap args._grepMaxResults)
  if T.null pat
    then pure "Error: pattern must be non-empty"
    else do
      resolved <- sandboxPath cfg rel
      isDir <- doesDirectoryExist resolved
      isFile <- doesFileExist resolved
      -- An explicit file gets the same size cap as files found by the walk.
      sizeOk <- if isFile then isSizeOk resolved else pure True
      if not (isDir || isFile)
        then pure $ "Error: path does not exist: " <> args._grepPath
        else
          if not sizeOk
            then
              pure $
                "Error: "
                  <> args._grepPath
                  <> " exceeds the "
                  <> T.pack (show maxFileSizeBytes)
                  <> "-byte cap for grep."
            else do
              files <-
                if isFile
                  then pure [(resolved, takeFileName resolved)]
                  else collectFiles resolved "" 0 exts
              (matches, scanned, truncated) <- searchFiles files pat ci cap
              pure $ renderResults args._grepPath pat matches scanned truncated

-- | Lowercase and strip leading dots so callers can pass "hs" or ".hs".
normalizeExtensions :: [Text] -> [Text]
normalizeExtensions = map (T.toLower . T.dropWhile (== '.'))

-- | Recursively collect candidate files under a directory, respecting
-- depth, hidden-file, extension, and size limits. Order is stable
-- (directories sorted) so output is deterministic. Each file comes with
-- its path relative to the search root, built from the names walked, so
-- no host path is shown.
collectFiles :: FilePath -> FilePath -> Int -> [Text] -> IO [(FilePath, FilePath)]
collectFiles _ _ depth _ | depth > maxDepth = pure []
collectFiles dir display depth exts = do
  entries <- sort <$> safeListDirectory dir
  let visible = filter (not . isFileHidden) entries
  results <- mapM (visit dir display depth exts) visible
  pure (concat results)

visit :: FilePath -> FilePath -> Int -> [Text] -> FilePath -> IO [(FilePath, FilePath)]
visit parent display depth exts name = do
  let full = parent </> name
      fullDisplay = display </> name
  -- Skip symlinks to prevent traversal out of the sandbox.
  isLink <- isSymlink full
  if isLink
    then pure []
    else do
      isDir <- doesDirectoryExist full
      if isDir
        then collectFiles full fullDisplay (depth + 1) exts
        else do
          isFile <- doesFileExist full
          if isFile && extensionAllowed exts name
            then do
              ok <- isSizeOk full
              pure [(full, fullDisplay) | ok]
            else pure []

extensionAllowed :: [Text] -> FilePath -> Bool
extensionAllowed [] _ = True
extensionAllowed exts name =
  let ext = T.toLower (T.pack (dropDot (takeExtension name)))
   in ext `elem` exts
  where
    dropDot ('.' : rest) = rest
    dropDot s = s

isSizeOk :: FilePath -> IO Bool
isSizeOk path = do
  r <- try (getFileSize path) :: IO (Either IOException Integer)
  case r of
    Right sz -> pure (sz <= maxFileSizeBytes)
    Left _ -> pure False

safeListDirectory :: FilePath -> IO [FilePath]
safeListDirectory path = do
  r <- try (listDirectory path) :: IO (Either IOException [FilePath])
  case r of
    Right xs -> pure xs
    Left _ -> pure []

-- | Search the given files for a literal pattern. Stops collecting matches
-- once the cap is reached but keeps scanning the remaining files so the
-- 'scanned' count reflects the full sweep. Returns (matches, files scanned,
-- truncated?).
searchFiles ::
  [(FilePath, FilePath)] ->
  Text ->
  Bool ->
  Int ->
  IO ([Match], Int, Bool)
searchFiles files pat ci cap = go files [] 0 False
  where
    needle = if ci then T.toLower pat else pat
    go [] acc scanned truncated = pure (reverse acc, scanned, truncated)
    go ((f, display) : rest) acc scanned truncated
      | length acc >= cap = go rest acc (scanned + 1) True
      | otherwise = do
          -- Binary and unreadable files are skipped.
          r <-
            try (withTextHandle (withBinaryFile f ReadMode) readLenient) ::
              IO (Either IOException (Maybe Text))
          case r of
            Right (Just content) -> do
              let ms = matchLines display content needle ci
                  remaining = cap - length acc
                  taken = take remaining ms
                  truncated' = truncated || length ms > remaining
              go rest (reverse taken ++ acc) (scanned + 1) truncated'
            _ -> go rest acc (scanned + 1) truncated

data Match = Match
  { mPath :: FilePath,
    mLine :: Int,
    mText :: Text
  }

matchLines :: FilePath -> Text -> Text -> Bool -> [Match]
matchLines path content needle ci =
  [ Match path n (truncateLine line)
    | (n, line) <- zip [1 ..] (T.lines content),
      let hay = if ci then T.toLower line else line,
      needle `T.isInfixOf` hay
  ]

truncateLine :: Text -> Text
truncateLine t
  | T.length t <= maxLineLength = t
  | otherwise = T.take maxLineLength t <> "…"

renderResults :: Text -> Text -> [Match] -> Int -> Bool -> Text
renderResults rootDisplay pat matches scanned truncated =
  let header =
        "=== grep "
          <> T.pack (show pat)
          <> " in "
          <> rootDisplay
          <> " | "
          <> T.pack (show (length matches))
          <> " match(es) across "
          <> T.pack (show scanned)
          <> " file(s)"
          <> (if truncated then " | truncated (raise maxResults to see more)" else "")
          <> " ==="
      body = map fmtMatch matches
   in T.unlines (header : body)
  where
    fmtMatch m =
      T.pack m.mPath
        <> ":"
        <> T.pack (show m.mLine)
        <> ": "
        <> m.mText
