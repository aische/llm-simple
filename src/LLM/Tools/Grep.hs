module LLM.Tools.Grep
  ( grepToolTyped,
    GrepToolArgs (..),
  )
where

import Autodocodec qualified as AC
import Control.Exception (IOException, try)
import Control.Monad (unless)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Aeson (FromJSON)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, isFileHidden, isSymlink, sandboxPath, withWorkspacePaths)
import LLM.Tools.FsLimits (expensiveWalkDepth, maxOutputBytes, maxVisitedEntries)
import LLM.Tools.Internal.TextFile (readLenient, withTextHandle)
import LLM.Tools.Internal.WalkBudget (WalkBudget, budgetExhausted, budgetNote, capLines, newWalkBudget, takeEntries)
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
          <> "the header reports whether results were truncated and how many files were scanned. "
          <> "A search looks at most "
          <> T.pack (show maxVisitedEntries)
          <> " entries and stops at maxResults; then the file count is shown as 'N+'.",
      ttoolReadonly = True,
      ttoolExecute = const (withWorkspacePaths cfg . execute cfg)
    }

execute :: FsConfig -> GrepToolArgs -> IO Text
execute cfg args = do
  let rel = T.unpack args._grepPath
      pat = args._grepPattern
      ci = args._grepCaseInsensitive
      exts = normalizeExtensions args._grepExtensions
      cap = min maxResultsCap args._grepMaxResults
  if T.null pat
    then pure "Error: pattern must be non-empty"
    else if args._grepMaxResults <= 0
    then pure $ "Error: maxResults must be positive, got " <> T.pack (show args._grepMaxResults)
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
              budget <- newWalkBudget maxVisitedEntries
              st <- newIORef (GrepState [] 0 0 False)
              let search = searchFile st pat ci cap
              if isFile
                then search resolved (takeFileName resolved)
                else walkDir budget st search exts resolved "" 0
              GrepState matches _ scanned capHit <- readIORef st
              stopped <- budgetExhausted budget
              pure $
                renderResults
                  args._grepPath
                  pat
                  (reverse matches)
                  scanned
                  ((capHit && not isFile) || stopped)
                  capHit
                  (if stopped then Just (budgetNote budget) else Nothing)

-- | Lowercase and strip leading dots so callers can pass "hs" or ".hs".
normalizeExtensions :: [Text] -> [Text]
normalizeExtensions = map (T.toLower . T.dropWhile (== '.'))

-- | Matches so far (newest first), their count, files searched, and
-- whether the walk stopped because 'maxResults' was reached.
data GrepState = GrepState [Match] Int Int Bool

-- | Walk a directory and search each candidate file as it is found,
-- respecting depth, hidden-file, extension and size limits and the entry
-- budget. Stops once 'maxResults' is reached. Order is stable (entries
-- sorted) so output is deterministic. Each file is shown by its path
-- relative to the search root, built from the names walked, so no host
-- path is shown.
walkDir ::
  WalkBudget ->
  IORef GrepState ->
  (FilePath -> FilePath -> IO ()) ->
  [Text] ->
  FilePath ->
  FilePath ->
  Int ->
  IO ()
walkDir budget st search exts = go
  where
    go dir display depth
      | depth > maxDepth = pure ()
      | otherwise = do
          entries <- sort <$> safeListDirectory dir
          allowed <- takeEntries budget (filter (not . isFileHidden) entries)
          mapM_ (visit dir display depth) allowed
    visit parent display depth name = do
      GrepState _ _ _ capHit <- readIORef st
      unless capHit $ do
        let full = parent </> name
            fullDisplay = display </> name
        -- Skip symlinks to prevent traversal out of the sandbox.
        isLink <- isSymlink full
        unless isLink $ do
          isDir <- doesDirectoryExist full
          if isDir
            then go full fullDisplay (depth + 1)
            else do
              isFile <- doesFileExist full
              ok <-
                if isFile && extensionAllowed exts name
                  then isSizeOk full
                  else pure False
              if ok then search full fullDisplay else pure ()

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

-- | Search one file and add its matches, up to the cap. Binary and
-- unreadable files count as searched but give no matches.
searchFile :: IORef GrepState -> Text -> Bool -> Int -> FilePath -> FilePath -> IO ()
searchFile st pat ci cap f display = do
  r <-
    try (withTextHandle (withBinaryFile f ReadMode) readLenient) ::
      IO (Either IOException (Maybe Text))
  let ms = case r of
        Right (Just content) -> matchLines display content needle ci
        _ -> []
  modifyIORef' st $ \(GrepState acc n scanned _) ->
    let remaining = cap - n
        taken = take remaining ms
        n' = n + length taken
     in GrepState (reverse taken ++ acc) n' (scanned + 1) (n' >= cap)
  where
    needle = if ci then T.toLower pat else pat

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

-- | Render the results. @partial@: the walk stopped early, so the file
-- count is a lower bound. @capHit@: 'maxResults' was reached.
renderResults :: Text -> Text -> [Match] -> Int -> Bool -> Bool -> Maybe Text -> Text
renderResults rootDisplay pat allMatches scanned partial capHit note =
  let (body, cut) = capLines maxOutputBytes (map fmtMatch allMatches)
      header =
        "=== grep "
          <> T.pack (show pat)
          <> " in "
          <> rootDisplay
          <> " | "
          <> T.pack (show (length body))
          <> " match(es) across "
          <> T.pack (show scanned)
          <> (if partial then "+" else "")
          <> " file(s)"
          <> ( if cut
                 then " | output truncated at " <> T.pack (show maxOutputBytes) <> " bytes"
                 else if capHit then " | stopped at maxResults (raise it to see more)" else ""
             )
          <> maybe "" (" | " <>) note
          <> " ==="
   in T.unlines (header : body)
  where
    fmtMatch m =
      T.pack m.mPath
        <> ":"
        <> T.pack (show m.mLine)
        <> ": "
        <> m.mText
