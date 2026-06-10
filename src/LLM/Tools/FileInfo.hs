module LLM.Tools.FileInfo
  ( fileInfoToolTyped,
    FileInfoToolArgs (..),
  )
where

import Autodocodec qualified as AC
import Control.Exception (IOException, try)
import Data.Aeson (FromJSON)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, sandboxPath)
import System.Directory
  ( Permissions (..),
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    getFileSize,
    getModificationTime,
    getPermissions,
    listDirectory,
    pathIsSymbolicLink,
  )
import System.IO (IOMode (ReadMode), withBinaryFile)

-- | Number of leading bytes inspected when sniffing for binary content.
binarySniffBytes :: Int
binarySniffBytes = 8192

-- | Skip line-counting on files larger than this (bytes). For large text
-- files the line count is reported as "unknown (file too large)" so the
-- tool stays cheap regardless of input.
maxLineCountBytes :: Integer
maxLineCountBytes = 5 * 1024 * 1024 -- 5 MB

-- | Cap on directory entry counts in the summary, so listing a huge
-- directory doesn't itself burn the context window.
maxDirEntriesCounted :: Int
maxDirEntriesCounted = 10000

newtype FileInfoToolArgs = FileInfoToolArgs
  { _fiPath :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec FileInfoToolArgs)

instance AC.HasCodec FileInfoToolArgs where
  codec :: AC.JSONCodec FileInfoToolArgs
  codec =
    AC.object "get metadata about a file or directory" $
      FileInfoToolArgs
        <$> AC.requiredField "path" "Relative path to inspect"
          AC..= (._fiPath)

fileInfoToolTyped :: FsConfig -> TypedTool ctx FileInfoToolArgs
fileInfoToolTyped cfg =
  TypedTool
    { ttoolName = "file_info",
      ttoolDescription =
        "Get metadata about a file or directory at a workspace-relative path. "
          <> "For files: kind (text/binary), size in bytes, line count (text files up to 5 MB), "
          <> "permissions, modification time, and whether it is a symbolic link. "
          <> "For directories: entry count (visible vs. total), permissions, modification time. "
          <> "Reports clearly if the path does not exist. "
          <> "Use this before read_file_paginated to decide whether a file is worth reading.",
      ttoolReadonly = True,
      ttoolExecute = const (execute cfg)
    }

execute :: FsConfig -> FileInfoToolArgs -> IO Text
execute cfg args = do
  let rel = args._fiPath
  resolved <- sandboxPath cfg (T.unpack rel)
  exists <- doesPathExist resolved
  if not exists
    then pure $ "Error: path does not exist: " <> rel
    else do
      isDir <- doesDirectoryExist resolved
      isFile <- doesFileExist resolved
      isLink <- safePathIsSymbolicLink resolved
      perms <- safeGetPermissions resolved
      mtime <- safeGetModificationTime resolved
      case (isDir, isFile) of
        (True, _) -> renderDirInfo rel resolved isLink perms mtime
        (_, True) -> renderFileInfo rel resolved isLink perms mtime
        _ -> pure $ "Error: path is neither a regular file nor a directory: " <> rel

-- ---------------------------------------------------------------------------
-- File rendering
-- ---------------------------------------------------------------------------

renderFileInfo ::
  Text ->
  FilePath ->
  Bool ->
  Maybe Permissions ->
  Maybe Text ->
  IO Text
renderFileInfo rel resolved isLink perms mtime = do
  size <- safeGetFileSize resolved
  isBin <- detectBinary resolved
  lineCount <-
    if isBin
      then pure Nothing
      else case size of
        Just n | n <= maxLineCountBytes -> safeLineCount resolved
        _ -> pure Nothing
  let header =
        "=== "
          <> rel
          <> " | file"
          <> (if isLink then " (symlink)" else "")
          <> " ==="
      lns =
        [ "kind: " <> if isBin then "binary" else "text",
          "size: " <> maybe "unknown" (\n -> T.pack (show n) <> " bytes") size,
          "lines: "
            <> case (isBin, lineCount, size) of
              (True, _, _) -> "n/a (binary)"
              (False, Just n, _) -> T.pack (show n)
              (False, Nothing, Just n)
                | n > maxLineCountBytes ->
                    "unknown (file larger than " <> T.pack (show maxLineCountBytes) <> " bytes)"
              _ -> "unknown",
          permsLine perms,
          "modified: " <> fromMaybeText "unknown" mtime
        ]
  pure $ T.unlines (header : lns)

-- ---------------------------------------------------------------------------
-- Directory rendering
-- ---------------------------------------------------------------------------

renderDirInfo ::
  Text ->
  FilePath ->
  Bool ->
  Maybe Permissions ->
  Maybe Text ->
  IO Text
renderDirInfo rel resolved isLink perms mtime = do
  entries <- safeListDirectory resolved
  let total = length entries
      visible = length (filter (not . isHidden) entries)
      countText n
        | n >= maxDirEntriesCounted = T.pack (show maxDirEntriesCounted) <> "+"
        | otherwise = T.pack (show n)
      header =
        "=== "
          <> rel
          <> " | directory"
          <> (if isLink then " (symlink)" else "")
          <> " ==="
      lns =
        [ "entries (visible): " <> countText visible,
          "entries (total):   " <> countText total,
          permsLine perms,
          "modified: " <> fromMaybeText "unknown" mtime
        ]
  pure $ T.unlines (header : lns)

isHidden :: FilePath -> Bool
isHidden ('.' : _) = True
isHidden _ = False

permsLine :: Maybe Permissions -> Text
permsLine Nothing = "permissions: unknown"
permsLine (Just p) =
  let bit b c = if b then T.singleton c else "-"
   in "permissions: "
        <> bit (readable p) 'r'
        <> bit (writable p) 'w'
        <> bit (executable p) 'x'
        <> (if searchable p then " (searchable)" else "")

fromMaybeText :: Text -> Maybe Text -> Text
fromMaybeText d Nothing = d
fromMaybeText _ (Just t) = t

-- ---------------------------------------------------------------------------
-- Safe IO wrappers
-- ---------------------------------------------------------------------------

safeGetFileSize :: FilePath -> IO (Maybe Integer)
safeGetFileSize p = do
  r <- try (getFileSize p) :: IO (Either IOException Integer)
  pure (eitherToMaybe r)

safeGetModificationTime :: FilePath -> IO (Maybe Text)
safeGetModificationTime p = do
  r <- try (getModificationTime p) :: IO (Either IOException UTCTime)
  pure $ case r of
    Right t -> Just (T.pack (show t))
    Left _ -> Nothing

safeGetPermissions :: FilePath -> IO (Maybe Permissions)
safeGetPermissions p = do
  r <- try (getPermissions p) :: IO (Either IOException Permissions)
  pure (eitherToMaybe r)

safePathIsSymbolicLink :: FilePath -> IO Bool
safePathIsSymbolicLink p = do
  r <- try (pathIsSymbolicLink p) :: IO (Either IOException Bool)
  pure $ case r of
    Right b -> b
    Left _ -> False

safeListDirectory :: FilePath -> IO [FilePath]
safeListDirectory p = do
  r <- try (listDirectory p) :: IO (Either IOException [FilePath])
  pure $ case r of
    Right xs -> xs
    Left _ -> []

safeLineCount :: FilePath -> IO (Maybe Int)
safeLineCount p = do
  r <- try (TIO.readFile p) :: IO (Either IOException Text)
  pure $ case r of
    Right content -> Just (length (T.lines content))
    Left _ -> Nothing

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Right a) = Just a
eitherToMaybe (Left _) = Nothing

-- | NUL byte in the first few KB is a robust, cheap binary heuristic
-- (the same one used by git diff and grep).
detectBinary :: FilePath -> IO Bool
detectBinary path = do
  r <-
    try (withBinaryFile path ReadMode (`BS.hGet` binarySniffBytes)) ::
      IO (Either IOException BS.ByteString)
  pure $ case r of
    Right bs -> BS.elem 0 bs
    Left _ -> True
