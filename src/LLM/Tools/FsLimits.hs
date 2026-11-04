-- | Shared limits and helpers for filesystem tools.
--
-- Centralizing these constants prevents drift between tools (e.g. one tool
-- having a higher binary-sniff threshold than another) and makes it
-- straightforward to retune the sandbox's resource posture in a single
-- place.
--
-- Two walk depths are exposed deliberately:
--
-- * 'cheapWalkDepth' for traversals that only stat directories
--   (@find_files@, @directory_tree@).
-- * 'expensiveWalkDepth' for traversals that read every encountered file
--   (@grep@). The lower bound bounds the worst-case I/O fan-out.
--
-- Collapsing the two into a single constant would either make @grep@ pay
-- for very deep vendored trees or make @find_files@ miss legitimate
-- matches; keeping both behind named helpers preserves the intent.
module LLM.Tools.FsLimits
  ( cheapWalkDepth,
    expensiveWalkDepth,
    maxReadBytes,
    maxPaginatedFileBytes,
    maxTreeEntries,
    maxVisitedEntries,
    maxOutputBytes,
    maxSearchBytes,
    binarySniffBytes,
    detectBinary,
    readBoundedTextFile,
    readBoundedUtf8File,
    readTextFileLenient,
    writeUtf8File,
    atomicWriteWith,
  )
where

import Control.Exception (IOException, bracketOnError, try)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import LLM.Tools.Internal.TextFile
  ( readBoundedTextWith,
    readBoundedUtf8With,
  )
import LLM.Tools.Internal.TextFile qualified as TextFile
import System.Directory (copyPermissions, doesFileExist, removeFile, renameFile)
import System.FilePath (splitFileName)
import System.IO
  ( Handle,
    IOMode (ReadMode),
    hClose,
    openBinaryTempFileWithDefaultPermissions,
    withBinaryFile,
  )

-- | Maximum directory depth for traversals that only stat entries.
cheapWalkDepth :: Int
cheapWalkDepth = 20

-- | Maximum directory depth for traversals that read every encountered
-- file (e.g. @grep@). Lower than 'cheapWalkDepth' because the per-dir
-- work is dominated by file I/O rather than @readdir@.
expensiveWalkDepth :: Int
expensiveWalkDepth = 12

-- | Hard cap on the number of bytes a single @readfile@-style tool may
-- materialize into memory. Paginated readers should use their own
-- per-page limits; this is the ceiling for "give me the whole thing"
-- tools that have no pagination story.
maxReadBytes :: Int
maxReadBytes = 1024 * 1024 -- 1 MiB

-- | Maximum source-file size for @read_file_paginated@. Higher than
-- 'maxReadBytes' because pagination exists to walk large files; this cap
-- still bounds worst-case line-skipping work.
maxPaginatedFileBytes :: Integer
maxPaginatedFileBytes = 10 * 1024 * 1024 -- 10 MiB

-- | Hard cap on the number of entries a @directory_tree@-style tool may
-- emit. Output is truncated past this point with a marker in the header.
maxTreeEntries :: Int
maxTreeEntries = 2000

-- | Budget of directory entries one call of a walking tool (@find_files@,
-- @grep@, @directory_tree@) may look at, counted as directories are
-- listed. A walk that runs out stops and says so in its output, so a large
-- tree costs a bounded amount of work even when nothing matches.
maxVisitedEntries :: Int
maxVisitedEntries = 20000

-- | Cap on the bytes of text one filesystem tool result returns (a page of
-- @read_file_paginated@, the lines of a listing or search). Output past it
-- is cut and the cut is reported.
maxOutputBytes :: Int
maxOutputBytes = 100 * 1024 -- 100 KiB

-- | Budget of file bytes one @grep@ call may read. Only the text files it
-- searches count; binary files and files over the per-file size cap are
-- skipped without being read. A search that runs out stops before the
-- next file and says so in its output.
--
-- @since 0.4.3.0
maxSearchBytes :: Int
maxSearchBytes = 64 * 1024 * 1024 -- 64 MiB

-- | Number of leading bytes inspected when sniffing for binary content.
-- Matches the heuristic used by @git diff@ and @grep@: any NUL byte in
-- the prefix means binary.
binarySniffBytes :: Int
binarySniffBytes = TextFile.binarySniffBytes

-- | A NUL byte in the first 'binarySniffBytes' is a robust, cheap binary
-- heuristic. On I/O failure the file is treated as binary (the caller
-- should fail closed rather than read an unreadable file as text).
detectBinary :: FilePath -> IO Bool
detectBinary path = do
  r <-
    try (withBinaryFile path ReadMode (`BS.hGet` binarySniffBytes)) ::
      IO (Either IOException BS.ByteString)
  case r of
    Right bs -> pure (BS.elem 0 bs)
    Left _ -> pure True

-- | Read a text file up to @cap@ bytes for display. Invalid UTF-8 bytes
-- become U+FFFD, so never write the result back; edit tools use
-- 'readBoundedUtf8File'.
readBoundedTextFile :: FilePath -> Text -> Int -> IO (Either Text Text)
readBoundedTextFile path = readBoundedTextWith (withBinaryFile path ReadMode)

-- | Read a text file up to @cap@ bytes for editing. Refuses files that are
-- not valid UTF-8, since writing them back would destroy their bytes.
readBoundedUtf8File :: FilePath -> Text -> Int -> IO (Either Text Text)
readBoundedUtf8File path = readBoundedUtf8With (withBinaryFile path ReadMode)

-- | Read a whole file as UTF-8, replacing invalid bytes with U+FFFD.
-- Independent of the locale. For read-only use.
readTextFileLenient :: FilePath -> IO Text
readTextFileLenient path = decodeUtf8With lenientDecode <$> BS.readFile path

-- | Write text as UTF-8, independent of the locale. The file is replaced
-- atomically ('atomicWriteWith').
writeUtf8File :: FilePath -> Text -> IO ()
writeUtf8File path t = atomicWriteWith path (`BS.hPut` encodeUtf8 t)

-- | Replace the file at @path@ with what the action writes, so that readers
-- see either the old content or the new content, never a partial file.
--
-- The action writes to a temporary file in the same directory, which is
-- then closed and renamed over @path@. If @path@ already exists, its mode
-- is copied to the new file first ('copyPermissions': the full mode on
-- POSIX, the read-only attribute on Windows); a new file gets the default
-- mode (@0666@ minus the umask). On any exception the temporary file is
-- removed and the exception rethrown, leaving @path@ unchanged.
--
-- Limits:
--
-- * The data is flushed to the operating system but not @fsync@ed, so a
--   power loss shortly after the write can still lose it.
-- * The file gets a new inode. A hard link to the old file keeps the old
--   content, and ACLs and extended attributes are not copied. The new file
--   is owned by the user running the process.
-- * On Windows, the rename fails while another process has @path@ open.
atomicWriteWith :: FilePath -> (Handle -> IO ()) -> IO ()
atomicWriteWith path write =
  bracketOnError
    (openBinaryTempFileWithDefaultPermissions dir ("." <> name <> ".tmp"))
    ( \(tmp, h) -> do
        _ <- try @IOException (hClose h)
        _ <- try @IOException (removeFile tmp)
        pure ()
    )
    ( \(tmp, h) -> do
        write h
        hClose h
        exists <- doesFileExist path
        when exists $ copyPermissions path tmp
        renameFile tmp path
    )
  where
    (dirRaw, name) = splitFileName path
    dir = if null dirRaw then "." else dirRaw
