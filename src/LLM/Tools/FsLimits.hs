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
    binarySniffBytes,
    detectBinary,
    readBoundedTextFile,
  )
where

import Control.Exception (IOException, try)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.IO (IOMode (ReadMode), hFileSize, withBinaryFile)

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

-- | Number of leading bytes inspected when sniffing for binary content.
-- Matches the heuristic used by @git diff@ and @grep@: any NUL byte in
-- the prefix means binary.
binarySniffBytes :: Int
binarySniffBytes = 8192

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

-- | Read a text file up to @cap@ bytes. Refuses binary files, checks size
-- before reading, and performs a bounded read to mitigate TOCTOU growth.
readBoundedTextFile :: FilePath -> Text -> Int -> IO (Either Text Text)
readBoundedTextFile path display cap = do
  isBinary <- detectBinary path
  if isBinary
    then
      pure $
        Left $
          display
            <> " appears to be a binary file; only text files are supported."
    else do
      sizeRes <-
        try (withBinaryFile path ReadMode hFileSize) ::
          IO (Either IOException Integer)
      case sizeRes of
        Left e ->
          pure $
            Left $
              "could not stat "
                <> display
                <> ": "
                <> T.pack (show e)
        Right sz
          | sz > fromIntegral cap ->
              pure $
                Left $
                  display
                    <> " is "
                    <> T.pack (show sz)
                    <> " bytes, which exceeds the "
                    <> T.pack (show cap)
                    <> "-byte cap."
          | otherwise -> do
              bsRes <-
                try (withBinaryFile path ReadMode (`BS.hGet` (cap + 1))) ::
                  IO (Either IOException BS.ByteString)
              case bsRes of
                Left e ->
                  pure $
                    Left $
                      "could not read "
                        <> display
                        <> ": "
                        <> T.pack (show e)
                Right bs
                  | BS.length bs > cap ->
                      pure $
                        Left $
                          display
                            <> " grew past the "
                            <> T.pack (show cap)
                            <> "-byte cap during the read."
                  | otherwise ->
                      pure (Right (decodeUtf8With lenientDecode bs))
