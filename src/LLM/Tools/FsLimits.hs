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
    maxTreeEntries,
    binarySniffBytes,
    detectBinary,
  )
where

import Control.Exception (IOException, try)
import Data.ByteString qualified as BS
import System.IO (IOMode (ReadMode), withBinaryFile)

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
