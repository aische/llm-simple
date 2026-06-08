module LLM.Tools.ReadFilePaginated
  ( readFilePaginatedToolTyped,
    ReadFilePaginatedArgs (..),
  )
where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, sandboxPath)
import LLM.Tools.FsLimits (binarySniffBytes)
import System.IO
  ( Handle,
    IOMode (ReadMode),
    hFileSize,
    hIsEOF,
    hSetEncoding,
    utf8,
    withBinaryFile,
    withFile,
  )

-- | Default number of lines returned per page when the caller omits 'limit'.
defaultLimit :: Int
defaultLimit = 200

-- | Hard cap on the number of lines returned per page, to protect the
-- model's context window from runaway responses on pathological inputs.
maxLimit :: Int
maxLimit = 2000

data ReadFilePaginatedArgs = ReadFilePaginatedArgs
  { _rfpPath :: Text,
    _rfpOffset :: Int,
    _rfpLimit :: Int
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec ReadFilePaginatedArgs)

instance AC.HasCodec ReadFilePaginatedArgs where
  codec =
    AC.object "read a page of a text file" $
      ReadFilePaginatedArgs
        <$> AC.requiredField "path" "Relative file path to read"
          AC..= (._rfpPath)
        <*> AC.optionalFieldWithDefault
          "offset"
          1
          "1-based line number to start reading from (default 1)"
          AC..= (._rfpOffset)
        <*> AC.optionalFieldWithDefault
          "limit"
          defaultLimit
          "Maximum number of lines to return in this page (default 200, hard cap 2000)"
          AC..= (._rfpLimit)

readFilePaginatedToolTyped :: FsConfig -> TypedTool ctx ReadFilePaginatedArgs
readFilePaginatedToolTyped cfg =
  TypedTool
    { ttoolName = "read_file_paginated",
      ttoolDescription =
        "Read a slice of a text file by line range (path relative to the workspace). "
          <> "Use 'offset' (1-based line number, default 1) and 'limit' (default 200, max 2000) "
          <> "to walk through large files one page at a time. The response header reports the "
          <> "file's byte size, the line range returned, and whether more lines remain along "
          <> "with the next offset to use. The tool is stateless: pass the next offset on the "
          <> "following call. Binary files are refused.",
      ttoolReadonly = True,
      ttoolExecute = const (execute cfg)
    }

execute :: FsConfig -> ReadFilePaginatedArgs -> IO Text
execute cfg args = do
  let rel = T.unpack args._rfpPath
      offset = max 1 args._rfpOffset
      limit = max 1 (min maxLimit args._rfpLimit)
  resolved <- sandboxPath cfg rel
  isBinary <- detectBinary resolved
  if isBinary
    then
      pure $
        "Error: "
          <> args._rfpPath
          <> " appears to be a binary file; read_file_paginated only supports text files."
    else do
      size <- withBinaryFile resolved ReadMode hFileSize
      (lns, hasMore) <- readPage resolved offset limit
      pure $ renderPage args._rfpPath size offset lns hasMore

-- | Skip @offset - 1@ lines, take up to @limit@ lines, then probe one
-- past the last returned line to determine whether more content remains.
-- Reads strictly within 'withFile' so the handle is released eagerly.
readPage :: FilePath -> Int -> Int -> IO ([Text], Bool)
readPage path offset limit = withFile path ReadMode $ \h -> do
  hSetEncoding h utf8
  skipLines h (offset - 1)
  collected <- takeLines h limit
  more <- not <$> hIsEOF h
  pure (collected, more)

skipLines :: Handle -> Int -> IO ()
skipLines _ 0 = pure ()
skipLines h n = do
  eof <- hIsEOF h
  if eof
    then pure ()
    else do
      _ <- TIO.hGetLine h
      skipLines h (n - 1)

takeLines :: Handle -> Int -> IO [Text]
takeLines h n0 = go n0 id
  where
    go 0 acc = pure (acc [])
    go k acc = do
      eof <- hIsEOF h
      if eof
        then pure (acc [])
        else do
          line <- TIO.hGetLine h
          go (k - 1) (acc . (line :))

renderPage :: Text -> Integer -> Int -> [Text] -> Bool -> Text
renderPage path size offset lns hasMore =
  let count = length lns
      lastNo = offset + count - 1
      rangePart =
        if count == 0
          then "no lines at offset " <> T.pack (show offset)
          else "lines " <> T.pack (show offset) <> "-" <> T.pack (show lastNo)
      tailPart =
        if hasMore
          then "more (next offset: " <> T.pack (show (lastNo + 1)) <> ")"
          else "end of file"
      header =
        "=== "
          <> path
          <> " | "
          <> rangePart
          <> " | "
          <> T.pack (show size)
          <> " bytes | "
          <> tailPart
          <> " ==="
      numbered = zipWith fmtLine [offset ..] lns
   in T.unlines (header : numbered)

-- | Right-align the line number in a 6-char gutter for readability and
-- so the model can reliably reference specific lines back to other tools.
fmtLine :: Int -> Text -> Text
fmtLine n line =
  let s = T.pack (show n)
      pad = T.replicate (max 0 (6 - T.length s)) " "
   in pad <> s <> "| " <> line

-- | A NUL byte in the first few KB is a robust, cheap binary heuristic
-- (the same one used by git diff and grep).
detectBinary :: FilePath -> IO Bool
detectBinary path = withBinaryFile path ReadMode $ \h -> do
  bs <- BS.hGet h binarySniffBytes
  pure (BS.elem 0 bs)
