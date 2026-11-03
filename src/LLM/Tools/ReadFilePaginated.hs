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
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, sandboxPath, withWorkspacePaths)
import LLM.Tools.FsLimits (maxOutputBytes, maxPaginatedFileBytes)
import LLM.Tools.Internal.TextFile (withTextHandle)
import LLM.Tools.Internal.WalkBudget (utf8Prefix)
import System.IO
  ( IOMode (ReadMode),
    hFileSize,
    withBinaryFile,
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
          <> "to walk through large files one page at a time. Source files larger than "
          <> T.pack (show maxPaginatedFileBytes)
          <> " bytes are refused. A page holds at most "
          <> T.pack (show maxOutputBytes)
          <> " bytes of text; a longer line is cut and marked. The response header reports the "
          <> "file's byte size, the line range returned, and whether more lines remain along "
          <> "with the next offset to use. The tool is stateless: pass the next offset on the "
          <> "following call. Binary files are refused.",
      ttoolReadonly = True,
      ttoolExecute = const (withWorkspacePaths cfg . execute cfg)
    }

execute :: FsConfig -> ReadFilePaginatedArgs -> IO Text
execute cfg args
  | args._rfpOffset < 1 =
      pure $ "Error: offset is a 1-based line number and must be at least 1, got " <> T.pack (show args._rfpOffset)
  | args._rfpLimit < 1 =
      pure $ "Error: limit must be positive, got " <> T.pack (show args._rfpLimit)
  | otherwise = do
      let rel = T.unpack args._rfpPath
          offset = args._rfpOffset
          limit = min maxLimit args._rfpLimit
          path = args._rfpPath
          capText = T.pack (show maxPaginatedFileBytes)
      resolved <- sandboxPath cfg rel
      result <- withTextHandle (withBinaryFile resolved ReadMode) $ \h -> do
        size <- hFileSize h
        if size > maxPaginatedFileBytes
          then
            pure . Left $
              path
                <> " is "
                <> T.pack (show size)
                <> " bytes, which exceeds the "
                <> capText
                <> "-byte cap for read_file_paginated."
          else do
            -- Read one byte past the cap, so a file that grew past it since
            -- the size check is reported instead of looking like it ended.
            bytes <- BS.hGet h (fromIntegral maxPaginatedFileBytes + 1)
            pure $
              if fromIntegral (BS.length bytes) > maxPaginatedFileBytes
                then
                  Left $
                    path
                      <> " grew past the "
                      <> capText
                      <> "-byte cap for read_file_paginated during the read."
                else
                  let (lns, hasMore) = takePage offset limit bytes
                   in Right (size, lns, hasMore)
      pure $ case result of
        Nothing ->
          "Error: "
            <> path
            <> " appears to be a binary file; read_file_paginated only supports text files."
        Just (Left err) -> "Error: " <> err
        Just (Right (size, lns, hasMore)) ->
          renderPage path size offset lns hasMore

-- | Take up to @limit@ lines starting at line @offset@, and report whether
-- more lines follow.
--
-- Lines are found on the raw bytes (a newline byte never occurs inside a
-- UTF-8 sequence), so the lines before @offset@ are skipped without being
-- decoded. Only the page is decoded, leniently: invalid UTF-8 bytes show
-- as U+FFFD instead of failing the read.
--
-- The page holds at most 'maxOutputBytes' bytes of line text. It ends
-- before a line that would go over, unless that line is the first one;
-- then it is cut and marked, and the next page starts at the next line.
takePage :: Int -> Int -> BS.ByteString -> ([Text], Bool)
takePage offset limit = go 0 0 [] . dropLines (offset - 1)
  where
    go :: Int -> Int -> [Text] -> BS.ByteString -> ([Text], Bool)
    go n used acc bs
      | BS.null bs = (reverse acc, False)
      | n >= limit = (reverse acc, True)
      | used + len > maxOutputBytes =
          if null acc
            then ([cutLine line], not (BS.null rest))
            else (reverse acc, True)
      | otherwise = go (n + 1) (used + len) (decode line : acc) rest
      where
        (line, rest0) = BS.break (== newline) bs
        rest = BS.drop 1 rest0
        len = BS.length line

    dropLines :: Int -> BS.ByteString -> BS.ByteString
    dropLines k bs
      | k <= 0 = bs
      | otherwise = case BS.elemIndex newline bs of
          Nothing -> BS.empty
          Just i -> dropLines (k - 1) (BS.drop (i + 1) bs)

    cutLine line =
      decode (utf8Prefix maxOutputBytes line)
        <> " [line cut: "
        <> T.pack (show (BS.length line))
        <> " bytes, first "
        <> T.pack (show maxOutputBytes)
        <> " shown]"

    decode = decodeUtf8With lenientDecode
    newline = 10

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

