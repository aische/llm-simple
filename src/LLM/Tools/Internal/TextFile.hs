-- | Text reads on one open 'Handle': the binary sniff, the size check and
-- the read itself all use the same open file, instead of reopening the
-- path for each step. Shared by 'LLM.Tools.FsLimits' and the tools.
module LLM.Tools.Internal.TextFile
  ( Opener,
    binarySniffBytes,
    withTextHandle,
    readBoundedBytesWith,
    readBoundedTextWith,
    readBoundedUtf8With,
    readLenient,
    writeUtf8,
  )
where

import Control.Exception (IOException, throwIO, try)
import GHC.IO.Exception (IOException (ioe_filename))
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8', decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import System.IO (Handle, SeekMode (AbsoluteSeek), hFileSize, hSeek)

-- | Opens a file for reading and runs an action on it, e.g.
-- @withBinaryFile path ReadMode@.
type Opener = forall a. (Handle -> IO a) -> IO a

-- | Number of leading bytes inspected when sniffing for binary content.
-- Matches the heuristic used by @git diff@ and @grep@: any NUL byte in
-- the prefix means binary.
binarySniffBytes :: Int
binarySniffBytes = 8192

-- | Open a file, sniff its first 'binarySniffBytes' for a NUL byte, and run
-- the action from the start of the file if there is none.
--
-- 'Nothing' if the file looks binary, or cannot be opened or sniffed: the
-- caller fails closed rather than read an unreadable file as text. An
-- 'IOException' from the action itself is rethrown.
withTextHandle :: Opener -> (Handle -> IO r) -> IO (Maybe r)
withTextHandle open act = do
  r <- try $ open $ \h -> do
    sniff <- try (BS.hGet h binarySniffBytes)
    case sniff of
      Left (_ :: IOException) -> pure (Right Nothing)
      Right bs
        | BS.elem 0 bs -> pure (Right Nothing)
        | otherwise -> do
            hSeek h AbsoluteSeek 0
            fmap Just <$> try (act h)
  case r of
    Left (_ :: IOException) -> pure Nothing
    Right (Left (e :: IOException)) -> throwIO e
    Right (Right x) -> pure x

-- | Read a file up to @cap@ bytes. Refuses binary files, checks the size
-- before reading, and reads at most @cap + 1@ bytes so a file that grows
-- during the read is caught.
readBoundedBytesWith :: Opener -> Text -> Int -> IO (Either Text BS.ByteString)
readBoundedBytesWith open display cap = do
  r <- withTextHandle open $ \h -> do
    sizeRes <- try (hFileSize h)
    case sizeRes of
      Left (e :: IOException) ->
        pure $ Left $ "could not stat " <> display <> ": " <> describeIOError e
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
            bsRes <- try (BS.hGet h (cap + 1))
            pure $ case bsRes of
              Left (e :: IOException) ->
                Left $ "could not read " <> display <> ": " <> describeIOError e
              Right bs
                | BS.length bs > cap ->
                    Left $
                      display
                        <> " grew past the "
                        <> T.pack (show cap)
                        <> "-byte cap during the read."
                | otherwise -> Right bs
  pure $
    fromMaybe
      (Left (display <> " appears to be a binary file; only text files are supported."))
      r

-- | An 'IOException' without its file name, which for a handle is the
-- absolute path it was opened with. Callers name the file themselves.
describeIOError :: IOException -> Text
describeIOError e = T.pack (show e {ioe_filename = Nothing})

-- | 'readBoundedBytesWith', decoded for display. Invalid UTF-8 bytes become
-- U+FFFD, so never write the result back.
readBoundedTextWith :: Opener -> Text -> Int -> IO (Either Text Text)
readBoundedTextWith open display cap =
  fmap (decodeUtf8With lenientDecode) <$> readBoundedBytesWith open display cap

-- | 'readBoundedBytesWith', for editing. Refuses files that are not valid
-- UTF-8, since writing them back would destroy their bytes.
readBoundedUtf8With :: Opener -> Text -> Int -> IO (Either Text Text)
readBoundedUtf8With open display cap = do
  r <- readBoundedBytesWith open display cap
  pure $
    r >>= \bs -> case decodeUtf8' bs of
      Right t -> Right t
      Left _ -> Left $ display <> " is not valid UTF-8; refusing to edit it."

-- | Read the rest of the handle as UTF-8, replacing invalid bytes with
-- U+FFFD. Independent of the locale.
readLenient :: Handle -> IO Text
readLenient h = decodeUtf8With lenientDecode <$> BS.hGetContents h

-- | Write text as UTF-8, independent of the locale.
writeUtf8 :: Handle -> Text -> IO ()
writeUtf8 h = BS.hPut h . encodeUtf8
