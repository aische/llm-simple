-- | Text reads on one open 'Handle': the binary sniff, the size check and
-- the read itself all use the same open file, instead of reopening the
-- path for each step. Shared by 'LLM.Tools.FsLimits' and the tools.
module LLM.Tools.Internal.TextFile
  ( Opener,
    binarySniffBytes,
    withTextHandle,
    CappedRead (..),
    readCapped,
    readCappedWith,
    readBoundedBytesWith,
    readBoundedBytesWithSize,
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

-- | What 'readCapped' found.
data CappedRead
  = -- | The file is over the cap at the size check; its size.
    CappedTooLarge Integer
  | -- | The file was within the cap at the size check but went past it
    -- before the read ended.
    CappedGrew
  | -- | The size at the check, and the file's bytes.
    CappedBytes Integer BS.ByteString
  deriving (Eq, Show)

-- | Check the size of an open file, then read it, refusing more than @cap@
-- bytes. Reads one byte past the cap, so a file that grows past it after
-- the size check is reported as 'CappedGrew' instead of looking like it
-- ended at the cap.
readCapped :: Integer -> Handle -> IO CappedRead
readCapped = readCappedWith hFileSize

-- | 'readCapped' with the size check given, so a test can stand in for a
-- file that grows between the check and the read.
readCappedWith :: (Handle -> IO Integer) -> Integer -> Handle -> IO CappedRead
readCappedWith sizeOf cap h = do
  size <- sizeOf h
  if size > cap
    then pure (CappedTooLarge size)
    else do
      bytes <- BS.hGet h (fromIntegral cap + 1)
      pure $
        if fromIntegral (BS.length bytes) > cap
          then CappedGrew
          else CappedBytes size bytes

-- | Read a file up to @cap@ bytes. Refuses binary files, checks the size
-- before reading, and reads at most @cap + 1@ bytes so a file that grows
-- during the read is caught ('readCapped').
readBoundedBytesWith :: Opener -> Text -> Int -> IO (Either Text BS.ByteString)
readBoundedBytesWith = readBoundedBytesWithSize hFileSize

-- | 'readBoundedBytesWith' with the size check given, so a test can stand
-- in for a file that grows between the check and the read. An
-- 'IOException' from the size check or the read becomes an error naming
-- that step.
readBoundedBytesWithSize :: (Handle -> IO Integer) -> Opener -> Text -> Int -> IO (Either Text BS.ByteString)
readBoundedBytesWithSize sizeOf open display cap = do
  r <- withTextHandle open $ \h -> do
    sizeRes <- try (sizeOf h)
    case sizeRes of
      Left (e :: IOException) ->
        pure $ Left $ "could not stat " <> display <> ": " <> describeIOError e
      Right sz -> do
        readRes <- try (readCappedWith (const (pure sz)) (fromIntegral cap) h)
        pure $ case readRes of
          Left (e :: IOException) ->
            Left $ "could not read " <> display <> ": " <> describeIOError e
          Right (CappedTooLarge _) ->
            Left $
              display
                <> " is "
                <> T.pack (show sz)
                <> " bytes, which exceeds the "
                <> T.pack (show cap)
                <> "-byte cap."
          Right CappedGrew ->
            Left $
              display
                <> " grew past the "
                <> T.pack (show cap)
                <> "-byte cap during the read."
          Right (CappedBytes _ bs) -> Right bs
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
