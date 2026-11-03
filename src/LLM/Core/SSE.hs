module LLM.Core.SSE
  ( SSEEvent (..),
    readSSEEvents,
    malformedEvent,
    streamFailure,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import LLM.Core.Internal.Body (ResponseTooLarge (..), maxStreamEventBytes)
import LLM.Core.Types (LLMError (..))

data SSEEvent = SSEEvent
  { sseEvent :: Maybe Text,
    sseData :: Text
  }
  deriving (Show)

-- | Read Server-Sent Events from a streaming body reader.
-- Calls the callback for each complete event. Returns when the stream ends.
--
-- An event whose name or data is not valid UTF-8 is skipped, and reading
-- continues so the caller can still tell whether the stream ended properly.
-- The result describes the first such event, if any.
--
-- An event (or a line) larger than 'maxStreamEventBytes' throws
-- 'ResponseTooLarge', which the @generic*@ functions turn into a
-- 'ParseError'.
readSSEEvents :: IO ByteString -> (SSEEvent -> IO ()) -> IO (Maybe Text)
readSSEEvents readChunk callback = do
  bufRef <- newIORef BS.empty
  badRef <- newIORef Nothing
  let readLine :: IO (Maybe ByteString)
      readLine = do
        buf <- readIORef bufRef
        case BC.elemIndex '\n' buf of
          Just i -> do
            let (line, rest) = BS.splitAt i buf
                line' =
                  if not (BS.null line) && BC.last line == '\r'
                    then BS.init line
                    else line
            writeIORef bufRef (BS.drop 1 rest)
            pure (Just line')
          Nothing -> do
            -- Read until a chunk holds a newline (or the stream ends), then
            -- concatenate once, so a line split into many chunks is copied
            -- once rather than once per chunk.
            (pending, ended) <- readUntilNewline [buf] (BS.length buf)
            let buf' = BS.concat (reverse pending)
            if ended
              then
                if BS.null buf'
                  then pure Nothing
                  else do
                    writeIORef bufRef BS.empty
                    pure (Just buf')
              else do
                writeIORef bufRef buf'
                readLine

      -- Chunks read so far (newest first) and whether the stream ended.
      readUntilNewline :: [ByteString] -> Int -> IO ([ByteString], Bool)
      readUntilNewline acc size = do
        chunk <- readChunk
        if BS.null chunk
          then pure (acc, True)
          else do
            let size' = size + BS.length chunk
            when (size' > maxStreamEventBytes) $
              throwIO (ResponseTooLarge maxStreamEventBytes)
            if BC.elem '\n' chunk
              then pure (chunk : acc, False)
              else readUntilNewline (chunk : acc) size'

      -- dataSize: bytes of data collected for the current event.
      loop :: Maybe ByteString -> [ByteString] -> Int -> IO ()
      loop eventType dataLines dataSize = do
        mLine <- readLine
        case mLine of
          Nothing ->
            unless (null dataLines) $
              fireEvent eventType dataLines
          Just line
            | BS.null line -> do
                unless (null dataLines) $
                  fireEvent eventType dataLines
                loop Nothing [] 0
            | "data:" `BS.isPrefixOf` line -> do
                let d = stripField 5 line
                    dataSize' = dataSize + BS.length d + 1
                when (dataSize' > maxStreamEventBytes) $
                  throwIO (ResponseTooLarge maxStreamEventBytes)
                loop eventType (d : dataLines) dataSize'
            | "event:" `BS.isPrefixOf` line ->
                loop (Just (stripField 6 line)) dataLines dataSize
            | ":" `BS.isPrefixOf` line ->
                loop eventType dataLines dataSize -- comment, skip
            | otherwise ->
                loop eventType dataLines dataSize

      stripField :: Int -> ByteString -> ByteString
      stripField n bs =
        let rest = BS.drop n bs
         in if not (BS.null rest) && BC.head rest == ' '
              then BS.tail rest
              else rest

      fireEvent :: Maybe ByteString -> [ByteString] -> IO ()
      fireEvent eventType dataLines =
        case (traverse TE.decodeUtf8' eventType, TE.decodeUtf8' (BS.intercalate "\n" (reverse dataLines))) of
          (Right name, Right dat) -> callback SSEEvent {sseEvent = name, sseData = dat}
          (Left err, _) -> invalid err
          (_, Left err) -> invalid err
        where
          invalid err =
            modifyIORef' badRef (<|> Just ("stream event is not valid UTF-8: " <> T.pack (show err)))
  loop Nothing [] 0
  readIORef badRef

-- | Message for a data event that should be JSON but is not, with a short
-- excerpt of the data.
malformedEvent :: Text -> Text
malformedEvent raw =
  "stream event is not valid JSON: " <> excerpt
  where
    excerpt
      | T.length raw > 200 = T.take 200 raw <> "..."
      | otherwise = raw

-- | Stream-level failure of a parsed stream, given the name of the
-- protocol's end-of-stream marker, whether it was seen, and the first
-- malformed event (if any).
--
-- Parsers check this after in-band provider errors and before their own
-- content checks. A missing marker is a retryable 'NetworkError': the
-- connection closed early, and a malformed final event is usually just the
-- cut-off tail of that.
streamFailure :: Text -> Bool -> Maybe Text -> Maybe LLMError
streamFailure marker complete malformed
  | not complete = Just (NetworkError ("stream ended before " <> marker))
  | Just msg <- malformed = Just (ParseError msg)
  | otherwise = Nothing
