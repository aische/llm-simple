module LLM.Core.SSE (SSEEvent (..), readSSEEvents) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE

data SSEEvent = SSEEvent
  { sseEvent :: Maybe Text,
    sseData :: Text
  }
  deriving (Show)

-- | Read Server-Sent Events from a streaming body reader.
-- Calls the callback for each complete event. Returns when the stream ends.
readSSEEvents :: IO ByteString -> (SSEEvent -> IO ()) -> IO ()
readSSEEvents readChunk callback = do
  bufRef <- newIORef BS.empty
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
            chunk <- readChunk
            if BS.null chunk
              then
                if BS.null buf
                  then pure Nothing
                  else do
                    writeIORef bufRef BS.empty
                    pure (Just buf)
              else do
                writeIORef bufRef (buf <> chunk)
                readLine

      loop :: Maybe Text -> [ByteString] -> IO ()
      loop eventType dataLines = do
        mLine <- readLine
        case mLine of
          Nothing ->
            unless (null dataLines) $
              fireEvent eventType dataLines
          Just line
            | BS.null line -> do
                unless (null dataLines) $
                  fireEvent eventType dataLines
                loop Nothing []
            | "data:" `BS.isPrefixOf` line ->
                loop eventType (stripField 5 line : dataLines)
            | "event:" `BS.isPrefixOf` line ->
                loop (Just (TE.decodeUtf8 (stripField 6 line))) dataLines
            | ":" `BS.isPrefixOf` line ->
                loop eventType dataLines -- comment, skip
            | otherwise ->
                loop eventType dataLines

      stripField :: Int -> ByteString -> ByteString
      stripField n bs =
        let rest = BS.drop n bs
         in if not (BS.null rest) && BC.head rest == ' '
              then BS.tail rest
              else rest

      fireEvent :: Maybe Text -> [ByteString] -> IO ()
      fireEvent eventType dataLines =
        callback
          SSEEvent
            { sseEvent = eventType,
              sseData = TE.decodeUtf8 (BS.intercalate "\n" (reverse dataLines))
            }
  loop Nothing []
