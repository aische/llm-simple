-- | Bounded reading of provider response bodies.
--
-- A 200 body larger than 'maxResponseBodyBytes' (a stream larger than it
-- in total, or a stream event larger than 'maxStreamEventBytes') is
-- signalled with 'ResponseTooLarge', which
-- is not exported from any public module. The @generic*@ functions in
-- 'LLM.Core.LLMProvider' catch it and return a v'LLM.Core.Types.ParseError'; the type of
-- 'LLM.Core.LLMProvider.sendRequest' stays unchanged.
module LLM.Core.Internal.Body
  ( ResponseTooLarge (..),
    responseTooLargeMessage,
    maxErrorBodyBytes,
    maxResponseBodyBytes,
    maxStreamEventBytes,
    readBodyPrefix,
    readErrorBody,
  )
where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T

-- | A response body or stream event exceeded the given number of bytes.
newtype ResponseTooLarge = ResponseTooLarge Int
  deriving (Show)

instance Exception ResponseTooLarge

-- | The v'LLM.Core.Types.ParseError' text for 'ResponseTooLarge'.
responseTooLargeMessage :: ResponseTooLarge -> Text
responseTooLargeMessage (ResponseTooLarge n) =
  "response body exceeds " <> T.pack (show n) <> " bytes"

-- | Bytes of a non-200 body kept for the v'LLM.Core.Types.HttpError' message.
maxErrorBodyBytes :: Int
maxErrorBodyBytes = 64 * 1024

-- | Largest 200 body a non-streaming call accepts, and the largest total
-- size of a stream.
maxResponseBodyBytes :: Int
maxResponseBodyBytes = 32 * 1024 * 1024

-- | Largest single stream event (one SSE event, or one line of one).
maxStreamEventBytes :: Int
maxStreamEventBytes = 8 * 1024 * 1024

-- | Read at most @cap@ bytes from a body reader (empty chunk = end). Returns
-- the bytes read and whether the body went on past @cap@. Stops reading at
-- the cap, so an endless body costs at most @cap@ plus one chunk.
readBodyPrefix :: Int -> IO ByteString -> IO (ByteString, Bool)
readBodyPrefix cap readChunk = go 0 []
  where
    go n acc = do
      chunk <- readChunk
      if BS.null chunk
        then pure (BS.concat (reverse acc), False)
        else do
          let n' = n + BS.length chunk
          if n' > cap
            then pure (BS.concat (reverse (BS.take (cap - n) chunk : acc)), True)
            else go n' (chunk : acc)

-- | Read an error body up to 'maxErrorBodyBytes', with @...@ appended when
-- it was cut.
readErrorBody :: IO ByteString -> IO ByteString
readErrorBody readChunk = do
  (bs, cut) <- readBodyPrefix maxErrorBodyBytes readChunk
  pure (if cut then bs <> "..." else bs)
