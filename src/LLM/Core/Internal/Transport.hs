-- | The HTTP calls every built-in provider makes: a JSON @POST@ with
-- 'lenientConfig', read with 'readResponseBody' or handed to a stream
-- parser through 'handleStreamResponse'. Each provider keeps its own URL,
-- auth options and body preparation.
module LLM.Core.Internal.Transport
  ( postJson,
    postJsonStream,
  )
where

import Data.Aeson (Value)
import Data.ByteString (ByteString)
import LLM.Core.ProviderUtils (handleStreamResponse, lenientConfig, readResponseBody)
import LLM.Core.Types (LLMTextResult)
import Network.HTTP.Req (Option, POST (POST), ReqBodyJson (ReqBodyJson), Url, reqBr, runReq)

-- | Send a non-streaming request; the result is
-- 'LLM.Core.LLMProvider.sendRequest''s @(status, body)@.
postJson :: Url scheme -> Option scheme -> Value -> IO (Int, ByteString)
postJson url opts body =
  runReq lenientConfig $
    reqBr POST url (ReqBodyJson body) opts readResponseBody

-- | Send a streaming request, for 'LLM.Core.LLMProvider.openStream'. On a
-- 200 response the continuation gets the body reader; any other status
-- becomes an 'LLM.Core.Types.HttpError' without calling it.
postJsonStream ::
  Url scheme ->
  Option scheme ->
  Value ->
  (IO ByteString -> IO LLMTextResult) ->
  IO LLMTextResult
postJsonStream url opts body k =
  runReq lenientConfig $
    reqBr POST url (ReqBodyJson body) opts $ \resp ->
      handleStreamResponse resp k
