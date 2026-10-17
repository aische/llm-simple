module LLM.Core.LLMProvider
  ( LLMProvider (..),
    toGateway,
    genericGenerateText,
    genericStreamText,
  )
where

import Control.Exception (try)
import Data.Aeson (Value, eitherDecodeStrict')
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import LLM.Core.Types
  ( ChatRequest (reqTools),
    LLMError (HttpError, NetworkError, ParseError),
    LLMGateway (..),
    LLMHooks (..),
    LLMObjectResult,
    LLMResult,
    LLMTextResult,
    StreamEvent,
  )
import LLM.Core.Utils (streamResponseJson)
import Network.HTTP.Req (HttpException)

data LLMProvider = LLMProvider
  { -- | Provider name for logging/hooks (e.g. "claude", "gemini", "openai")
    providerName :: Text,
    -- | MIME types of inline images the provider accepts (see 'gwImageMediaTypes').
    providerImageMediaTypes :: [Text],
    -- | Whether the provider accepts URL images (see 'gwImageUrlSupport').
    providerImageUrlSupport :: Bool,
    -- | Build the JSON request body. Bool indicates whether streaming is requested.
    buildBody :: Bool -> ChatRequest -> Value,
    -- | Make a non-streaming HTTP call, returning (status code, raw response
    -- body). Must not throw on a non-2xx status.
    sendRequest :: Value -> IO (Int, ByteString),
    -- | Make a streaming HTTP call. On a 200 response the continuation receives
    -- a reader over the raw body (empty chunk = EOF); other statuses become an
    -- 'HttpError' without calling it.
    openStream :: Value -> (IO ByteString -> IO LLMTextResult) -> IO LLMTextResult,
    -- | Parse a raw streaming body. Receives the request body (e.g. for the
    -- model name), the body reader and the stream event callback.
    parseStream :: Value -> IO ByteString -> (StreamEvent -> IO ()) -> IO LLMTextResult,
    -- | Parse a complete (non-streaming) JSON response body.
    parseResponse :: Value -> IO LLMTextResult,
    -- | Build the JSON request body for object generation.
    buildObjectBody :: ChatRequest -> Value -> Value,
    -- | Make a non-streaming HTTP call for object generation, returning
    -- (status code, raw response body).
    sendObjectRequest :: Value -> IO (Int, ByteString),
    -- | Parse a complete JSON response body for object generation.
    parseObjectResponse :: Value -> IO LLMObjectResult
  }

-- | Generic streaming chat via the LLMProvider.
genericStreamText :: LLMProvider -> LLMHooks -> ChatRequest -> (StreamEvent -> IO ()) -> IO LLMTextResult
genericStreamText p hooks r callback = do
  let body = p.buildBody True r
  hooks.onLLMRequest p.providerName body
  result <- try (p.openStream body (\readChunk -> p.parseStream body readChunk callback))
  case result of
    Left e -> do
      hooks.onLLMResponseError p.providerName (T.pack (show (e :: HttpException)))
      pure $ Left $ NetworkError (T.pack (show (e :: HttpException)))
    Right r' -> do
      case r' of
        Right resp -> do
          hooks.onLLMResponse p.providerName (streamResponseJson resp)
          pure r'
        Left e -> do
          hooks.onLLMResponseError p.providerName (T.pack (show e))
          pure $ Left e

-- | Generic non-streaming chat via the LLMProvider.
genericGenerateText :: LLMProvider -> LLMHooks -> ChatRequest -> IO LLMTextResult
genericGenerateText p hooks r = do
  let body = p.buildBody False r
  hooks.onLLMRequest p.providerName body
  result <- try (p.sendRequest body)
  case result of
    Left e -> do
      hooks.onLLMResponseError p.providerName (T.pack (show (e :: HttpException)))
      pure $ Left $ NetworkError (T.pack (show (e :: HttpException)))
    Right resp -> handleResponse p hooks p.parseResponse resp

-- | Generic object generation via the LLMProvider.
genericGenerateObject :: LLMProvider -> LLMHooks -> Value -> ChatRequest -> IO LLMObjectResult
genericGenerateObject p hooks schema r = do
  let body = p.buildObjectBody r {reqTools = []} schema
  hooks.onLLMRequest p.providerName body
  result <- try (p.sendObjectRequest body)
  case result of
    Left e -> do
      hooks.onLLMResponseError p.providerName (T.pack (show (e :: HttpException)))
      pure $ Left $ NetworkError (T.pack (show (e :: HttpException)))
    Right resp -> handleResponse p hooks p.parseObjectResponse resp

-- | Turn a non-streaming (status, raw body) into a result. A non-200 status
-- is an 'HttpError' carrying the body as text, whether or not it is JSON
-- (an HTML 502 from a proxy keeps its status). A 200 body that is not JSON
-- is a 'ParseError'. 'onLLMResponse' sees every body that decodes as JSON.
handleResponse :: LLMProvider -> LLMHooks -> (Value -> IO (LLMResult a)) -> (Int, ByteString) -> IO (LLMResult a)
handleResponse p hooks parse (status, raw) = do
  let decoded = eitherDecodeStrict' raw
  either (const (pure ())) (hooks.onLLMResponse p.providerName) decoded
  if status /= 200
    then do
      let bodyText = decodeUtf8With lenientDecode raw
      hooks.onLLMResponseError p.providerName ("HTTP error: " <> T.pack (show status) <> " " <> bodyText)
      pure $ Left $ HttpError status bodyText
    else case decoded of
      Left err -> do
        let msg = "response is not valid JSON: " <> T.pack err
        hooks.onLLMResponseError p.providerName msg
        pure $ Left $ ParseError msg
      Right v -> parse v

-- | Convert any LLMProvider instance into a LLMGateway.
-- LLMHooks are not baked in — they are passed at call time.
toGateway :: LLMProvider -> LLMGateway
toGateway p =
  LLMGateway
    { gwName = p.providerName,
      gwImageMediaTypes = p.providerImageMediaTypes,
      gwImageUrlSupport = p.providerImageUrlSupport,
      gwGenerateText = genericGenerateText p,
      gwStreamText = genericStreamText p,
      gwGenerateObject = genericGenerateObject p
    }
