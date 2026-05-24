module LLM.Core.LLMProvider
  ( LLMProvider (..),
    toGateway,
    genericGenerateText,
    genericStreamText,
  )
where

import Control.Exception (try)
import Data.Aeson (Value)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Types
  ( ChatRequest (reqTools),
    LLMError (HttpError, NetworkError),
    LLMGateway (..),
    LLMHooks (..),
    LLMObjectResult,
    LLMTextResult,
    StreamEvent,
  )
import LLM.Core.Utils (streamResponseJson)
import Network.HTTP.Req (HttpException)

data LLMProvider = LLMProvider
  { -- | Provider name for logging/hooks (e.g. "claude", "gemini", "openai")
    providerName :: Text,
    -- | Build the JSON request body. Bool indicates whether streaming is requested.
    buildBody :: Bool -> ChatRequest -> Value,
    -- | Make a non-streaming HTTP call, returning (status code, response JSON).
    sendRequest :: Value -> IO (Int, Value),
    -- | Make a streaming HTTP call. The handler receives the raw response
    -- and should parse it (checking status, reading body, etc.).
    sendStreamRequest :: Value -> (StreamEvent -> IO ()) -> IO LLMTextResult,
    -- | Parse a complete (non-streaming) JSON response body.
    parseResponse :: Value -> IO LLMTextResult,
    -- | Build the JSON request body for object generation.
    buildObjectBody :: ChatRequest -> Value -> Value,
    -- | Make a non-streaming HTTP call for object generation, returning (status code, response JSON).
    sendObjectRequest :: Value -> IO (Int, Value),
    -- | Parse a complete JSON response body for object generation.
    parseObjectResponse :: Value -> IO LLMObjectResult
  }

-- | Generic non-streaming chat via the LLMProvider.
genericGenerateText :: LLMProvider -> LLMHooks -> ChatRequest -> IO LLMTextResult
genericGenerateText p hooks r = do
  let body = buildBody p False r
  onLLMRequest hooks (providerName p) body
  result <- try (sendRequest p body)
  case result of
    Left e -> pure $ Left $ NetworkError (T.pack (show (e :: HttpException)))
    Right (status, respBody) -> do
      onLLMResponse hooks (providerName p) respBody
      if status == 200
        then parseResponse p respBody
        else pure $ Left $ HttpError status (T.pack $ show respBody)

-- | Generic object generation via the LLMProvider.
genericGenerateObject :: LLMProvider -> LLMHooks -> Value -> ChatRequest -> IO LLMObjectResult
genericGenerateObject p hooks schema r = do
  let body = buildObjectBody p r {reqTools = []} schema
  onLLMRequest hooks (providerName p) body
  result <- try (sendObjectRequest p body)
  case result of
    Left e -> do
      onLLMResponseError hooks (providerName p) (T.pack (show (e :: HttpException)))
      pure $ Left $ NetworkError (T.pack (show (e :: HttpException)))
    Right (status, respBody) -> do
      onLLMResponse hooks (providerName p) respBody
      if status == 200
        then parseObjectResponse p respBody
        else pure $ Left $ HttpError status (T.pack $ show respBody)

-- | Generic streaming chat via the LLMProvider.
genericStreamText :: LLMProvider -> LLMHooks -> ChatRequest -> (StreamEvent -> IO ()) -> IO LLMTextResult
genericStreamText p hooks r callback = do
  let body = buildBody p True r
  onLLMRequest hooks (providerName p) body
  result <- try (sendStreamRequest p body callback)
  case result of
    Left e -> pure $ Left $ NetworkError (T.pack (show (e :: HttpException)))
    Right r' -> do
      case r' of
        Right resp -> onLLMResponse hooks (providerName p) (streamResponseJson resp)
        _ -> pure ()
      pure r'

-- | Convert any LLMProvider instance into a LLMGateway.
-- LLMHooks are not baked in — they are passed at call time via 'ChatEnv'.
toGateway :: LLMProvider -> LLMGateway
toGateway p =
  LLMGateway
    { gwName = providerName p,
      gwGenerateText = genericGenerateText p,
      gwStreamText = genericStreamText p,
      gwGenerateObject = genericGenerateObject p
    }
