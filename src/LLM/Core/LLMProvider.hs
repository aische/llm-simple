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

-- | Generic streaming chat via the LLMProvider.
genericStreamText :: LLMProvider -> LLMHooks -> ChatRequest -> (StreamEvent -> IO ()) -> IO LLMTextResult
genericStreamText p hooks r callback = do
  let body = p.buildBody True r
  hooks.onLLMRequest p.providerName body
  result <- try (p.sendStreamRequest body callback)
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
    Right (status, respBody) -> do
      hooks.onLLMResponse p.providerName respBody
      if status == 200
        then p.parseResponse respBody
        else do
          hooks.onLLMResponseError p.providerName ("HTTP error: " <> T.pack (show status) <> " " <> T.pack (show respBody))
          pure $ Left $ HttpError status (T.pack $ show respBody)

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
    Right (status, respBody) -> do
      hooks.onLLMResponse p.providerName respBody
      if status == 200
        then p.parseObjectResponse respBody
        else do
          hooks.onLLMResponseError p.providerName ("HTTP error: " <> T.pack (show status) <> " " <> T.pack (show respBody))
          pure $ Left $ HttpError status (T.pack $ show respBody)

-- | Convert any LLMProvider instance into a LLMGateway.
-- LLMHooks are not baked in — they are passed at call time.
toGateway :: LLMProvider -> LLMGateway
toGateway p =
  LLMGateway
    { gwName = p.providerName,
      gwGenerateText = genericGenerateText p,
      gwStreamText = genericStreamText p,
      gwGenerateObject = genericGenerateObject p
    }
