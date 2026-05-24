module LLM.Providers.Ollama (ollama, ollamaWith, ollamaGateway, ollamaGatewayWith) where

import Data.Aeson
  ( KeyValue ((.=)),
    Value,
    decodeStrict',
    object,
    withObject,
    (.:),
  )
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import LLM.Core.LLMProvider (LLMProvider (..), toGateway)
import LLM.Core.ProviderUtils (handleStreamResponse, lenientConfig, normalizeSchemaOpenAI, stripJsonFences)
import LLM.Core.Types
  ( ChatRequest
      ( reqMaxTokens,
        reqModel,
        reqTemperature,
        reqTools
      ),
    LLMError (EmptyResponse),
    LLMGateway,
  )
import LLM.Providers.OpenAI (buildMessages, encodeToolDef, openAIBuildBodyPairs, parseOpenAIResponse, parseOpenAIStream, parseOpenAIUsage)
import Network.HTTP.Req
  ( Option,
    POST (POST),
    ReqBodyJson (ReqBodyJson),
    Scheme (Http),
    Url,
    http,
    jsonResponse,
    port,
    req,
    reqBr,
    responseBody,
    responseStatusCode,
    runReq,
    (/:),
  )

-- | Default Ollama provider at localhost:11434.
ollama :: LLMProvider
ollama = ollamaProvider (http "localhost") (port 11434)

-- | Custom Ollama provider with a different host/port.
ollamaWith :: Url 'Http -> Option 'Http -> LLMProvider
ollamaWith = ollamaProvider

ollamaProvider :: Url scheme -> Option scheme -> LLMProvider
ollamaProvider baseUrl baseOpts =
  LLMProvider
    { providerName = "ollama",
      -- Ollama uses the same request format as OpenAI, but without stream_options
      -- (Ollama doesn't support include_usage in streaming).
      buildBody = ollamaBuildBody,
      sendRequest = sendRequest,
      sendStreamRequest = \body callback ->
        runReq lenientConfig $ do
          let url = baseUrl /: "v1" /: "chat" /: "completions"
          reqBr POST url (ReqBodyJson body) baseOpts $ \resp ->
            handleStreamResponse resp (`parseOpenAIStream` callback),
      parseResponse = pure . parseOpenAIResponse,
      buildObjectBody = \r schema ->
        object $
          openAIBuildBodyPairs False r
            <> [ "response_format"
                   .= object
                     [ "type" .= ("json_schema" :: Text),
                       "json_schema"
                         .= object
                           [ "name" .= ("response" :: Text),
                             "schema" .= normalizeSchemaOpenAI schema,
                             "strict" .= True
                           ]
                     ]
               ],
      sendObjectRequest = sendRequest,
      parseObjectResponse = \v -> case parseMaybe parseObject v of
        Nothing -> pure $ Left EmptyResponse
        Just contentStr -> case decodeStrict' (encodeUtf8 (stripJsonFences contentStr)) of
          Nothing -> pure $ Left EmptyResponse
          Just obj -> pure $ Right (obj, parseOpenAIUsage v)
    }
  where
    sendRequest body =
      runReq lenientConfig $ do
        let url = baseUrl /: "v1" /: "chat" /: "completions"
        resp <- req POST url (ReqBodyJson body) jsonResponse baseOpts
        pure (responseStatusCode resp, responseBody resp)
    parseObject :: Value -> Parser Text
    parseObject = withObject "OpenAIObjectResponse" $ \o -> do
      (choice : _) <- o .: "choices" :: Parser [Value]
      withObject "choice" (\co -> co .: "message" >>= withObject "message" (.: "content")) choice

-- | Create a LLMGateway for the default Ollama instance (localhost:11434).
ollamaGateway :: LLMGateway
ollamaGateway = toGateway ollama

-- | Create a LLMGateway for a custom Ollama instance.
ollamaGatewayWith :: Url 'Http -> Option 'Http -> LLMGateway
ollamaGatewayWith baseUrl baseOpts = toGateway (ollamaWith baseUrl baseOpts)

-- | Build request body — same as OpenAI but without stream_options
-- since Ollama doesn't support include_usage.
ollamaBuildBody :: Bool -> ChatRequest -> Value
ollamaBuildBody stream r =
  object $
    [ "model" .= reqModel r,
      "messages" .= buildMessages r
    ]
      ++ ["num_predict" .= reqMaxTokens r]
      ++ ["temperature" .= t | Just t <- [reqTemperature r]]
      ++ ["tools" .= map encodeToolDef (reqTools r) | not (null (reqTools r))]
      ++ ["stream" .= True | stream]
