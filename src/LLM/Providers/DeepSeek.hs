module LLM.Providers.DeepSeek
  ( deepSeekGateway,
    deepSeekGatewayWith,
    deepSeekProvider,
    deepSeekProviderWith,
  )
where

import Data.Aeson
  ( KeyValue ((.=)),
    Value,
    decodeStrict',
    object,
    withObject,
    (.:),
  )
import Data.Aeson.Types (Pair, Parser, parseMaybe)
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
import LLM.Providers.OpenAI
  ( authHeader,
    buildMessages,
    encodeToolDef,
    parseOpenAIResponse,
    parseOpenAIStream,
    parseOpenAIUsage,
  )
import Network.HTTP.Req
  ( Option,
    POST (POST),
    ReqBodyJson (ReqBodyJson),
    Url,
    https,
    jsonResponse,
    req,
    reqBr,
    responseBody,
    responseStatusCode,
    runReq,
    (/:),
  )

-- | DeepSeek provider at api.deepseek.com.
deepSeekProvider :: Text -> LLMProvider
deepSeekProvider = deepSeekProviderWith (https "api.deepseek.com") mempty

-- | DeepSeek-compatible provider with a custom base URL.
deepSeekProviderWith :: Url scheme -> Option scheme -> Text -> LLMProvider
deepSeekProviderWith baseUrl baseOpts apiKey =
  LLMProvider
    { providerName = "deepseek",
      buildBody = deepSeekBuildBody,
      sendRequest = sendRequest,
      sendStreamRequest = \body callback ->
        runReq lenientConfig $ do
          let url = baseUrl /: "v1" /: "chat" /: "completions"
              opts = baseOpts <> authHeader apiKey
          reqBr POST url (ReqBodyJson body) opts $ \resp ->
            handleStreamResponse resp (`parseOpenAIStream` callback),
      parseResponse = pure . parseOpenAIResponse,
      buildObjectBody = \r schema ->
        object $
          deepSeekBuildBodyPairs False r
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
            opts = baseOpts <> authHeader apiKey
        resp <- req POST url (ReqBodyJson body) jsonResponse opts
        pure (responseStatusCode resp, responseBody resp)
    parseObject :: Value -> Parser Text
    parseObject = withObject "OpenAIObjectResponse" $ \o -> do
      (choice : _) <- o .: "choices" :: Parser [Value]
      withObject "choice" (\co -> co .: "message" >>= withObject "message" (.: "content")) choice

deepSeekGateway :: Text -> LLMGateway
deepSeekGateway apiKey = toGateway $ deepSeekProvider apiKey

deepSeekGatewayWith :: Url scheme -> Option scheme -> Text -> LLMGateway
deepSeekGatewayWith baseUrl baseOpts apiKey = toGateway (deepSeekProviderWith baseUrl baseOpts apiKey)

-- | OpenAI-compatible body; DeepSeek uses @max_tokens@ (not @max_completion_tokens@).
deepSeekBuildBody :: Bool -> ChatRequest -> Value
deepSeekBuildBody stream r = object $ deepSeekBuildBodyPairs stream r

deepSeekBuildBodyPairs :: Bool -> ChatRequest -> [Pair]
deepSeekBuildBodyPairs stream r =
  [ "model" .= reqModel r,
    "max_tokens" .= reqMaxTokens r,
    "messages" .= buildMessages r,
    -- Thinking mode requires reasoning_content on tool-call follow-ups; we do not
    -- preserve it yet. Disable until AssistantTurn carries reasoning_content.
    "thinking" .= object ["type" .= ("disabled" :: Text)]
  ]
    ++ ["temperature" .= t | Just t <- [reqTemperature r]]
    ++ ["tools" .= map encodeToolDef (reqTools r) | not (null (reqTools r))]
    ++ ["stream" .= True | stream]
    ++ ["stream_options" .= object ["include_usage" .= True] | stream]
