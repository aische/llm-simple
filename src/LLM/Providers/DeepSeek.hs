module LLM.Providers.DeepSeek
  ( deepSeekGateway,
    deepSeekGatewayWith,
    deepSeekProvider,
    deepSeekProviderWith,
    deepSeekBuildBodyPairs,
  )
where

import Data.Aeson
  ( KeyValue ((.=)),
    Value,
    decodeStrict',
    encode,
    object,
    withObject,
    (.:),
  )
import Data.Aeson.Types (Pair, Parser, parseMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import LLM.Core.LLMProvider (LLMProvider (..), toGateway)
import LLM.Core.ProviderUtils (handleStreamResponse, lenientConfig, stripJsonFences)
import LLM.Core.Types
  ( ChatRequest
      ( reqConversation,
        reqMaxTokens,
        reqModel,
        reqTemperature,
        reqThinking,
        reqTools
      ),
    LLMError (EmptyResponse),
    LLMGateway,
    ThinkingMode (..),
    Turn (UserTurn),
    deepSeekMessageEncodeOptions,
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

-- | Create a LLMGateway for the DeepSeek provider. Takes the API key as a parameter.
deepSeekGateway :: Text -> LLMGateway
deepSeekGateway apiKey = toGateway $ deepSeekProvider apiKey

-- | Create a LLMGateway for the DeepSeek provider with a custom base URL. Takes the API key and base URL as parameters.
deepSeekGatewayWith :: Url scheme -> Option scheme -> Text -> LLMGateway
deepSeekGatewayWith baseUrl baseOpts apiKey = toGateway (deepSeekProviderWith baseUrl baseOpts apiKey)

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
        let schemaText = TL.toStrict . TLE.decodeUtf8 $ encode schema
            instruction =
              "Respond with a raw JSON object matching this JSON schema. "
                <> "No markdown, no code fences, no explanation:\n"
                <> schemaText
            r' = r {reqConversation = r.reqConversation <> [UserTurn instruction]}
         in object $
              deepSeekBuildBodyPairs False r'
                <> ["response_format" .= object ["type" .= ("json_object" :: Text)]],
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

-- | OpenAI-compatible body; DeepSeek uses @max_tokens@ (not @max_completion_tokens@).
deepSeekBuildBody :: Bool -> ChatRequest -> Value
deepSeekBuildBody stream r = object $ deepSeekBuildBodyPairs stream r

deepSeekBuildBodyPairs :: Bool -> ChatRequest -> [Pair]
deepSeekBuildBodyPairs stream r =
  [ "model" .= r.reqModel,
    "max_tokens" .= r.reqMaxTokens,
    "messages" .= buildMessages deepSeekMessageEncodeOptions r
  ]
    ++ thinkingPairs r
    ++ ["temperature" .= t | Just t <- [r.reqTemperature]]
    ++ ["tools" .= map encodeToolDef r.reqTools | not (null r.reqTools)]
    ++ ["stream" .= True | stream]
    ++ ["stream_options" .= object ["include_usage" .= True] | stream]

thinkingPairs :: ChatRequest -> [Pair]
thinkingPairs r =
  case r.reqThinking of
    Just tm
      | not tm.tmEnabled ->
          ["thinking" .= object ["type" .= ("disabled" :: Text)]]
    Just tm ->
      ("thinking" .= object ["type" .= ("enabled" :: Text)]) : ["reasoning_effort" .= e | Just e <- [tm.tmEffort]]
    Nothing ->
      ["thinking" .= object ["type" .= ("disabled" :: Text)]]
