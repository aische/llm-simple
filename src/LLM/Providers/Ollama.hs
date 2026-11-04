module LLM.Providers.Ollama
  ( ollamaProvider,
    ollamaProviderWith,
    ollamaGateway,
    ollamaGatewayWith,
  )
where

import Data.Aeson
  ( KeyValue ((.=)),
    object,
  )
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import LLM.Core.Internal.Transport (postJson, postJsonStream)
import LLM.Core.LLMProvider (LLMProvider (..), toGateway)
import LLM.Core.ProviderUtils (normalizeSchemaOpenAI)
import LLM.Core.Types
  ( commonImageMediaTypes,
    ChatRequest
      ( reqMaxTokens,
        reqModel,
        reqTemperature,
        reqTools
      ),
    LLMGateway,
    defaultMessageEncodeOptions,
  )
import LLM.Providers.OpenAI (buildMessages, encodeToolDef, parseOpenAIObjectResponse, parseOpenAIResponse, parseOpenAIStream)
import Network.HTTP.Req
  ( Option,
    Url,
    http,
    port,
    (/:),
  )

-- | Create a LLMGateway for the default Ollama instance (localhost:11434).
ollamaGateway :: LLMGateway
ollamaGateway = toGateway ollamaProvider

-- | Create a LLMGateway for a custom Ollama instance.
ollamaGatewayWith :: Url scheme -> Option scheme -> LLMGateway
ollamaGatewayWith baseUrl baseOpts = toGateway (ollamaProviderWith baseUrl baseOpts)

-- | Default Ollama provider at localhost:11434.
ollamaProvider :: LLMProvider
ollamaProvider = ollamaProviderWith (http "localhost") (port 11434)

ollamaProviderWith :: Url scheme -> Option scheme -> LLMProvider
ollamaProviderWith baseUrl baseOpts =
  LLMProvider
    { providerName = "ollama",
      providerImageMediaTypes = commonImageMediaTypes,
      -- Ollama rejects image URLs ("not currently supported, please use
      -- base64 encoded data instead"); only data URLs work.
      providerImageUrlSupport = False,
      buildBody = \stream r -> object (ollamaBuildBodyPairs stream r),
      sendRequest = sendRequest,
      openStream = postJsonStream url baseOpts,
      parseStream = const parseOpenAIStream,
      parseResponse = pure . parseOpenAIResponse,
      buildObjectBody = \r schema ->
        object $
          ollamaBuildBodyPairs False r
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
      parseObjectResponse = pure . parseOpenAIObjectResponse
    }
  where
    sendRequest = postJson url baseOpts
    url = baseUrl /: "v1" /: "chat" /: "completions"

-- | Request body for Ollama's OpenAI-compatible endpoint. Like OpenAI's, but
-- the token limit is @max_tokens@: Ollama ignores @max_completion_tokens@
-- (and @num_predict@ belongs to its native API).
ollamaBuildBodyPairs :: Bool -> ChatRequest -> [Pair]
ollamaBuildBodyPairs stream r =
  [ "model" .= r.reqModel,
    "max_tokens" .= r.reqMaxTokens,
    "messages" .= buildMessages defaultMessageEncodeOptions r
  ]
    ++ ["temperature" .= t | Just t <- [r.reqTemperature]]
    ++ ["tools" .= map encodeToolDef r.reqTools | not (null r.reqTools)]
    ++ ["stream" .= True | stream]
    ++ ["stream_options" .= object ["include_usage" .= True] | stream]
