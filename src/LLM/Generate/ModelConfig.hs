module LLM.Generate.ModelConfig
  ( ModelCapabilities (..),
    defaultModelCapabilities,
    ModelConfig (..),
    ModelWithFallbacks (..),
    mfwToModelConfigs,
    modelRetryPolicy,
  )
where

import Control.Retry (RetryPolicyM, capDelay, fullJitterBackoff, limitRetries)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.!=), (.:?), (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import LLM.Core.Types (LLMGateway, ThinkingMode)
import LLM.Core.Usage (PricingInfo)

-- | Declared model capabilities from the catalog.
--
-- Missing flags decode as 'False'. These are declarations, not automatic
-- API discovery; request validation consults them before I/O.
data ModelCapabilities = ModelCapabilities
  { capThinking :: Bool,
    capVision :: Bool,
    capPromptCaching :: Bool
  }
  deriving (Show, Eq, Ord, Generic)

defaultModelCapabilities :: ModelCapabilities
defaultModelCapabilities =
  ModelCapabilities
    { capThinking = False,
      capVision = False,
      capPromptCaching = False
    }

instance ToJSON ModelCapabilities where
  toJSON caps =
    object
      [ "thinking" .= caps.capThinking,
        "vision" .= caps.capVision,
        "promptCaching" .= caps.capPromptCaching
      ]

instance FromJSON ModelCapabilities where
  parseJSON = withObject "capabilities" $ \o ->
    ModelCapabilities
      <$> o .:? "thinking" .!= False
      <*> o .:? "vision" .!= False
      <*> o .:? "promptCaching" .!= False

-- | Provider connection and per-model tuning parameters.
--
-- Usually constructed via 'LLM.Load.loadModelOrThrow' from a JSON catalog entry,
-- or manually when wiring a custom 'LLMGateway'.
data ModelConfig = ModelConfig
  { -- | Provider implementation (OpenAI, Claude, Ollama, etc.).
    mcGateway :: LLMGateway,
    -- | Provider-specific model identifier (e.g. @gpt-4.1-2025-04-14@).
    mcModel :: Text,
    -- | Per-million-token pricing used to populate 'Usage.usageTotalCost'.
    mcPricing :: PricingInfo,
    -- | Maximum tokens requested from the provider.
    mcMaxTokens :: Int,
    -- | Sampling temperature, when supported by the provider.
    mcTemperature :: Maybe Double,
    -- | Thinking / reasoning mode (DeepSeek, etc.), when supported.
    mcThinking :: Maybe ThinkingMode,
    -- | Declared capabilities (vision, thinking, prompt caching).
    mcCapabilities :: ModelCapabilities,
    -- | Per-attempt timeout in milliseconds ('Nothing' = no timeout). Each
    -- retry gets the full timeout; a timeout is retryable. For streaming it
    -- bounds only the time until the first stream event.
    mcRequestTimeout :: Maybe Int,
    -- | Delay in milliseconds before each API call (rate limiting).
    mcThrottleDelay :: Maybe Int,
    -- | Number of retries on retryable 'LLMError's before giving up on this model.
    mcRetryCount :: Int,
    -- | Base backoff in milliseconds between retries (jittered). The
    -- server's @Retry-After@ header is not honoured, so for a provider that
    -- often rate-limits (429), raise this (each delay is capped at 30 s).
    mcJitterBackoff :: Int
  }

-- | Primary model plus an ordered fallback chain.
--
-- On failure the library tries 'mwfModel' first, then each entry in
-- 'mwfFallbacks' in order. Aborts ('GErrAborted') do not fall through.
data ModelWithFallbacks = ModelWithFallbacks
  { mwfModel :: ModelConfig,
    mwfFallbacks :: [ModelConfig]
  }

mfwToModelConfigs :: ModelWithFallbacks -> [ModelConfig]
mfwToModelConfigs mwf = mwf.mwfModel : mwf.mwfFallbacks

-- | 'mcRetryCount' retries with full-jitter exponential backoff from
-- 'mcJitterBackoff', each delay capped at 30 s. @Retry-After@ is ignored.
modelRetryPolicy :: ModelConfig -> RetryPolicyM IO
modelRetryPolicy mc =
  capDelay 30_000_000 $
    limitRetries mc.mcRetryCount <> fullJitterBackoff (mc.mcJitterBackoff * 1000)
