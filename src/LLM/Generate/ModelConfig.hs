module LLM.Generate.ModelConfig
  ( ModelConfig (..),
    ModelWithFallbacks (..),
    mfwToModelConfigs,
    modelRetryPolicy,
  )
where

import Control.Retry (RetryPolicyM, fullJitterBackoff, limitRetries)
import Data.Text (Text)
import LLM.Core.Types (LLMGateway, ThinkingMode)
import LLM.Core.Usage (PricingInfo)

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
    -- | Whole-request timeout in milliseconds ('Nothing' = no timeout).
    mcRequestTimeout :: Maybe Int,
    -- | Delay in milliseconds before each API call (rate limiting).
    mcThrottleDelay :: Maybe Int,
    -- | Number of retries on retryable 'LLMError's before giving up on this model.
    mcRetryCount :: Int,
    -- | Base backoff in milliseconds between retries (jittered).
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

modelRetryPolicy :: ModelConfig -> RetryPolicyM IO
modelRetryPolicy mc = limitRetries mc.mcRetryCount <> fullJitterBackoff (mc.mcJitterBackoff * 1000)
