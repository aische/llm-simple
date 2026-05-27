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

data ModelConfig = ModelConfig
  { mcGateway :: LLMGateway,
    mcModel :: Text,
    mcPricing :: PricingInfo,
    mcMaxTokens :: Int,
    mcTemperature :: Maybe Double,
    mcThinking :: Maybe ThinkingMode,
    mcRequestTimeout :: Maybe Int, -- milliseconds; timeout the whole request if it takes too long
    mcThrottleDelay :: Maybe Int, -- milliseconds; wait before each API call
    mcRetryCount :: Int,
    mcJitterBackoff :: Int -- milliseconds; wait before each retry
  }

data ModelWithFallbacks = ModelWithFallbacks
  { mwfModel :: ModelConfig,
    mwfFallbacks :: [ModelConfig]
  }

mfwToModelConfigs :: ModelWithFallbacks -> [ModelConfig]
mfwToModelConfigs mwf = mwf.mwfModel : mwf.mwfFallbacks

modelRetryPolicy :: ModelConfig -> RetryPolicyM IO
modelRetryPolicy mc = limitRetries mc.mcRetryCount <> fullJitterBackoff (mc.mcJitterBackoff * 1000)