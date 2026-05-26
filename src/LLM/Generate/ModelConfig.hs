module LLM.Generate.ModelConfig
  ( ModelConfig (..),
    ModelWithFallbacks (..),
    mfwToModelConfigs,
    modelRetryPolicy,
  )
where

import Control.Retry (RetryPolicyM, fullJitterBackoff, limitRetries)
import Data.Text (Text)
import LLM.Core.Types (LLMGateway)
import LLM.Core.Usage (PricingInfo)

data ModelConfig = ModelConfig
  { mcGateway :: LLMGateway,
    mcModel :: Text,
    mcPricing :: PricingInfo,
    mcMaxTokens :: Int,
    mcTemperature :: Maybe Double,
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
mfwToModelConfigs mwf = mwfModel mwf : mwfFallbacks mwf

modelRetryPolicy :: ModelConfig -> RetryPolicyM IO
modelRetryPolicy mc = limitRetries (mcRetryCount mc) <> fullJitterBackoff (mcJitterBackoff mc * 1000)