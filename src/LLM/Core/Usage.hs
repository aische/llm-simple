module LLM.Core.Usage
  ( Usage (..),
    PricingInfo (..),
    emptyUsage,
    addUsage,
    estimateCost,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- | Token usage from a single API call
data Usage = Usage
  { usageInputTokens :: !Int,
    usageOutputTokens :: !Int,
    usageTotalCost :: !Double
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Semigroup Usage where
  (<>) :: Usage -> Usage -> Usage
  (<>) = addUsage

instance Monoid Usage where
  mempty :: Usage
  mempty = emptyUsage

emptyUsage :: Usage
emptyUsage = Usage {usageInputTokens = 0, usageOutputTokens = 0, usageTotalCost = 0}

addUsage :: Usage -> Usage -> Usage
addUsage a b =
  Usage
    { usageInputTokens = usageInputTokens a + usageInputTokens b,
      usageOutputTokens = usageOutputTokens a + usageOutputTokens b,
      usageTotalCost = usageTotalCost a + usageTotalCost b
    }

-- | Pricing in dollars per million tokens
data PricingInfo = PricingInfo
  { pricePerMillionInput :: Double,
    pricePerMillionOutput :: Double
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

estimateCost :: PricingInfo -> Usage -> Double
estimateCost p u =
  fromIntegral (usageInputTokens u) * pricePerMillionInput p / 1_000_000
    + fromIntegral (usageOutputTokens u) * pricePerMillionOutput p / 1_000_000
