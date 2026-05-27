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
    { usageInputTokens = a.usageInputTokens + b.usageInputTokens,
      usageOutputTokens = a.usageOutputTokens + b.usageOutputTokens,
      usageTotalCost = a.usageTotalCost + b.usageTotalCost
    }

-- | Pricing in dollars per million tokens
data PricingInfo = PricingInfo
  { pricePerMillionInput :: Double,
    pricePerMillionOutput :: Double
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

estimateCost :: PricingInfo -> Usage -> Double
estimateCost p u =
  fromIntegral u.usageInputTokens * p.pricePerMillionInput / 1_000_000
    + fromIntegral u.usageOutputTokens * p.pricePerMillionOutput / 1_000_000
