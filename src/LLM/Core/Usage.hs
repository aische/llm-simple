module LLM.Core.Usage
  ( Usage (..),
    PricingInfo (..),
    emptyUsage,
    mkUsage,
    addUsage,
    estimateCost,
    usageOrdinaryInputTokens,
    defaultPricingInfo,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Types (Object, Parser)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

-- | Token usage from a single API call.
--
-- 'usageInputTokens' is the total input token count, including tokens that were
-- cache reads or cache writes. Provider parsers normalize differing wire
-- semantics so this total is never double-counted.
--
-- Cost estimation treats
-- @usageInputTokens - usageCacheReadTokens - usageCacheCreationTokens@ as
-- ordinary (uncached) input.
data Usage = Usage
  { usageInputTokens :: !Int,
    usageOutputTokens :: !Int,
    usageCacheReadTokens :: !Int,
    usageCacheCreationTokens :: !Int,
    usageTotalCost :: !Double
  }
  deriving (Show, Eq, Generic)

instance ToJSON Usage where
  toJSON :: Usage -> Value
  toJSON u =
    object
      [ "usageInputTokens" .= u.usageInputTokens,
        "usageOutputTokens" .= u.usageOutputTokens,
        "usageCacheReadTokens" .= u.usageCacheReadTokens,
        "usageCacheCreationTokens" .= u.usageCacheCreationTokens,
        "usageTotalCost" .= u.usageTotalCost
      ]

instance FromJSON Usage where
  parseJSON :: Value -> Parser Usage
  parseJSON = withObject "Usage" $ \o ->
    Usage
      <$> o .: "usageInputTokens"
      <*> o .: "usageOutputTokens"
      <*> o .:? "usageCacheReadTokens" .!= 0
      <*> o .:? "usageCacheCreationTokens" .!= 0
      <*> o .:? "usageTotalCost" .!= 0

instance Semigroup Usage where
  (<>) :: Usage -> Usage -> Usage
  (<>) = addUsage

instance Monoid Usage where
  mempty :: Usage
  mempty = emptyUsage

emptyUsage :: Usage
emptyUsage =
  Usage
    { usageInputTokens = 0,
      usageOutputTokens = 0,
      usageCacheReadTokens = 0,
      usageCacheCreationTokens = 0,
      usageTotalCost = 0
    }

-- | Construct usage with zero cache counters and zero cost.
mkUsage :: Int -> Int -> Usage
mkUsage input output =
  Usage
    { usageInputTokens = input,
      usageOutputTokens = output,
      usageCacheReadTokens = 0,
      usageCacheCreationTokens = 0,
      usageTotalCost = 0
    }

addUsage :: Usage -> Usage -> Usage
addUsage a b =
  Usage
    { usageInputTokens = a.usageInputTokens + b.usageInputTokens,
      usageOutputTokens = a.usageOutputTokens + b.usageOutputTokens,
      usageCacheReadTokens = a.usageCacheReadTokens + b.usageCacheReadTokens,
      usageCacheCreationTokens = a.usageCacheCreationTokens + b.usageCacheCreationTokens,
      usageTotalCost = a.usageTotalCost + b.usageTotalCost
    }

-- | Uncached input tokens used for ordinary input pricing.
usageOrdinaryInputTokens :: Usage -> Int
usageOrdinaryInputTokens u =
  max 0 (u.usageInputTokens - u.usageCacheReadTokens - u.usageCacheCreationTokens)

-- | Pricing in dollars per million tokens.
--
-- Optional cache rates fall back to 'pricePerMillionInput' when absent.
data PricingInfo = PricingInfo
  { pricePerMillionInput :: Double,
    pricePerMillionOutput :: Double,
    pricePerMillionCacheRead :: Maybe Double,
    pricePerMillionCacheWrite :: Maybe Double
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PricingInfo where
  toJSON :: PricingInfo -> Value
  toJSON p =
    object $
      [ "pricePerMillionInput" .= p.pricePerMillionInput,
        "pricePerMillionOutput" .= p.pricePerMillionOutput
      ]
        ++ ["pricePerMillionCacheRead" .= r | Just r <- [p.pricePerMillionCacheRead]]
        ++ ["pricePerMillionCacheWrite" .= w | Just w <- [p.pricePerMillionCacheWrite]]

instance FromJSON PricingInfo where
  parseJSON :: Value -> Parser PricingInfo
  parseJSON = withObject "PricingInfo" parsePricingInfo

parsePricingInfo :: Object -> Parser PricingInfo
parsePricingInfo o =
  PricingInfo
    <$> o .: "pricePerMillionInput"
    <*> o .: "pricePerMillionOutput"
    <*> o .:? "pricePerMillionCacheRead"
    <*> o .:? "pricePerMillionCacheWrite"

-- | Pricing with only ordinary input/output rates (no cache-specific rates).
defaultPricingInfo :: Double -> Double -> PricingInfo
defaultPricingInfo input output =
  PricingInfo
    { pricePerMillionInput = input,
      pricePerMillionOutput = output,
      pricePerMillionCacheRead = Nothing,
      pricePerMillionCacheWrite = Nothing
    }

estimateCost :: PricingInfo -> Usage -> Double
estimateCost p u =
  let ordinary = usageOrdinaryInputTokens u
      cacheReadRate = fromMaybe p.pricePerMillionInput p.pricePerMillionCacheRead
      cacheWriteRate = fromMaybe p.pricePerMillionInput p.pricePerMillionCacheWrite
   in fromIntegral ordinary * p.pricePerMillionInput / 1_000_000
        + fromIntegral u.usageCacheReadTokens * cacheReadRate / 1_000_000
        + fromIntegral u.usageCacheCreationTokens * cacheWriteRate / 1_000_000
        + fromIntegral u.usageOutputTokens * p.pricePerMillionOutput / 1_000_000
