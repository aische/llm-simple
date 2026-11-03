module LLM.Load.ModelCatalog where

import Control.Monad.Except (ExceptT, liftEither)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Usage (PricingInfo (..))
import LLM.Generate.ModelConfig (ModelCapabilities)
import LLM.Load.Types (LoadConfigError (..))
import LLM.Load.Utils (decodeJsonFile)

data ModelCatalogItem = ModelCatalogItem
  { modelConfigName :: Text,
    providerName :: Text, -- key from providers.json (built-in or custom)
    modelName :: Text,
    pricing :: PricingInfo,
    maxTokens :: Int,
    temperature :: Maybe Double,
    requestTimeout :: Maybe Int,
    thinking :: Maybe Text,
    -- | Optional capability flags; missing object or keys default to false.
    capabilities :: Maybe ModelCapabilities,
    throttleDelay :: Maybe Int,
    retryCount :: Int,
    jitterBackoff :: Int
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

type ModelCatalogMap = Map Text ModelCatalogItem

-- | Read a model catalog file. Fails with 'LoadModelCatalogError' if the
-- file cannot be read or parsed, or if an entry is invalid: a
-- @modelConfigName@ used twice, @maxTokens <= 0@, or a negative
-- @requestTimeout@, @throttleDelay@, @retryCount@, @jitterBackoff@ or price.
-- The message names the entry.
loadModelCatalog :: FilePath -> ExceptT LoadConfigError IO ModelCatalogMap
loadModelCatalog filePath = do
  items <- decodeJsonFile filePath (LoadModelCatalogError . T.pack)
  liftEither (checkItems items)
  pure (Map.fromList [(item.modelConfigName, item) | item <- items])
  where
    checkItems :: [ModelCatalogItem] -> Either LoadConfigError ()
    checkItems items = case duplicates (map (.modelConfigName) items) of
      name : _ -> Left (LoadModelCatalogError ("duplicate modelConfigName: " <> name))
      [] -> mapM_ checkItem items
    duplicates names = [n | (i, n) <- zip [0 :: Int ..] names, n `elem` take i names]
    checkItem item = case problems item of
      [] -> Right ()
      problem : _ -> Left (LoadModelCatalogError ("model " <> item.modelConfigName <> ": " <> problem))
    problems item =
      [ "maxTokens must be positive, got " <> showT item.maxTokens
      | item.maxTokens <= 0
      ]
        ++ negativeInt "requestTimeout" item.requestTimeout
        ++ negativeInt "throttleDelay" item.throttleDelay
        ++ negativeInt "retryCount" (Just item.retryCount)
        ++ negativeInt "jitterBackoff" (Just item.jitterBackoff)
        ++ negativePrice "pricePerMillionInput" (Just item.pricing.pricePerMillionInput)
        ++ negativePrice "pricePerMillionOutput" (Just item.pricing.pricePerMillionOutput)
        ++ negativePrice "pricePerMillionCacheRead" item.pricing.pricePerMillionCacheRead
        ++ negativePrice "pricePerMillionCacheWrite" item.pricing.pricePerMillionCacheWrite
    negativeInt field v = [field <> " must not be negative, got " <> showT n | Just n <- [v], n < 0]
    negativePrice field v = [field <> " must not be negative, got " <> showT p | Just p <- [v], p < 0]
    showT :: (Show a) => a -> Text
    showT = T.pack . show
