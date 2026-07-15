module LLM.Load.ModelCatalog where

import Control.Monad.Except (ExceptT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Usage (PricingInfo)
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
    throttleDelay :: Maybe Int,
    retryCount :: Int,
    jitterBackoff :: Int
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

type ModelCatalogMap = Map Text ModelCatalogItem

loadModelCatalog :: FilePath -> ExceptT LoadConfigError IO ModelCatalogMap
loadModelCatalog filePath =
  Map.fromList . fmap (\item -> (item.modelConfigName, item))
    <$> decodeJsonFile filePath (LoadModelCatalogError . T.pack)
