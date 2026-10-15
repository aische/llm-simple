module LLM.Load.LoadModels
  ( ModelConfigMap,
    loadModelConfigMap,
    loadModels,
    loadModel,
    loadModelsOrThrow,
    loadModelOrThrow,
    loadModelsOrThrow_,
    loadModelOrThrow_,
  )
where

import Control.Exception (throwIO)
import Control.Lens (Each (each), mapMOf)
import Control.Monad.Except (ExceptT, liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import LLM.Core.Types (LLMGateway, ThinkingMode (..))
import LLM.Generate.ModelConfig
  ( ModelConfig (..),
    defaultModelCapabilities,
  )
import LLM.Load.LoadGateways (GatewayMap, ProviderUnavailable, describeProviderUnavailable, resolveGateways)
import LLM.Load.ModelCatalog (ModelCatalogItem (..), ModelCatalogMap, loadModelCatalog)
import LLM.Load.ProviderCatalog (loadProviderCatalogForModelCatalog)
import LLM.Load.Types (LoadConfigError (LoadModelConfigError))

-- | Model configs by catalog name.
type ModelConfigMap = Map Text ModelConfig

-- | Providers by name, each either its gateway or why it is unavailable.
type ResolvedProviders = Map Text (Either ProviderUnavailable LLMGateway)

-- | Parse the model and provider catalogs and resolve every provider. Fails
-- only if a catalog cannot be read or parsed; unavailable providers are kept
-- with the reason.
loadCatalogs :: FilePath -> ExceptT LoadConfigError IO (ModelCatalogMap, ResolvedProviders)
loadCatalogs filePath = do
  providerCatalog <- loadProviderCatalogForModelCatalog filePath
  modelCatalog <- loadModelCatalog filePath
  providers <- liftIO $ resolveGateways providerCatalog
  pure (modelCatalog, providers)

-- | Load the catalogs next to @filePath@ and return all models and gateways
-- that are currently available. Models whose provider is unavailable (API key
-- env var not set, invalid base URL, unknown provider) are left out; this
-- never fails because of them.
loadModelConfigMap :: FilePath -> ExceptT LoadConfigError IO (ModelConfigMap, GatewayMap)
loadModelConfigMap filePath = available <$> loadCatalogs filePath

available :: (ModelCatalogMap, ResolvedProviders) -> (ModelConfigMap, GatewayMap)
available (models, providers) =
  ( Map.mapMaybe (either (const Nothing) Just . resolveModel providers) models,
    Map.mapMaybe (either (const Nothing) Just) providers
  )

-- | Look up a requested model. Fails with the reason when the model is not in
-- the catalog or its provider is unavailable.
lookupModel :: ModelCatalogMap -> ResolvedProviders -> Text -> Either LoadConfigError ModelConfig
lookupModel models providers name =
  case Map.lookup name models of
    Nothing -> Left $ LoadModelConfigError $ "Model not found: " <> name
    Just item -> resolveModel providers item

resolveModel :: ResolvedProviders -> ModelCatalogItem -> Either LoadConfigError ModelConfig
resolveModel providers item =
  case Map.lookup item.providerName providers of
    Nothing -> unavailable "is not in the provider catalog"
    Just (Left reason) -> unavailable (describeProviderUnavailable reason)
    Just (Right gateway) ->
      Right
        ModelConfig
          { mcGateway = gateway,
            mcModel = item.modelName,
            mcPricing = item.pricing,
            mcMaxTokens = item.maxTokens,
            mcTemperature = item.temperature,
            mcThinking = fmap (\x -> ThinkingMode {tmEnabled = True, tmEffort = Just x}) item.thinking,
            mcCapabilities = fromMaybe defaultModelCapabilities item.capabilities,
            mcRequestTimeout = item.requestTimeout,
            mcThrottleDelay = item.throttleDelay,
            mcRetryCount = item.retryCount,
            mcJitterBackoff = item.jitterBackoff
          }
  where
    unavailable reason =
      Left $
        LoadModelConfigError $
          "Model " <> item.modelConfigName <> " needs provider " <> item.providerName <> ", but " <> reason

-- | Load the requested models. Also returns all available models and
-- gateways (see 'loadModelConfigMap').
loadModels :: (Each s t Text ModelConfig) => FilePath -> s -> ExceptT LoadConfigError IO (t, ModelConfigMap, GatewayMap)
loadModels filePath s = do
  catalogs@(models, providers) <- loadCatalogs filePath
  r <- liftEither $ mapMOf each (lookupModel models providers) s
  let (modelConfigMap, gatewayMap) = available catalogs
  pure (r, modelConfigMap, gatewayMap)

-- | Load one model. Also returns all available models and gateways (see
-- 'loadModelConfigMap').
loadModel :: FilePath -> Text -> ExceptT LoadConfigError IO (ModelConfig, ModelConfigMap, GatewayMap)
loadModel filePath modelConfigName = do
  catalogs@(models, providers) <- loadCatalogs filePath
  modelConfig <- liftEither $ lookupModel models providers modelConfigName
  let (modelConfigMap, gatewayMap) = available catalogs
  pure (modelConfig, modelConfigMap, gatewayMap)

-- | Load models from a json file and throw an error if any model is not found
--
-- > (gpt, llama, haiku) <- loadModelsOrThrow "./model-catalog.json" ("gpt_4_1", "llama_3_2", "haiku_4_5")
loadModelsOrThrow :: (Each s t Text ModelConfig) => FilePath -> s -> IO t
loadModelsOrThrow filepath s =
  (\(x, _, _) -> x) <$> loadModelsOrThrow_ filepath s

loadModelsOrThrow_ :: (Each s t Text ModelConfig) => FilePath -> s -> IO (t, ModelConfigMap, GatewayMap)
loadModelsOrThrow_ filePath s = do
  r <- runExceptT $ loadModels filePath s
  case r of
    Left err -> throwIO err
    Right result -> pure result

-- | Load a single model from a json file and throw an error if the model is not found
loadModelOrThrow :: FilePath -> Text -> IO ModelConfig
loadModelOrThrow filePath modelConfigName = do
  (\(x, _, _) -> x) <$> loadModelOrThrow_ filePath modelConfigName

loadModelOrThrow_ :: FilePath -> Text -> IO (ModelConfig, ModelConfigMap, GatewayMap)
loadModelOrThrow_ filePath modelConfigName =
  runExceptT (loadModel filePath modelConfigName) >>= either throwIO pure
