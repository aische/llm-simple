module LLM.Load.LoadModels where

import Control.Lens (Each (each), mapMOf)
import Control.Monad (forM)
import Control.Monad.Except (ExceptT, MonadError (throwError), liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import LLM.Core.Types (ThinkingMode (..))
import LLM.Generate.ModelConfig (ModelConfig (..))
import LLM.Load.LoadGateways (GatewayMap, loadGateways)
import LLM.Load.ModelCatalog (ModelCatalogItem (..), loadModelCatalog)
import LLM.Load.Types (LoadConfigError (LoadModelConfigError))

type ModelConfigMap = Map Text ModelConfig

loadModelConfigMap :: FilePath -> ExceptT LoadConfigError IO (ModelConfigMap, GatewayMap)
loadModelConfigMap filePath = do
  gatewayMap <- liftIO loadGateways
  modelCatalogMap <- loadModelCatalog filePath
  modelConfigs <- forM (Map.toList modelCatalogMap) $ \(modelConfigName, item) -> do
    case Map.lookup item.providerName gatewayMap of
      Nothing -> throwError $ LoadModelConfigError $ "Provider not found: " <> item.providerName
      Just gateway ->
        pure
          ( modelConfigName,
            ModelConfig
              { mcGateway = gateway,
                mcModel = item.modelName,
                mcPricing = item.pricing,
                mcMaxTokens = item.maxTokens,
                mcTemperature = item.temperature,
                mcThinking = fmap (\x -> ThinkingMode {tmEnabled = True, tmEffort = Just x}) item.thinking,
                mcRequestTimeout = item.requestTimeout,
                mcThrottleDelay = item.throttleDelay,
                mcRetryCount = item.retryCount,
                mcJitterBackoff = item.jitterBackoff
              }
          )
  pure (Map.fromList modelConfigs, gatewayMap)

loadModels :: (Each s t Text ModelConfig) => FilePath -> s -> ExceptT LoadConfigError IO (t, ModelConfigMap, GatewayMap)
loadModels filePath s = do
  (modelConfigMap, gatewayMap) <- loadModelConfigMap filePath
  r <- liftEither $ maybe (Left $ LoadModelConfigError "Model not found") Right $ mapMOf each (`Map.lookup` modelConfigMap) s
  pure (r, modelConfigMap, gatewayMap)

loadModelsOrThrow :: (Each s t Text ModelConfig) => FilePath -> s -> IO t
loadModelsOrThrow filepath s =
  (\(x, _, _) -> x) <$> loadModelsOrThrow_ filepath s

loadModelsOrThrow_ :: (Each s t Text ModelConfig) => FilePath -> s -> IO (t, ModelConfigMap, GatewayMap)
loadModelsOrThrow_ filePath s = do
  r <- runExceptT $ loadModels filePath s
  case r of
    Left err -> error (show err)
    Right result -> pure result

loadModelOrThrow :: FilePath -> Text -> IO ModelConfig
loadModelOrThrow filePath modelConfigName = do
  (\(x, _, _) -> x) <$> loadModelOrThrow_ filePath modelConfigName

loadModelOrThrow_ :: FilePath -> Text -> IO (ModelConfig, ModelConfigMap, GatewayMap)
loadModelOrThrow_ filePath modelConfigName = do
  r <- runExceptT $ loadModelConfigMap filePath
  case r of
    Left err -> error (show err)
    Right (modelConfigMap, gatewayMap) -> do
      case Map.lookup modelConfigName modelConfigMap of
        Nothing -> error ("Model not found: " <> show modelConfigName)
        Just modelConfig -> pure (modelConfig, modelConfigMap, gatewayMap)
