module LLM.LoadSpec (spec) where

import Control.Exception (IOException, SomeException, catch, displayException, fromException, try)
import Control.Monad.Except (runExceptT)
import Data.Aeson (Value (Array), eitherDecodeFileStrict')
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Types (LLMGateway (..))
import LLM.Generate.ModelConfig (ModelConfig (..))
import LLM.Load.LoadGateways (buildGateway, loadGateways, loadGatewaysFromCatalog, parseProviderBaseUrl)
import LLM.Load.LoadModels (loadModelOrThrow, loadModelOrThrow_, loadModelsOrThrow)
import LLM.Load.ProviderCatalog
  ( ProviderCatalogItem (..),
    ProviderProtocol (..),
    defaultProviderCatalogMap,
    loadProviderCatalog,
    loadProviderCatalogForModelCatalog,
  )
import LLM.Load.Types (LoadConfigError (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
  ( Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
    shouldContain,
    shouldSatisfy,
  )

ollamaCatalog :: FilePath
ollamaCatalog = "./test/fixtures/ollama-catalog.json"

providersOllamaOnly :: FilePath
providersOllamaOnly = "./test/fixtures/providers-ollama-only.json"

providersOpenrouterOnly :: FilePath
providersOpenrouterOnly = "./test/fixtures/providers-openrouter-only.json"

spec :: Spec
spec = describe "Load" $ do
  describe "model catalog JSON" $ do
    it "parses the bundled model catalog" $ do
      result <- eitherDecodeFileStrict' "./model-catalog.json"
      case result of
        Right (Array xs) -> length xs `shouldSatisfy` (> 0)
        _ -> expectationFailure "expected a JSON array"

    it "fails on missing catalog file" $ do
      result <- try @SomeException (loadModelOrThrow "./no-such-catalog.json" "llama_3_2")
      case result of
        Left err
          | isJust (fromException err :: Maybe IOException) -> pure ()
          | isJust (fromException err :: Maybe LoadConfigError) -> pure ()
          | otherwise ->
              expectationFailure $ "expected missing-file failure, got: " <> displayException err
        Right _ -> expectationFailure "expected failure"

  describe "loadModelsOrThrow" $ do
    it "loads a known model config" $ do
      cfg <- loadModelOrThrow ollamaCatalog "llama_3_2"
      cfg.mcModel `shouldBe` "llama3.2:latest"

    it "throws when a model config is missing" $ do
      result <- try @LoadConfigError (loadModelsOrThrow ollamaCatalog ("llama_3_2" :: Text, "missing_model" :: Text))
      case result of
        Left (LoadModelConfigError msg) -> T.unpack msg `shouldContain` "Model not found"
        Left other -> expectationFailure $ "expected LoadModelConfigError, got: " <> show other
        Right _ -> expectationFailure "expected failure"

    it "loads the full catalog map from an ollama-only fixture" $ do
      (_, catalog, _) <- loadModelOrThrow_ ollamaCatalog "llama_3_2"
      Map.member "llama_3_2" catalog `shouldBe` True
      Map.member "mistral" catalog `shouldBe` True

  describe "loadGateways" $ do
    it "always includes ollama" $ do
      gateways <- loadGateways
      Map.member "ollama" gateways `shouldBe` True

    it "builds an openai-compatible gateway from a custom provider catalog" $ do
      let item =
            ProviderCatalogItem
              { providerName = "openrouter",
                protocol = OpenAIProtocol,
                baseUrl = "https://openrouter.ai/api",
                apiKeyEnv = Just "OPENROUTER_API_KEY",
                baseUrlEnv = Nothing
              }
      Right baseUrl <- pure $ parseProviderBaseUrl item.baseUrl
      let gateway = buildGateway item baseUrl "test-key"
          LLMGateway {gwName = name} = gateway
      name `shouldBe` "openrouter"

    it "loads gateways from a provider catalog file" $ do
      result <- runExceptT $ loadProviderCatalog providersOllamaOnly
      case result of
        Left err -> expectationFailure $ show err
        Right catalog -> do
          gateways <- loadGatewaysFromCatalog catalog
          Map.keys gateways `shouldSatisfy` ("ollama" `elem`)

  describe "provider catalog JSON" $ do
    it "parses the bundled provider catalog" $ do
      result <- eitherDecodeFileStrict' "./providers.json"
      case result of
        Right (Array xs) -> length xs `shouldSatisfy` (> 0)
        _ -> expectationFailure "expected a JSON array"

    it "includes built-in providers in the default catalog" $ do
      Map.member "openai" defaultProviderCatalogMap `shouldBe` True
      Map.member "ollama" defaultProviderCatalogMap `shouldBe` True

    it "merges a partial providers.json with built-in defaults" $
      withProviderCatalogDir providersOpenrouterOnly $ \catalogPath -> do
        result <- runExceptT $ loadProviderCatalogForModelCatalog catalogPath
        case result of
          Left err -> expectationFailure $ show err
          Right catalog -> do
            Map.member "openrouter" catalog `shouldBe` True
            Map.member "openai" catalog `shouldBe` True
            Map.member "ollama" catalog `shouldBe` True

    it "lets providers.json override a built-in provider" $
      withProviderCatalogDir providersOllamaOnly $ \catalogPath -> do
        result <- runExceptT $ loadProviderCatalogForModelCatalog catalogPath
        case result of
          Left err -> expectationFailure $ show err
          Right catalog -> do
            Map.lookup "ollama" catalog
              `shouldBe` Just
                ProviderCatalogItem
                  { providerName = "ollama",
                    protocol = OllamaProtocol,
                    baseUrl = "http://localhost:11434",
                    apiKeyEnv = Nothing,
                    baseUrlEnv = Nothing
                  }
            Map.member "openai" catalog `shouldBe` True

  describe "invalid catalog JSON" $ do
    it "reports a catalog parse/load error" $ withTempCatalog "[not valid json" $ \path -> do
      result <- try @LoadConfigError (loadModelOrThrow path "x")
      case result of
        Left (LoadModelCatalogError _) -> pure ()
        Left other -> expectationFailure $ "expected LoadModelCatalogError, got: " <> show other
        Right _ -> expectationFailure "expected failure"

withTempCatalog :: String -> (FilePath -> IO a) -> IO a
withTempCatalog contents act =
  withSystemTempDirectory "llm-simple-load" $ \root -> do
    let path = root </> "catalog.json"
    writeFile path contents
    act path `catch` \(_ :: IOException) -> act path

withProviderCatalogDir :: FilePath -> (FilePath -> IO a) -> IO a
withProviderCatalogDir providersFixture act =
  withSystemTempDirectory "llm-simple-providers" $ \root -> do
    let catalogPath = root </> "model-catalog.json"
        providersPath = root </> "providers.json"
    writeFile catalogPath "[]"
    providersJson <- readFile providersFixture
    writeFile providersPath providersJson
    act catalogPath `catch` \(_ :: IOException) -> act catalogPath
