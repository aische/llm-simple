module LLM.LoadSpec (spec) where

import Control.Exception (IOException, SomeException, catch, displayException, fromException, try)
import Data.Aeson (Value (Array), eitherDecodeFileStrict')
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Generate.ModelConfig (ModelConfig (..))
import LLM.Load.LoadGateways (loadGateways)
import LLM.Load.LoadModels (loadModelOrThrow, loadModelOrThrow_, loadModelsOrThrow)
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
