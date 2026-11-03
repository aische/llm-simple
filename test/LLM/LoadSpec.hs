module LLM.LoadSpec (spec) where

import Control.Exception (IOException, SomeException, catch, displayException, fromException, try)
import Control.Monad (forM_)
import Control.Monad.Except (runExceptT)
import Data.Aeson (Value (Array, Object), eitherDecodeFileStrict', encode, object, toJSON, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString qualified as BS
import Data.List (find)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Types (LLMGateway (..))
import LLM.Generate.ModelConfig (ModelCapabilities (..), ModelConfig (..), defaultModelCapabilities)
import LLM.Load.LoadGateways (ProviderUnavailable (..), buildGateway, loadGateways, loadGatewaysFromCatalog, loadGatewaysWithDotenv, loadGatewaysFromCatalogOrThrow, parseProviderBaseUrl, resolveGateway)
import LLM.Load.LoadModels (loadModelOrThrow, loadModelOrThrow_, loadModelsOrThrow)
import LLM.Load.ModelCatalog (ModelCatalogItem (..))
import LLM.Load.ProviderCatalog
  ( ProviderCatalogItem (..),
    ProviderProtocol (..),
    defaultProviderCatalogMap,
    loadProviderCatalog,
    loadProviderCatalogForModelCatalog,
  )
import LLM.Load.Types (LoadConfigError (..))
import LLM.Load.Utils (getSystemPrompt)
import System.Directory (createDirectory, withCurrentDirectory)
import System.Environment (setEnv)
import System.Environment.Blank qualified as Blank
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
  ( Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
    shouldContain,
    shouldReturn,
    shouldSatisfy,
    shouldThrow,
    anyErrorCall,
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
      cfg.mcCapabilities.capVision `shouldBe` False

    it "loads catalog capabilities when present" $ do
      result <- eitherDecodeFileStrict' "./model-catalog.json"
      case result of
        Right (items :: [ModelCatalogItem]) -> do
          fmap (.capabilities) (find (\i -> i.modelConfigName == "haiku_4_5") items)
            `shouldBe` Just
              ( Just
                  ModelCapabilities
                    { capThinking = True,
                      capVision = True,
                      capPromptCaching = True
                    }
              )
        Left err -> expectationFailure err

    it "loads catalogs without capabilities (defaults false)" $ do
      cfg <- loadModelOrThrow ollamaCatalog "mistral"
      cfg.mcCapabilities `shouldBe` defaultModelCapabilities

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

  describe "providers that are unavailable" $ do
    it "loads a model when another model's provider has no API key" $
      withCatalogs mixedCatalog mixedProviders $ \path -> do
        cfg <- loadModelOrThrow path "llama_3_2"
        cfg.mcModel `shouldBe` "llama3.2:latest"

    it "names the missing env var when the requested model's provider has no API key" $
      withCatalogs mixedCatalog mixedProviders $ \path -> do
        result <- try @LoadConfigError (loadModelOrThrow path "haiku")
        case result of
          Left (LoadModelConfigError msg) -> do
            T.unpack msg `shouldContain` "haiku"
            T.unpack msg `shouldContain` "claude"
            T.unpack msg `shouldContain` "LLM_SIMPLE_TEST_UNSET_KEY"
          Left other -> expectationFailure $ "expected LoadModelConfigError, got: " <> show other
          Right _ -> expectationFailure "expected failure"

    it "ignores an invalid baseUrl of a provider no requested model uses" $
      withCatalogs mixedCatalog mixedProviders $ \path -> do
        (_, catalog, _) <- loadModelOrThrow_ path "llama_3_2"
        Map.keys catalog `shouldBe` ["llama_3_2"]

    it "reports the invalid baseUrl when the requested model's provider has one" $
      withCatalogs mixedCatalog mixedProviders $ \path -> do
        result <- try @LoadConfigError (loadModelOrThrow path "broken_model")
        case result of
          Left (LoadModelConfigError msg) -> do
            T.unpack msg `shouldContain` "broken"
            T.unpack msg `shouldContain` "baseUrl"
          Left other -> expectationFailure $ "expected LoadModelConfigError, got: " <> show other
          Right _ -> expectationFailure "expected failure"

  describe "loadGatewaysWithDotenv" $ do
    it "ignores a missing .env" $
      inTempDir $ \_ -> do
        gateways <- loadGatewaysWithDotenv
        Map.member "ollama" gateways `shouldBe` True

    it "ignores a .env it cannot read" $
      inTempDir $ \dir -> do
        createDirectory (dir </> ".env")
        gateways <- loadGatewaysWithDotenv
        Map.member "ollama" gateways `shouldBe` True

    it "only swallows IO errors, not other exceptions" $
      inTempDir $ \dir -> do
        writeFile (dir </> ".env") "not a dotenv line\n"
        loadGatewaysWithDotenv `shouldThrow` anyErrorCall

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

    it "builds a claude gateway from a custom baseUrl origin" $ do
      let item =
            ProviderCatalogItem
              { providerName = "claude",
                protocol = ClaudeProtocol,
                baseUrl = "https://claude-proxy.example.com",
                apiKeyEnv = Just "CLAUDE_API_KEY",
                baseUrlEnv = Nothing
              }
      Right baseUrl <- pure $ parseProviderBaseUrl item.baseUrl
      let gateway = buildGateway item baseUrl "test-key"
          LLMGateway {gwName = name} = gateway
      name `shouldBe` "claude"

    it "builds a gemini gateway from a custom baseUrl origin" $ do
      let item =
            ProviderCatalogItem
              { providerName = "gemini",
                protocol = GeminiProtocol,
                baseUrl = "https://gemini-proxy.example.com",
                apiKeyEnv = Just "GEMINI_API_KEY",
                baseUrlEnv = Nothing
              }
      Right baseUrl <- pure $ parseProviderBaseUrl item.baseUrl
      let gateway = buildGateway item baseUrl "test-key"
          LLMGateway {gwName = name} = gateway
      name `shouldBe` "gemini"

    describe "plain-HTTP base URLs" $ do
      let item proto url =
            ProviderCatalogItem
              { providerName = "custom",
                protocol = proto,
                baseUrl = url,
                apiKeyEnv = Just "LLM_SIMPLE_TEST_HTTP_KEY",
                baseUrlEnv = Nothing
              }
          resolve i = do
            setEnv "LLM_SIMPLE_TEST_HTTP_KEY" "secret"
            either Left (const (Right ())) <$> resolveGateway i
          isInsecure = \case
            Left (InvalidBaseUrl msg) -> "insecure baseUrl" `T.isInfixOf` msg
            _ -> False

      it "rejects a remote http:// base URL for a provider that sends a key" $ do
        forM_ [OpenAIProtocol, ClaudeProtocol, GeminiProtocol, DeepSeekProtocol] $ \proto -> do
          r <- resolve (item proto "http://proxy.example.com")
          r `shouldSatisfy` isInsecure
        r <- resolve (item OpenAIProtocol "http://127.example.com")
        r `shouldSatisfy` isInsecure

      it "accepts http:// to a loopback host" $ do
        forM_ ["http://localhost:8080", "http://LOCALHOST.", "http://127.0.0.1:8080/api", "http://127.9.8.7", "http://[::1]:8080"] $ \url -> do
          r <- resolve (item OpenAIProtocol url)
          (url, r) `shouldBe` (url, Right ())

      it "accepts https:// to a remote host" $ do
        r <- resolve (item ClaudeProtocol "https://proxy.example.com")
        r `shouldBe` Right ()

      it "accepts a remote http:// base URL when no key is sent" $ do
        r <- resolve (item OllamaProtocol "http://gpu-box.lan:11434")
        r `shouldBe` Right ()
        r' <- resolve (item OpenAIProtocol "http://gpu-box.lan:8000") {apiKeyEnv = Nothing}
        r' `shouldBe` Right ()

      it "makes loadGatewaysFromCatalogOrThrow throw" $ do
        setEnv "LLM_SIMPLE_TEST_HTTP_KEY" "secret"
        r <- try (loadGatewaysFromCatalogOrThrow (Map.singleton "custom" (item OpenAIProtocol "http://proxy.example.com")))
        case r of
          Left (LoadModelConfigError msg) -> T.unpack msg `shouldContain` "insecure baseUrl"
          Left other -> expectationFailure ("unexpected error: " <> show other)
          Right _ -> expectationFailure "expected LoadModelConfigError"

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

  describe "invalid catalog entries" $ do
    let expectCatalogError path name needles = do
          result <- try @LoadConfigError (loadModelOrThrow path name)
          case result of
            Left (LoadModelCatalogError msg) -> mapM_ (\n -> T.unpack msg `shouldContain` n) needles
            Left other -> expectationFailure $ "expected LoadModelCatalogError, got: " <> show other
            Right _ -> expectationFailure "expected failure"

    it "rejects a modelConfigName used twice" $
      withModelCatalog [modelEntry "twice" [], modelEntry "twice" [("maxTokens", 2048)]] $ \path ->
        expectCatalogError path "twice" ["duplicate modelConfigName", "twice"]

    it "rejects maxTokens <= 0, naming the entry" $
      forM_ [0, -5] $ \n ->
        withModelCatalog [modelEntry "ok" [], modelEntry "bad_tokens" [("maxTokens", n)]] $ \path ->
          -- The whole catalog is rejected, not only the bad entry.
          expectCatalogError path "ok" ["bad_tokens", "maxTokens"]

    it "rejects a negative limit, naming the entry and the field" $
      forM_ ["requestTimeout", "throttleDelay", "retryCount", "jitterBackoff"] $ \field ->
        withModelCatalog [modelEntry "bad_limit" [(field, -1)]] $ \path ->
          expectCatalogError path "bad_limit" ["bad_limit", T.unpack field]

    it "rejects a negative price, naming the entry and the field" $
      forM_ ["pricePerMillionInput", "pricePerMillionOutput", "pricePerMillionCacheRead", "pricePerMillionCacheWrite"] $ \field ->
        withModelCatalog [modelEntryWithPricing "bad_price" [(field, -0.5)]] $ \path ->
          expectCatalogError path "bad_price" ["bad_price", T.unpack field]

    it "accepts zero limits and prices" $
      withModelCatalog [modelEntry "zeros" [("requestTimeout", 0), ("throttleDelay", 0), ("retryCount", 0), ("jitterBackoff", 0)]] $ \path -> do
        cfg <- loadModelOrThrow path "zeros"
        cfg.mcMaxTokens `shouldBe` 1024

    it "rejects a providerName used twice in providers.json" $ do
      let provider url = "{\"providerName\":\"dup\",\"protocol\":\"ollama\",\"baseUrl\":\"" <> url <> "\"}"
      withCatalogs "[]" ("[" <> provider "http://localhost:1" <> "," <> provider "http://localhost:2" <> "]") $ \path -> do
        result <- try @LoadConfigError (loadModelOrThrow path "x")
        case result of
          Left (LoadProviderCatalogError msg) -> do
            T.unpack msg `shouldContain` "duplicate providerName"
            T.unpack msg `shouldContain` "dup"
          Left other -> expectationFailure $ "expected LoadProviderCatalogError, got: " <> show other
          Right _ -> expectationFailure "expected failure"

  describe "empty API keys" $ do
    let item =
          ProviderCatalogItem
            { providerName = "custom",
              protocol = OpenAIProtocol,
              baseUrl = "https://example.com",
              apiKeyEnv = Just "LLM_SIMPLE_TEST_BLANK_KEY",
              baseUrlEnv = Nothing
            }
        resolveWith value = do
          Blank.setEnv "LLM_SIMPLE_TEST_BLANK_KEY" value True
          either Left (const (Right ())) <$> resolveGateway item

    it "treats an empty or whitespace-only key as missing" $
      forM_ ["", "   ", "\t\n"] $ \value ->
        resolveWith value `shouldReturn` Left (MissingApiKey "LLM_SIMPLE_TEST_BLANK_KEY")

    it "accepts a non-empty key" $
      resolveWith "secret" `shouldReturn` Right ()

  describe "getSystemPrompt" $ do
    it "reports a file that is not valid UTF-8" $
      withSystemTempDirectory "llm-simple-prompt" $ \root -> do
        let path = root </> "prompt.txt"
        BS.writeFile path (BS.pack [0x63, 0x61, 0x66, 0xE9])
        result <- runExceptT $ getSystemPrompt (Just (T.pack ("file:" <> path)))
        case result of
          Left (LoadSystemPromptError msg) -> do
            T.unpack msg `shouldContain` path
            T.unpack msg `shouldContain` "UTF-8"
          Left other -> expectationFailure $ "expected LoadSystemPromptError, got: " <> show other
          Right p -> expectationFailure $ "expected failure, got: " <> show p

-- | A model catalog entry for Ollama (no key) with integer fields
-- overridden.
modelEntry :: Text -> [(Text, Int)] -> Value
modelEntry name overrides = withFields (baseModelEntry name) [(f, toJSON v) | (f, v) <- overrides]

-- | A model catalog entry with pricing fields overridden.
modelEntryWithPricing :: Text -> [(Text, Double)] -> Value
modelEntryWithPricing name overrides =
  case baseModelEntry name of
    Object o
      | Just pricing <- KM.lookup "pricing" o ->
          Object (KM.insert "pricing" (withFields pricing [(f, toJSON v) | (f, v) <- overrides]) o)
    other -> other

baseModelEntry :: Text -> Value
baseModelEntry name =
  object
    [ "modelConfigName" .= name,
      "providerName" .= ("ollama" :: Text),
      "modelName" .= ("llama3.2:latest" :: Text),
      "pricing" .= object ["pricePerMillionInput" .= (0 :: Double), "pricePerMillionOutput" .= (0 :: Double)],
      "maxTokens" .= (1024 :: Int),
      "retryCount" .= (0 :: Int),
      "jitterBackoff" .= (0 :: Int)
    ]

withFields :: Value -> [(Text, Value)] -> Value
withFields (Object o) fields = Object (foldr (\(f, v) -> KM.insert (K.fromText f) v) o fields)
withFields v _ = v

-- | Write a model catalog (and an empty @providers.json@) to a temporary
-- directory.
withModelCatalog :: [Value] -> (FilePath -> IO a) -> IO a
withModelCatalog entries act =
  withSystemTempDirectory "llm-simple-catalog-entries" $ \root -> do
    let catalogPath = root </> "model-catalog.json"
    BSL.writeFile catalogPath (encode entries)
    writeFile (root </> "providers.json") "[]"
    act catalogPath

withTempCatalog :: String -> (FilePath -> IO a) -> IO a
withTempCatalog contents act =
  withSystemTempDirectory "llm-simple-load" $ \root -> do
    let path = root </> "catalog.json"
    writeFile path contents
    act path `catch` \(_ :: IOException) -> act path

-- | Write a model catalog and a @providers.json@ next to it.
withCatalogs :: String -> String -> (FilePath -> IO a) -> IO a
withCatalogs models providers act =
  withSystemTempDirectory "llm-simple-catalogs" $ \root -> do
    let catalogPath = root </> "model-catalog.json"
    writeFile catalogPath models
    writeFile (root </> "providers.json") providers
    act catalogPath

-- | One model each for Ollama (no key), Claude (key env var unset) and a
-- provider with an invalid base URL.
mixedCatalog :: String
mixedCatalog =
  "[" <> catalogEntry "llama_3_2" "ollama" "llama3.2:latest"
    <> "," <> catalogEntry "haiku" "claude" "claude-haiku-4-5"
    <> "," <> catalogEntry "broken_model" "broken" "some-model"
    <> "]"
  where
    catalogEntry name provider model =
      "{\"modelConfigName\":\"" <> name <> "\",\"providerName\":\"" <> provider
        <> "\",\"modelName\":\"" <> model
        <> "\",\"pricing\":{\"pricePerMillionInput\":0,\"pricePerMillionOutput\":0}"
        <> ",\"maxTokens\":1024,\"retryCount\":0,\"jitterBackoff\":0}"

mixedProviders :: String
mixedProviders =
  "[{\"providerName\":\"claude\",\"protocol\":\"claude\",\"baseUrl\":\"https://api.anthropic.com\",\"apiKeyEnv\":\"LLM_SIMPLE_TEST_UNSET_KEY\"}"
    <> ",{\"providerName\":\"broken\",\"protocol\":\"openai\",\"baseUrl\":\"not a url\"}]"

withProviderCatalogDir :: FilePath -> (FilePath -> IO a) -> IO a
withProviderCatalogDir providersFixture act =
  withSystemTempDirectory "llm-simple-providers" $ \root -> do
    let catalogPath = root </> "model-catalog.json"
        providersPath = root </> "providers.json"
    writeFile catalogPath "[]"
    providersJson <- readFile providersFixture
    writeFile providersPath providersJson
    act catalogPath `catch` \(_ :: IOException) -> act catalogPath

-- | Run in a fresh temporary directory as the working directory, so a
-- @.env@ in the repository is never loaded.
inTempDir :: (FilePath -> IO a) -> IO a
inTempDir act =
  withSystemTempDirectory "llm-simple-dotenv" $ \dir ->
    withCurrentDirectory dir (act dir)
