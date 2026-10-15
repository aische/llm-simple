module LLM.Load.LoadGateways
  ( GatewayMap,
    ProviderBaseUrl (..),
    buildGateway,
    loadGateways,
    loadGatewaysWithDotenv,
    loadGatewaysFromCatalog,
    loadGatewaysFromCatalogOrThrow,
    parseProviderBaseUrl,
    ProviderUnavailable (..),
    describeProviderUnavailable,
    resolveGateway,
    resolveGateways,
  )
where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch, throwIO)
import Data.Map qualified as Map
import Data.ByteString.Char8 qualified as B
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Types (LLMGateway)
import LLM.Load.ProviderCatalog
  ( ProviderCatalogItem (..),
    ProviderCatalogMap,
    ProviderProtocol (..),
    defaultProviderCatalogMap,
  )
import LLM.Load.Types (LoadConfigError (LoadModelConfigError))
import LLM.Providers.Claude (claudeGatewayWith)
import LLM.Providers.DeepSeek (deepSeekGatewayWith)
import LLM.Providers.Gemini (geminiGatewayWith)
import LLM.Providers.Ollama (ollamaGatewayWith)
import LLM.Providers.OpenAI (openAIGatewayWithName)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Req
  ( Option,
    Scheme (Http, Https),
    Url,
    http,
    https,
    port,
    (/:),
  )
import System.Environment (lookupEnv)

type GatewayMap = Map.Map Text LLMGateway

data ProviderBaseUrl
  = HttpsProviderBase (Url 'Https) (Option 'Https)
  | HttpProviderBase (Url 'Http) (Option 'Http)

parseProviderBaseUrl :: Text -> Either Text ProviderBaseUrl
parseProviderBaseUrl urlText =
  case HC.parseRequest (T.unpack urlText) of
    Left err -> Left $ "invalid baseUrl: " <> urlText <> ": " <> T.pack (show err)
    Right req -> do
      let hostText = T.pack (B.unpack (HC.host req))
          reqPort = HC.port req
          pathSegments =
            map T.pack $
              filter (not . null) $
                splitOnChar '/' $
                  B.unpack (HC.path req)
          mbPort
            | HC.secure req && reqPort == 443 = Nothing
            | not (HC.secure req) && reqPort == 80 = Nothing
            | otherwise = Just reqPort
      if HC.secure req
        then
          let (url, opts) = httpsBase hostText mbPort
           in Right $ HttpsProviderBase (applyPath pathSegments url) opts
        else
          let (url, opts) = httpBase hostText mbPort
           in Right $ HttpProviderBase (applyPath pathSegments url) opts
  where
    splitOnChar _ [] = []
    splitOnChar c xs =
      let (h, t) = break (== c) xs
       in h : case dropWhile (== c) t of
            [] -> []
            ys -> splitOnChar c ys
    httpsBase h Nothing = (https h, mempty)
    httpsBase h (Just p) = (https h, port p)
    httpBase h Nothing = (http h, mempty)
    httpBase h (Just p) = (http h, port p)
    applyPath segments url = foldl (/:) url segments

buildGateway :: ProviderCatalogItem -> ProviderBaseUrl -> Text -> LLMGateway
buildGateway item baseUrl apiKey =
  case item.protocol of
    ClaudeProtocol -> case baseUrl of
      HttpsProviderBase url opts -> claudeGatewayWith url opts apiKey
      HttpProviderBase url opts -> claudeGatewayWith url opts apiKey
    GeminiProtocol -> case baseUrl of
      HttpsProviderBase url opts -> geminiGatewayWith url opts apiKey
      HttpProviderBase url opts -> geminiGatewayWith url opts apiKey
    OpenAIProtocol -> case baseUrl of
      HttpsProviderBase url opts -> openAIGatewayWithName item.providerName url opts apiKey
      HttpProviderBase url opts -> openAIGatewayWithName item.providerName url opts apiKey
    DeepSeekProtocol -> case baseUrl of
      HttpsProviderBase url opts -> deepSeekGatewayWith url opts apiKey
      HttpProviderBase url opts -> deepSeekGatewayWith url opts apiKey
    OllamaProtocol -> case baseUrl of
      HttpsProviderBase url opts -> ollamaGatewayWith url opts
      HttpProviderBase url opts -> ollamaGatewayWith url opts

resolveBaseUrl :: ProviderCatalogItem -> IO Text
resolveBaseUrl item =
  case item.baseUrlEnv of
    Nothing -> pure item.baseUrl
    Just envVar -> do
      mbOverride <- lookupEnv (T.unpack envVar)
      pure $ maybe item.baseUrl T.pack mbOverride

-- | Why a provider in the catalog has no gateway.
data ProviderUnavailable
  = -- | The provider's API key environment variable is not set.
    MissingApiKey Text
  | -- | The provider's base URL cannot be parsed (the parse error).
    InvalidBaseUrl Text
  deriving (Eq, Show)

-- | Describe why a provider is unavailable, e.g. "env var X is not set".
describeProviderUnavailable :: ProviderUnavailable -> Text
describeProviderUnavailable = \case
  MissingApiKey envVar -> "env var " <> envVar <> " is not set"
  InvalidBaseUrl err -> "it has an " <> err

-- | Build the gateway for a provider, or say why it is unavailable. Never
-- throws.
resolveGateway :: ProviderCatalogItem -> IO (Either ProviderUnavailable LLMGateway)
resolveGateway item = do
  apiKey <- case item.apiKeyEnv of
    Nothing -> pure (Right "")
    Just envVar -> maybe (Left (MissingApiKey envVar)) (Right . T.pack) <$> lookupEnv (T.unpack envVar)
  baseUrlText <- resolveBaseUrl item
  pure $ do
    key <- apiKey
    baseUrl <- either (Left . InvalidBaseUrl) Right (parseProviderBaseUrl baseUrlText)
    pure (buildGateway item baseUrl key)

-- | Resolve every provider in the catalog. See 'resolveGateway'.
resolveGateways :: ProviderCatalogMap -> IO (Map.Map Text (Either ProviderUnavailable LLMGateway))
resolveGateways = traverse resolveGateway

-- | Gateways of all providers that are available. Providers without an API
-- key or with an invalid base URL are left out.
loadGatewaysFromCatalog :: ProviderCatalogMap -> IO GatewayMap
loadGatewaysFromCatalog catalog = Map.mapMaybe (either (const Nothing) Just) <$> resolveGateways catalog

-- | Like 'loadGatewaysFromCatalog', but throws a 'LoadModelConfigError' if
-- any provider has an invalid base URL.
loadGatewaysFromCatalogOrThrow :: ProviderCatalogMap -> IO GatewayMap
loadGatewaysFromCatalogOrThrow catalog = do
  resolved <- resolveGateways catalog
  case [(name, err) | (name, Left (InvalidBaseUrl err)) <- Map.toList resolved] of
    (name, err) : _ ->
      throwIO $ LoadModelConfigError $ "Invalid baseUrl for provider " <> name <> ": " <> err
    [] -> pure $ Map.mapMaybe (either (const Nothing) Just) resolved

-- | Build a gateway map from the bundled default provider catalog and the
-- current process environment. Does not read a @.env@ file; set variables
-- yourself or use 'loadGatewaysWithDotenv'.
--
-- For custom providers, place a @providers.json@ next to your model catalog;
-- 'loadModelOrThrow' loads that file automatically.
loadGateways :: IO GatewayMap
loadGateways = loadGatewaysFromCatalog defaultProviderCatalogMap

-- | Load @.env@ (if present) and then build a gateway map from the bundled
-- default provider catalog.
loadGatewaysWithDotenv :: IO GatewayMap
loadGatewaysWithDotenv = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()
  loadGateways
