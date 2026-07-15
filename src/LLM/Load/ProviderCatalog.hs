module LLM.Load.ProviderCatalog
  ( ProviderProtocol (..),
    ProviderCatalogItem (..),
    ProviderCatalogMap,
    defaultProviderCatalogMap,
    loadProviderCatalog,
    loadProviderCatalogForModelCatalog,
  )
where

import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Load.Types (LoadConfigError (..))
import LLM.Load.Utils (decodeJsonFile)
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeDirectory)

data ProviderProtocol
  = OpenAIProtocol
  | ClaudeProtocol
  | GeminiProtocol
  | OllamaProtocol
  | DeepSeekProtocol
  deriving (Show, Eq, Ord)

instance ToJSON ProviderProtocol where
  toJSON = toJSON . protocolToText

instance FromJSON ProviderProtocol where
  parseJSON = withText "ProviderProtocol" $ \t ->
    case T.toLower t of
      "openai" -> pure OpenAIProtocol
      "claude" -> pure ClaudeProtocol
      "gemini" -> pure GeminiProtocol
      "ollama" -> pure OllamaProtocol
      "deepseek" -> pure DeepSeekProtocol
      other -> fail $ "unknown provider protocol: " <> T.unpack other

protocolToText :: ProviderProtocol -> Text
protocolToText = \case
  OpenAIProtocol -> "openai"
  ClaudeProtocol -> "claude"
  GeminiProtocol -> "gemini"
  OllamaProtocol -> "ollama"
  DeepSeekProtocol -> "deepseek"

data ProviderCatalogItem = ProviderCatalogItem
  { providerName :: Text,
    protocol :: ProviderProtocol,
    baseUrl :: Text,
    apiKeyEnv :: Maybe Text,
    baseUrlEnv :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

type ProviderCatalogMap = Map Text ProviderCatalogItem

defaultProviderCatalogMap :: ProviderCatalogMap
defaultProviderCatalogMap =
  Map.fromList
    [ ("openai", openaiItem),
      ("claude", claudeItem),
      ("gemini", geminiItem),
      ("deepseek", deepseekItem),
      ("ollama", ollamaItem)
    ]
  where
    openaiItem =
      ProviderCatalogItem
        { providerName = "openai",
          protocol = OpenAIProtocol,
          baseUrl = "https://api.openai.com",
          apiKeyEnv = Just "OPENAI_API_KEY",
          baseUrlEnv = Just "OPENAI_BASE_URL"
        }
    claudeItem =
      ProviderCatalogItem
        { providerName = "claude",
          protocol = ClaudeProtocol,
          baseUrl = "https://api.anthropic.com",
          apiKeyEnv = Just "CLAUDE_API_KEY",
          baseUrlEnv = Nothing
        }
    geminiItem =
      ProviderCatalogItem
        { providerName = "gemini",
          protocol = GeminiProtocol,
          baseUrl = "https://generativelanguage.googleapis.com",
          apiKeyEnv = Just "GEMINI_API_KEY",
          baseUrlEnv = Nothing
        }
    deepseekItem =
      ProviderCatalogItem
        { providerName = "deepseek",
          protocol = DeepSeekProtocol,
          baseUrl = "https://api.deepseek.com",
          apiKeyEnv = Just "DEEPSEEK_API_KEY",
          baseUrlEnv = Just "DEEPSEEK_BASE_URL"
        }
    ollamaItem =
      ProviderCatalogItem
        { providerName = "ollama",
          protocol = OllamaProtocol,
          baseUrl = "http://localhost:11434",
          apiKeyEnv = Nothing,
          baseUrlEnv = Just "OLLAMA_BASE_URL"
        }

loadProviderCatalog :: FilePath -> ExceptT LoadConfigError IO ProviderCatalogMap
loadProviderCatalog filePath =
  Map.fromList . fmap (\item -> (item.providerName, item))
    <$> decodeJsonFile filePath (LoadProviderCatalogError . T.pack)

loadProviderCatalogForModelCatalog :: FilePath -> ExceptT LoadConfigError IO ProviderCatalogMap
loadProviderCatalogForModelCatalog modelCatalogPath = do
  let providerPath = takeDirectory modelCatalogPath </> "providers.json"
  exists <- liftIO $ doesFileExist providerPath
  if exists
    then loadProviderCatalog providerPath
    else pure defaultProviderCatalogMap
