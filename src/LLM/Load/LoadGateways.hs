module LLM.Load.LoadGateways where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import Control.Lens ((<&>))
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Types (LLMGateway)
import LLM.Providers.Claude (claudeGateway)
import LLM.Providers.DeepSeek (deepSeekGateway)
import LLM.Providers.Gemini (geminiGateway)
import LLM.Providers.Ollama (ollamaGateway)
import LLM.Providers.OpenAI (openAIGateway)
import System.Environment (lookupEnv)

type GatewayMap = Map.Map Text LLMGateway

loadGateways :: IO GatewayMap
loadGateways = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()
  let ollama = Just ("ollama", ollamaGateway)
  openai <- lookupEnv "OPENAI_API_KEY" <&> fmap (("openai",) . openAIGateway . T.pack)
  claude <- lookupEnv "CLAUDE_API_KEY" <&> fmap (("claude",) . claudeGateway . T.pack)
  gemini <- lookupEnv "GEMINI_API_KEY" <&> fmap (("gemini",) . geminiGateway . T.pack)
  deepseek <- lookupEnv "DEEPSEEK_API_KEY" <&> fmap (("deepseek",) . deepSeekGateway . T.pack)
  pure $ Map.fromList $ catMaybes [openai, claude, gemini, deepseek, ollama]
