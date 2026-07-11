module LLM.Load.LoadGateways
  ( GatewayMap,
    loadGateways,
    loadGatewaysWithDotenv,
  )
where

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

-- | Build a gateway map from the current process environment.
-- Does not read a @.env@ file; set variables yourself or use
-- 'loadGatewaysWithDotenv'.
loadGateways :: IO GatewayMap
loadGateways = loadGatewaysFromEnv

-- | Load @.env@ (if present) and then build a gateway map from the
-- environment. Convenience for local development; prefer explicit env
-- injection in production.
loadGatewaysWithDotenv :: IO GatewayMap
loadGatewaysWithDotenv = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()
  loadGatewaysFromEnv

loadGatewaysFromEnv :: IO GatewayMap
loadGatewaysFromEnv = do
  let ollama = Just ("ollama", ollamaGateway)
  openai <- lookupEnv "OPENAI_API_KEY" <&> fmap (("openai",) . openAIGateway . T.pack)
  claude <- lookupEnv "CLAUDE_API_KEY" <&> fmap (("claude",) . claudeGateway . T.pack)
  gemini <- lookupEnv "GEMINI_API_KEY" <&> fmap (("gemini",) . geminiGateway . T.pack)
  deepseek <- lookupEnv "DEEPSEEK_API_KEY" <&> fmap (("deepseek",) . deepSeekGateway . T.pack)
  pure $ Map.fromList $ catMaybes [openai, claude, gemini, deepseek, ollama]
