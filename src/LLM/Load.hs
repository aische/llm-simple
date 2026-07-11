-- | Load models from a json file
--
-- @
--
-- [{
--     "modelConfigName": "llama_3_2",
--     "providerName": "ollama",
--     "modelName": "llama3.2:latest",
--     "pricing": {
--         "pricePerMillionInput": 0.0,
--         "pricePerMillionOutput": 0.0
--     },
--     "maxTokens": 1024,
--     "temperature": 0.5,
--     "requestTimeout": 10000,
--     "throttleDelay": 1000,
--     "retryCount": 3,
--     "jitterBackoff": 1000
--   },
--   ... more models ...
-- ]
--
-- @
--
-- available providers:
--
--    * "openai"
--
--    * "claude"
--
--    * "gemini"
--
--    * "ollama"
--
--    * "deepseek"
--
--
-- If a model (resp. its provider) requires an API key, it must be set in the
-- process environment. This module does not load a @.env@ file automatically;
-- use @Configuration.Dotenv.loadFile@ in your @main@ or call
-- 'loadGatewaysWithDotenv' when building gateways yourself.
--
-- @
--    GEMINI_API_KEY=...
--    CLAUDE_API_KEY=...
--    OPENAI_API_KEY=...
--    DEEPSEEK_API_KEY=...
-- @
module LLM.Load
  ( loadModelsOrThrow,
    loadModelOrThrow,
    LoadConfigError (..),
    loadGateways,
    loadGatewaysWithDotenv,
    loadModelCatalog,
    ModelCatalogItem (..),
    ModelCatalogMap,
    fsTools,
    fsTools',
  )
where

import LLM.Load.FsTools (fsTools, fsTools')
import LLM.Load.LoadGateways (loadGateways, loadGatewaysWithDotenv)
import LLM.Load.LoadModels (loadModelOrThrow, loadModelsOrThrow)
import LLM.Load.ModelCatalog (ModelCatalogItem (..), ModelCatalogMap, loadModelCatalog)
import LLM.Load.Types (LoadConfigError (..))
