{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Autodocodec qualified as AC
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException (SomeException), catch)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import Heptapod (generate)
import LLM
  ( AbortSignal,
    Hooks (..),
    ThinkingMode (..),
    claudeGateway,
    deepSeekGateway,
    defaultDebugHooks,
    generateText,
    mkFsConfig,
    noEventObserver,
  )
import LLM.Agent.Types (Agent (..), RuntimeArgs (..))
import LLM.Core.Types (LLMHooks (..), ToolDef (..), Turn (UserTurn))
import LLM.Core.Usage (PricingInfo (..), Usage)
import LLM.Generate.Logger (noHooks)
import LLM.Generate.ModelConfig (ModelConfig (..), ModelWithFallbacks (..))
import LLM.Generate.Types
  ( GenerateErrorResult (..),
    GenerateTextResult (..),
    StreamChunk (..),
  )
import LLM.Load.FsTools (fsTools)
import LLM.Load.LoadModels (loadModelsOrThrow)
import System.Environment (getArgs, getEnv)

main :: IO ()
main = do
  (_gpt, llama, _haiku, gemini, mistral, deepseek) <-
    loadModelsOrThrow
      "./model-catalog.json"
      ("gpt_4_1", "llama_3_2", "haiku_4_5", "gemini_2_5_flash", "mistral", "deepseek4flash")

  let _models1 = ModelWithFallbacks {mwfModel = llama, mwfFallbacks = []}
      _models2 = ModelWithFallbacks {mwfModel = mistral, mwfFallbacks = []}
      _models3 = ModelWithFallbacks {mwfModel = gemini, mwfFallbacks = []}
      _models4 = ModelWithFallbacks {mwfModel = deepseek, mwfFallbacks = []}

  toolMap <-
    fsTools "./user-workspace/"
  genId <- generate
  let agent =
        Agent
          { agName = "friendly-assistant",
            agSystemPrompt = Nothing,
            agTools = ["directory_tree"],
            agMaxToolRounds = 10,
            agContextWindow = Nothing
          }
      rt =
        RuntimeArgs
          { rtGenerationId = genId,
            rtAbortSignal = Nothing,
            rtLLMHooks = llmHooks defaultDebugHooks,
            rtHooks = defaultDebugHooks,
            rtOnEvent = noEventObserver,
            rtReadonly = False
          }

  r <- generateText agent _models4 toolMap rt [UserTurn "are there any files in the current workspace?"]
  case r of
    Left err -> putStrLn $ "Error: " <> show err
    Right resp -> putStrLn $ "Response: " <> show resp

llmHooks :: Hooks -> LLMHooks
llmHooks hooks =
  LLMHooks
    { onLLMRequest = hooks.onRequest,
      onLLMResponse = hooks.onResponse,
      onLLMResponseError = hooks.onResponseError
    }
