{-# LANGUAGE ImpredicativeTypes #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import Heptapod (generate)
import LLM.Agent
  ( Agent (..),
    RuntimeArgs (..),
    generateText,
    noEventObserver,
  )
import LLM.Core
  ( Turn (UserTurn),
  )
import LLM.Generate
  ( ModelWithFallbacks (..),
    defaultDebugHooks,
    llmHooks,
  )
import LLM.Load
  ( fsTools,
    loadModelsOrThrow,
  )

main :: IO ()
main = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()
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
