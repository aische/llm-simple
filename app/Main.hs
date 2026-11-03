module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (IOException, catch)
import Heptapod (generate)
import LLM.Agent
  ( Agent (..),
    RuntimeArgs (..),
    generateText,
    noEventObserver,
  )
import LLM.Core
  ( pattern UserTurn,
  )
import LLM.Generate
  ( ModelWithFallbacks (..),
    defaultDebugHooks,
    llmHooks,
  )
import LLM.Load
  ( fsTools,
    loadModelOrThrow,
  )

main :: IO ()
main = do
  loadFile defaultConfig `catch` \(_ :: IOException) -> pure ()
  -- Load only the model this example uses, so only its provider's API key
  -- (here DEEPSEEK_API_KEY) must be set. Any name from model-catalog.json
  -- works, e.g. "llama_3_2" for a local Ollama with no key.
  model <- loadModelOrThrow "./model-catalog.json" "deepseek4flash"
  let models = ModelWithFallbacks {mwfModel = model, mwfFallbacks = []}

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

  r <- generateText agent models toolMap rt [UserTurn "are there any files in the current workspace?"]
  case r of
    Left err -> putStrLn $ "Error: " <> show err
    Right resp -> putStrLn $ "Response: " <> show resp
