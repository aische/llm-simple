{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Autodocodec qualified as AC
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException (SomeException), catch)
import Data.Aeson (FromJSON)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import Heptapod (generate)
import LLM (ThinkingMode (..), deepSeekGateway, mkFsConfig, ollamaGateway, openAIGateway, toTool)
import LLM.Agent.Generate4 (generateText, streamText)
import LLM.Agent.GenerateObject (generateObject)
import LLM.Agent.Types
  ( Agent (..),
    GenerateEvent (..),
    RuntimeArgs (..),
  )
import LLM.Core.Types (LLMHooks (..), Turn (UserTurn))
import LLM.Core.Usage (PricingInfo (..), Usage)
import LLM.Generate.Logger (noHooks)
import LLM.Generate.ModelConfig (ModelConfig (..), ModelWithFallbacks (..))
import LLM.Generate.Types (StreamChunk (..))
import LLM.Tools.DirectoryTree (directoryTreeToolTyped)
import LLM.Tools.FsConfig (FsConfig)
import LLM.Tools.Readdir (readdirToolTyped)
import LLM.Tools.Readfile (readfileToolTyped)
import LLM.Tools.Writefile (writefileToolTyped)
import System.Environment (getEnv)
import UTool1 (uTool1)

createAgent :: FsConfig -> Agent
createAgent _fsConfig =
  Agent
    { agName = "default",
      agSystemPrompt = Just "You are a helpful assistant.",
      agTools =
        [],
      agUTools = ["subagent"],
      agMaxToolRounds = 3,
      agContextWindow = Nothing
    }

createAgent2 :: FsConfig -> Agent
createAgent2 fsConfig =
  Agent
    { agName = "default",
      agSystemPrompt = Just "You are a helpful assistant.",
      agTools =
        [ toTool $ readfileToolTyped fsConfig,
          toTool $ writefileToolTyped fsConfig,
          toTool $ readdirToolTyped fsConfig,
          toTool $ directoryTreeToolTyped fsConfig
        ],
      agUTools = [],
      agMaxToolRounds = 3,
      agContextWindow = Nothing
    }

model1 :: Text -> ModelConfig
model1 apiKey =
  ModelConfig
    { mcGateway = deepSeekGateway apiKey,
      mcModel = "deepseek-v4-flash",
      -- mcGateway = openAIGateway apiKey,
      -- mcModel = "gpt-4.1-2025-04-14",
      mcPricing = PricingInfo {pricePerMillionInput = 1.0, pricePerMillionOutput = 5.00},
      mcMaxTokens = 1024,
      mcTemperature = Nothing,
      mcThinking = Just ThinkingMode {tmEnabled = True, tmEffort = Just "max"},
      mcRequestTimeout = Nothing,
      mcThrottleDelay = Nothing,
      mcRetryCount = 0,
      mcJitterBackoff = 1_000
    }

main :: IO ()
main = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()
  openAIGatewayKey <- getEnv "DEEPSEEK_API_KEY"
  -- openAIGatewayKey <- getEnv "OPENAI_API_KEY"
  let models = ModelWithFallbacks (model1 $ T.pack openAIGatewayKey) []
  uuid1 <- generate
  fsConfig <- mkFsConfig "./user-workspace/"
  let agent = createAgent fsConfig
  let agent2 = createAgent2 fsConfig
  let runtime =
        RuntimeArgs
          { rtGenerationId = uuid1,
            rtAbortSignal = Nothing,
            rtLLMHooks = LLMHooks {onLLMRequest = \_ _ -> pure (), onLLMResponse = \_ _ -> pure (), onLLMResponseError = \_ _ -> pure ()},
            rtHooks = noHooks,
            rtOnEvent = printEvent,
            rtReadonly = False
          }
  -- let currentConversation = [UserTurn "What is the capital of France? Write a poem about it, 4 paragraphs long"]
  let currentConversation = [UserTurn "Summarize all files in the workspace"]
      utoolRegistry =
        Map.fromList
          [ ("subagent", uTool1 (agent2, models, runtime))
          ]

  -- r <- generateText agent models runtime currentConversation
  r <- streamText utoolRegistry onStreamChunk agent models runtime currentConversation
  print r

-- o <- generateObject agent models runtime currentConversation
-- printExampleObject o

printExampleObject :: Either a (ExampleObject, Usage) -> IO ()
printExampleObject (Right (o, _)) = print o
printExampleObject (Left _) = pure ()

onStreamChunk :: StreamChunk -> IO ()
onStreamChunk = \case
  AnswerDelta txt -> TIO.putStr txt
  ReasoningDelta txt -> TIO.putStr txt
  PreambleDelta txt -> TIO.putStr txt
  StreamToolCallChunk _ -> pure ()

printEvent :: GenerateEvent -> IO ()
printEvent ev = do
  putStrLn "--------------------------------"
  print ev

data ExampleObject = ExampleObject
  { _title :: Text,
    _content :: Text,
    _rating :: Int,
    _flag :: Bool
  }
  deriving (Show, Generic)
  deriving (FromJSON) via (AC.Autodocodec ExampleObject)

instance AC.HasCodec ExampleObject where
  codec :: AC.JSONCodec ExampleObject
  codec =
    AC.object "ExampleObject" $
      ExampleObject
        <$> AC.requiredField "title" "title of the example" AC..= (\x -> x._title)
        <*> AC.requiredField "content" "content of the example" AC..= (\x -> x._content)
        <*> AC.requiredField "rating" "quality of the example (1..10)" AC..= (\x -> x._rating)
        <*> AC.requiredField "flag" "is the example good?" AC..= (\x -> x._flag)
