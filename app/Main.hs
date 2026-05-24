{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import Heptapod (generate)
import LLM (ollamaGateway, toTool)
import LLM.Core.Types (LLMHooks (..), Turn (UserTurn))
import LLM.Core.Usage (PricingInfo (..), Usage)
import LLM.Generate.Generate (generateText, streamText)
import LLM.Generate.GenerateObject (generateObject)
import LLM.Generate.Logger (noHooks)
import LLM.Generate.ModelConfig (ModelConfig (..), ModelWithFallbacks (..))
import LLM.Generate.Types
  ( Agent (..),
    GenerateEvent (..),
    RuntimeArgs (..),
    StreamChunk (..),
  )
import LLM.Tools.DirectoryTree (directoryTreeToolTyped)
import LLM.Tools.FsConfig (FsConfig (..))
import LLM.Tools.Readdir (readdirToolTyped)
import LLM.Tools.Readfile (readfileToolTyped)
import LLM.Tools.Writefile (writefileToolTyped)

agent :: Agent
agent =
  let fsConfig = FsConfig {fsBasePath = "./user-workspace/"}
   in Agent
        { agName = "default",
          agSystemPrompt = Just "You are a helpful assistant.",
          agTools =
            [ toTool $ readfileToolTyped fsConfig,
              toTool $ writefileToolTyped fsConfig,
              toTool $ readdirToolTyped fsConfig,
              toTool $ directoryTreeToolTyped fsConfig
            ],
          agWorkers = Nothing,
          agMaxToolRounds = 3,
          agContextWindow = Nothing
        }

model1 :: ModelConfig
model1 =
  ModelConfig
    { mcGateway = ollamaGateway,
      mcModel = "llama3.2:latest",
      mcPricing = PricingInfo {pricePerMillionInput = 1.0, pricePerMillionOutput = 5.00},
      mcMaxTokens = 1024,
      mcTemperature = Nothing,
      mcRequestTimeout = Nothing,
      mcThrottleDelay = Nothing,
      mcRetryCount = 0,
      mcJitterBackoff = 1_000
    }

main :: IO ()
main = do
  let models = ModelWithFallbacks model1 []
  uuid1 <- generate
  let runtime =
        RuntimeArgs
          { rtGenerationId = uuid1,
            rtAbortSignal = Nothing,
            rtLLMHooks = LLMHooks {onLLMRequest = \_ _ -> pure (), onLLMResponse = \_ _ -> pure (), onLLMResponseError = \_ _ -> pure ()},
            rtHooks = noHooks,
            rtOnEvent = printEvent,
            rtReadonly = False
          }
  let currentConversation = [UserTurn "What is the capital of France? Write a poem about it, 4 paragraphs long"]

  -- r <- generateText agent models runtime currentConversation
  r <- streamText onStreamChunk agent models runtime currentConversation
  print r

  o <- generateObject agent models runtime currentConversation
  printExampleObject o

printExampleObject :: Either a (ExampleObject, Usage) -> IO ()
printExampleObject (Right (o, _)) = print o
printExampleObject (Left _) = pure ()

onStreamChunk :: StreamChunk -> IO ()
onStreamChunk = \case
  AnswerDelta _ txt -> TIO.putStr txt
  PreambleDelta txt -> TIO.putStr txt
  StreamToolCallChunk _ -> pure ()

printEvent :: GenerateEvent -> IO ()
printEvent = print

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
        <$> AC.requiredField "title" "title of the example" AC..= _title
        <*> AC.requiredField "content" "content of the example" AC..= _content
        <*> AC.requiredField "rating" "quality of the example (1..10)" AC..= _rating
        <*> AC.requiredField "flag" "is the example good?" AC..= _flag
