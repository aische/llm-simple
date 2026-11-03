{-# LANGUAGE MultiWayIf #-}

module LLM.RecordedConversationSpec (spec) where

import Data.Aeson (Value (Object, String), encode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map qualified as Map
import Heptapod (generate)
import LLM.Agent.Events (noEventObserver)
import LLM.Agent.Generate (generateText, toolBudgetUsedUpPrompt)
import LLM.Agent.ToolUtils (toTool)
import LLM.Agent.Types (Agent (..), RuntimeArgs (..))
import LLM.Core.LLMProvider (LLMProvider (..), toGateway)
import LLM.Core.Types (pattern UserTurn)
import LLM.Core.Usage (defaultPricingInfo)
import LLM.GenericConversationTest (GenericConversationTextOps (..), createSpec)
import LLM.Generate.GenerateUtils (llmHooks)
import LLM.Generate.Logger (noHooks)
import LLM.Generate.ModelConfig (ModelConfig (..), ModelWithFallbacks (..), defaultModelCapabilities)
import LLM.Generate.Types (GenerateTextResult (..))
import LLM.Providers.Claude (claudeProvider)
import LLM.Providers.DeepSeek (deepSeekProvider)
import LLM.Providers.Gemini (geminiProvider)
import LLM.Providers.Ollama (ollamaProvider)
import LLM.Providers.OpenAI (openAIProvider)
import LLM.TestKit
  ( MockRequestResponse (..),
    readMockRequestResponse,
    recordedConversationSystemPrompt,
    recordedDeepSeekThinking,
  )
import LLM.WeatherTool (weatherToolTyped)
import Test.Hspec (SpecWith, describe, expectationFailure, it, shouldBe)

ollamaConversationGeneratedFilePath :: String
ollamaConversationGeneratedFilePath = "./test/fixtures/ollama-conversation-generated.json"

ollamaConversationStreamedFilePath :: String
ollamaConversationStreamedFilePath = "./test/fixtures/ollama-conversation-streamed.json"

claudeConversationGeneratedFilePath :: String
claudeConversationGeneratedFilePath = "./test/fixtures/claude-conversation-generated.json"

claudeConversationStreamedFilePath :: String
claudeConversationStreamedFilePath = "./test/fixtures/claude-conversation-streamed.json"

geminiConversationGeneratedFilePath :: String
geminiConversationGeneratedFilePath = "./test/fixtures/gemini-conversation-generated.json"

geminiConversationStreamedFilePath :: String
geminiConversationStreamedFilePath = "./test/fixtures/gemini-conversation-streamed.json"

openAIConversationGeneratedFilePath :: String
openAIConversationGeneratedFilePath = "./test/fixtures/openai-conversation-generated.json"

openAIConversationStreamedFilePath :: String
openAIConversationStreamedFilePath = "./test/fixtures/openai-conversation-streamed.json"

deepSeekConversationGeneratedFilePath :: String
deepSeekConversationGeneratedFilePath = "./test/fixtures/deepseek-conversation-generated.json"

deepSeekConversationStreamedFilePath :: String
deepSeekConversationStreamedFilePath = "./test/fixtures/deepseek-conversation-streamed.json"

spec :: SpecWith ()
spec =
  describe "recorded conversation" $ do
    createSpec $
      GenericConversationTextOps
        "Ollama"
        ollamaProvider
        "llama3.2:latest"
        Nothing
        ollamaConversationGeneratedFilePath
        ollamaConversationStreamedFilePath
    createSpec $
      GenericConversationTextOps
        "Claude"
        (claudeProvider "")
        "claude-haiku-4-5-20251001"
        Nothing
        claudeConversationGeneratedFilePath
        claudeConversationStreamedFilePath
    createSpec $
      GenericConversationTextOps
        "Gemini"
        (geminiProvider "")
        "gemini-2.5-flash"
        Nothing
        geminiConversationGeneratedFilePath
        geminiConversationStreamedFilePath
    createSpec $
      GenericConversationTextOps
        "OpenAI"
        (openAIProvider "")
        "gpt-4.1-2025-04-14"
        Nothing
        openAIConversationGeneratedFilePath
        openAIConversationStreamedFilePath
    createSpec $
      GenericConversationTextOps
        "DeepSeek"
        (deepSeekProvider "")
        "deepseek-v4-flash"
        (Just recordedDeepSeekThinking)
        deepSeekConversationGeneratedFilePath
        deepSeekConversationStreamedFilePath

    describe "Claude final call after the tool round limit" $
      it "is the recorded follow-up request with the extra system-prompt line" $
        claudeFinalCallSpec

-- | With @agMaxToolRounds = 1@, the call after the first tool round is the
-- final call (1.16). It must be exactly the recorded second request, which
-- Claude accepted (tools declared, tool blocks in the history), except for
-- the system prompt, which gets 'toolBudgetUsedUpPrompt' appended.
claudeFinalCallSpec :: IO ()
claudeFinalCallSpec = do
  recorded <- readMockRequestResponse claudeConversationGeneratedFilePath
  case recorded of
    Just (first : second : _) -> do
      let withFinalLine = \case
            Object o
              | Just (String sys) <- KM.lookup "system" o ->
                  Object (KM.insert "system" (String (sys <> "\n\n" <> toolBudgetUsedUpPrompt)) o)
            v -> v
          expectedFinal = withFinalLine second.request
      sent <- newIORef []
      let provider =
            (claudeProvider "")
              { sendRequest = \body -> do
                  modifyIORef' sent (++ [body])
                  if
                    | body == first.request -> pure (200, BSL.toStrict (encode first.response))
                    | body == expectedFinal -> pure (200, BSL.toStrict (encode second.response))
                    | otherwise -> pure (400, "unexpected request")
              }
          model =
            ModelConfig
              { mcGateway = toGateway provider,
                mcModel = "claude-haiku-4-5-20251001",
                mcPricing = defaultPricingInfo 0 0,
                mcMaxTokens = 1024,
                mcTemperature = Nothing,
                mcThinking = Nothing,
                mcCapabilities = defaultModelCapabilities,
                mcRequestTimeout = Nothing,
                mcThrottleDelay = Nothing,
                mcRetryCount = 0,
                mcJitterBackoff = 0
              }
          agent =
            Agent
              { agName = "test",
                agSystemPrompt = Just recordedConversationSystemPrompt,
                agTools = ["get_weather"],
                agMaxToolRounds = 1,
                agContextWindow = Nothing
              }
      genId <- generate
      let rt =
            RuntimeArgs
              { rtGenerationId = genId,
                rtAbortSignal = Nothing,
                rtLLMHooks = llmHooks noHooks,
                rtHooks = noHooks,
                rtOnEvent = noEventObserver,
                rtReadonly = False
              }
          toolMap = Map.fromList [("get_weather", toTool weatherToolTyped)]
      result <- generateText agent (ModelWithFallbacks model []) toolMap rt [UserTurn "how's the weather in london?"]
      bodies <- readIORef sent
      bodies `shouldBe` [first.request, expectedFinal]
      case result of
        Right r -> length r.gtrNewMessages `shouldBe` 3
        Left err -> expectationFailure (show err)
    _ -> expectationFailure "fixture needs at least two recorded calls"
