module LLM.OllamaSpec (spec) where

import Data.Aeson (Value (Object), object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import LLM.Core.LLMProvider (LLMProvider (..))
import LLM.Core.Types (ChatRequest (..), pattern UserTurn)
import LLM.Providers.Ollama (ollamaProvider)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Ollama" $ do
  describe "request body" $ do
    -- Ollama's OpenAI-compatible endpoint honours max_tokens and ignores
    -- num_predict and max_completion_tokens.
    it "sends max_tokens for chat requests" $ do
      let body = ollamaProvider.buildBody False req
      field "max_tokens" body `shouldBe` Just (toJSON (77 :: Int))
      field "num_predict" body `shouldBe` Nothing
      field "max_completion_tokens" body `shouldBe` Nothing

    it "sends max_tokens for object requests" $ do
      let body = ollamaProvider.buildObjectBody req (object ["type" .= ("object" :: Text)])
      field "max_tokens" body `shouldBe` Just (toJSON (77 :: Int))
      field "num_predict" body `shouldBe` Nothing
      field "max_completion_tokens" body `shouldBe` Nothing

    it "requests usage in streams" $ do
      let body = ollamaProvider.buildBody True req
      field "stream_options" body `shouldBe` Just (object ["include_usage" .= True])
  where
    req =
      ChatRequest
        { reqModel = "llama3.2",
          reqConversation = [UserTurn "hi"],
          reqSystem = Nothing,
          reqMaxTokens = 77,
          reqTemperature = Nothing,
          reqTools = [],
          reqThinking = Nothing
        }
    field k = \case
      Object o -> KM.lookup k o
      _ -> Nothing
