module LLM.RecordedConversationSpec (spec) where

import LLM.GenericConversationTest (GenericConversationTextOps (..), createSpec)
import LLM.Providers.Claude (claudeProvider)
import LLM.Providers.Gemini (geminiProvider)
import LLM.Providers.Ollama (ollama)
import LLM.Providers.OpenAI (openAIProvider)
import Test.Hspec (SpecWith, describe)

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

spec :: SpecWith ()
spec =
  describe "recorded conversation" $ do
    createSpec $
      GenericConversationTextOps
        "Ollama"
        ollama
        "llama3.2:latest"
        ollamaConversationGeneratedFilePath
        ollamaConversationStreamedFilePath
    createSpec $
      GenericConversationTextOps
        "Claude"
        (claudeProvider "")
        "claude-haiku-4-5-20251001"
        claudeConversationGeneratedFilePath
        claudeConversationStreamedFilePath
    createSpec $
      GenericConversationTextOps
        "Gemini"
        (geminiProvider "")
        "gemini-2.5-flash"
        geminiConversationGeneratedFilePath
        geminiConversationStreamedFilePath
    createSpec $
      GenericConversationTextOps
        "OpenAI"
        (openAIProvider "")
        "gpt-4.1-2025-04-14"
        openAIConversationGeneratedFilePath
        openAIConversationStreamedFilePath
