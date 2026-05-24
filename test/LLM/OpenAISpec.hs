{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module LLM.OpenAISpec (spec) where

import Data.Aeson (eitherDecodeFileStrict')
import LLM.Core.Types
  ( ChatResponse (respText),
    ToolCall (tcId, tcName),
  )
import LLM.Core.Usage (Usage (Usage))
import LLM.Core.Utils (getToolCalls, hasToolCalls)
import LLM.Providers.OpenAI (parseOpenAIResponse, parseOpenAIUsage)
import Test.Hspec
  ( Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
  )

spec :: Spec
spec = describe "OpenAI" $ do
  describe "parseOpenAIResponse" $ do
    it "parses a text response" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/openai-text.json"
      case parseOpenAIResponse val of
        Right resp -> do
          respText resp `shouldBe` "Hello! How can I help you today?"
          hasToolCalls resp `shouldBe` False
        Left err -> expectationFailure $ "Parse failed: " <> show err

    it "parses a tool_calls response" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/openai-tool-use.json"
      case parseOpenAIResponse val of
        Right resp -> do
          hasToolCalls resp `shouldBe` True
          let [tc] = getToolCalls resp
          tcName tc `shouldBe` "get_weather"
          tcId tc `shouldBe` "call_abc123"
        Left err -> expectationFailure $ "Parse failed: " <> show err

  describe "parseOpenAIUsage" $ do
    it "extracts token counts" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/openai-text.json"
      parseOpenAIUsage val `shouldBe` Just (Usage 15 9 0)
