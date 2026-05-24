{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module LLM.GeminiSpec (spec) where

import Data.Aeson (eitherDecodeFileStrict')
import LLM.Core.Types (ChatResponse (respText), ToolCall (tcName))
import LLM.Core.Usage (Usage (Usage))
import LLM.Core.Utils (getToolCalls, hasToolCalls)
import LLM.Providers.Gemini (parseGeminiResponse, parseGeminiUsage)
import Test.Hspec
  ( Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
  )

spec :: Spec
spec = describe "Gemini" $ do
  describe "parseGeminiResponse" $ do
    it "parses a text response" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/gemini-text.json"
      resp <- parseGeminiResponse val
      case resp of
        Right r -> do
          respText r `shouldBe` "Hello! How can I help you today?"
          hasToolCalls r `shouldBe` False
        Left err -> expectationFailure $ "Parse failed: " <> show err

    it "parses a function call response" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/gemini-tool-use.json"
      resp <- parseGeminiResponse val
      case resp of
        Right r -> do
          hasToolCalls r `shouldBe` True
          let [tc] = getToolCalls r
          tcName tc `shouldBe` "get_weather"
        Left err -> expectationFailure $ "Parse failed: " <> show err

  describe "parseGeminiUsage" $ do
    it "extracts token counts" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/gemini-text.json"
      parseGeminiUsage val `shouldBe` Just (Usage 20 8 0)
