{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module LLM.ClaudeSpec (spec) where

import Data.Aeson (eitherDecodeFileStrict')
import LLM.Core.Types
  ( ChatResponse (respText),
    ToolCall (tcId, tcName),
  )
import LLM.Core.Usage (Usage (Usage))
import LLM.Core.Utils (getToolCalls, hasToolCalls)
import LLM.Providers.Claude (parseClaudeResponse, parseClaudeUsage)
import Test.Hspec
  ( Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
  )

spec :: Spec
spec = describe "Claude" $ do
  describe "parseClaudeResponse" $ do
    it "parses a text response" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/claude-text.json"
      case parseClaudeResponse val of
        Right resp -> do
          respText resp `shouldBe` "Hello! How can I help you today?"
          hasToolCalls resp `shouldBe` False
        Left err -> expectationFailure $ "Parse failed: " <> show err

    it "parses a tool_use response" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/claude-tool-use.json"
      case parseClaudeResponse val of
        Right resp -> do
          respText resp `shouldBe` "Let me check the weather for you."
          hasToolCalls resp `shouldBe` True
          let [tc] = getToolCalls resp
          tcName tc `shouldBe` "get_weather"
          tcId tc `shouldBe` "toolu_01A09q90qw90lq917835lq9"
        Left err -> expectationFailure $ "Parse failed: " <> show err

  describe "parseClaudeUsage" $ do
    it "extracts token counts" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/claude-text.json"
      parseClaudeUsage val `shouldBe` Just (Usage 25 10 0)

    it "extracts token counts from tool_use response" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/claude-tool-use.json"
      parseClaudeUsage val `shouldBe` Just (Usage 50 35 0)
