module LLM.TypesSpec (spec) where

import Data.Aeson (object, (.=))
import LLM.Core.Types
  ( ChatResponse (ChatResponse),
    ContentBlock (TextBlock, ToolCallBlock),
    LLMError (EmptyResponse, HttpError, NetworkError),
    ToolCall (ToolCall),
  )
import LLM.Core.Usage
  ( PricingInfo (..),
    Usage (..),
    addUsage,
    emptyUsage,
    estimateCost,
    pricePerMillionInput,
    pricePerMillionOutput,
    usageInputTokens,
    usageOutputTokens,
  )
import LLM.Core.Utils
  ( getToolCalls,
    hasToolCalls,
    isRetryable,
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Types" $ do
  describe "Usage" $ do
    it "emptyUsage has zero tokens" $ do
      usageInputTokens emptyUsage `shouldBe` 0
      usageOutputTokens emptyUsage `shouldBe` 0

    it "addUsage sums token counts" $ do
      let u1 = Usage 10 20 0
          u2 = Usage 30 40 0
      addUsage u1 u2 `shouldBe` Usage 40 60 0

    it "addUsage is associative" $ do
      let u1 = Usage 1 2 0
          u2 = Usage 3 4 0
          u3 = Usage 5 6 0
      addUsage (addUsage u1 u2) u3 `shouldBe` addUsage u1 (addUsage u2 u3)

  describe "estimateCost" $ do
    it "calculates cost in dollars from per-million pricing" $ do
      let pricing = PricingInfo {pricePerMillionInput = 1.0, pricePerMillionOutput = 5.0}
          usage = Usage 1_000_000 1_000_000 0
      estimateCost pricing usage `shouldBe` 6.0

    it "returns 0 for zero usage" $ do
      let pricing = PricingInfo {pricePerMillionInput = 1.0, pricePerMillionOutput = 5.0}
      estimateCost pricing emptyUsage `shouldBe` 0.0

  describe "hasToolCalls / getToolCalls" $ do
    it "returns False for text-only response" $ do
      let resp = ChatResponse "hello" [TextBlock "hello"] Nothing
      hasToolCalls resp `shouldBe` False
      getToolCalls resp `shouldBe` []

    it "returns True when tool calls present" $ do
      let tc = ToolCall "id1" "get_weather" (object ["location" .= ("London" :: String)])
          resp = ChatResponse "" [ToolCallBlock tc] Nothing
      hasToolCalls resp `shouldBe` True
      getToolCalls resp `shouldBe` [tc]

  describe "isRetryable" $ do
    it "retries on 429" $ do
      isRetryable (HttpError 429 "rate limited") `shouldBe` True
    it "retries on 503" $ do
      isRetryable (HttpError 503 "overloaded") `shouldBe` True
    it "retries on network errors" $ do
      isRetryable (NetworkError "connection refused") `shouldBe` True
    it "does not retry on 400" $ do
      isRetryable (HttpError 400 "bad request") `shouldBe` False
    it "does not retry on empty response" $ do
      isRetryable EmptyResponse `shouldBe` False
