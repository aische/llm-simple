module LLM.TypesSpec (spec) where

import Data.Aeson (eitherDecode, encode, object, (.=))
import Data.Text qualified as T
import LLM.Core.Types
  ( CacheHint (..),
    ChatResponse (ChatResponse),
    ContentPart (..),
    LLMError (EmptyResponse, HttpError, NetworkError),
    PartBody (..),
    ThinkingContent (..),
    Turn (..),
    cacheEphemeral,
    imageBase64Part,
    imageUrlPart,
    mkToolCall,
    pattern UserTurn,
    textPart,
    thinkingPart,
    toolCallPart,
    validateTurn,
  )
import LLM.Core.Usage
  ( PricingInfo (..),
    Usage (..),
    addUsage,
    defaultPricingInfo,
    emptyUsage,
    estimateCost,
    mkUsage,
    usageOrdinaryInputTokens,
  )
import LLM.Core.Utils
  ( getToolCalls,
    hasToolCalls,
    isRetryable,
  )
import Test.Hspec
  ( Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
    shouldSatisfy,
  )

spec :: Spec
spec = describe "Types" $ do
  describe "Usage" $ do
    it "emptyUsage has zero tokens" $ do
      emptyUsage.usageInputTokens `shouldBe` 0
      emptyUsage.usageOutputTokens `shouldBe` 0
      emptyUsage.usageCacheReadTokens `shouldBe` 0
      emptyUsage.usageCacheCreationTokens `shouldBe` 0

    it "addUsage sums token counts including cache counters" $ do
      let u1 =
            Usage
              { usageInputTokens = 10,
                usageOutputTokens = 20,
                usageCacheReadTokens = 3,
                usageCacheCreationTokens = 2,
                usageTotalCost = 0.25
              }
          u2 =
            Usage
              { usageInputTokens = 30,
                usageOutputTokens = 40,
                usageCacheReadTokens = 4,
                usageCacheCreationTokens = 1,
                usageTotalCost = 0.5
              }
      addUsage u1 u2
        `shouldBe` Usage
          { usageInputTokens = 40,
            usageOutputTokens = 60,
            usageCacheReadTokens = 7,
            usageCacheCreationTokens = 3,
            usageTotalCost = 0.75
          }

    it "addUsage is associative" $ do
      let u1 = mkUsage 1 2
          u2 = mkUsage 3 4
          u3 = mkUsage 5 6
      addUsage (addUsage u1 u2) u3 `shouldBe` addUsage u1 (addUsage u2 u3)

    it "Semigroup matches addUsage and Monoid uses emptyUsage" $ do
      let u1 = mkUsage 10 1
          u2 = mkUsage 5 2
      (u1 <> u2) `shouldBe` addUsage u1 u2
      (mempty :: Usage) `shouldBe` emptyUsage

    it "JSON round-trips with cache fields" $ do
      let u =
            Usage
              { usageInputTokens = 100,
                usageOutputTokens = 20,
                usageCacheReadTokens = 40,
                usageCacheCreationTokens = 10,
                usageTotalCost = 0.5
              }
      eitherDecode (encode u) `shouldBe` Right u

    it "JSON decode defaults missing cache fields to zero" $ do
      let json = "{\"usageInputTokens\":12,\"usageOutputTokens\":3}"
      eitherDecode json
        `shouldBe` Right (mkUsage 12 3)

  describe "estimateCost" $ do
    it "calculates cost in dollars from per-million pricing" $ do
      let pricing = defaultPricingInfo 1.0 5.0
          usage = mkUsage 1_000_000 1_000_000
      estimateCost pricing usage `shouldBe` 6.0

    it "returns 0 for zero usage" $ do
      let pricing = defaultPricingInfo 1.0 5.0
      estimateCost pricing emptyUsage `shouldBe` 0.0

    it "prices cache read/write separately when rates are set" $ do
      let pricing =
            PricingInfo
              { pricePerMillionInput = 3.0,
                pricePerMillionOutput = 15.0,
                pricePerMillionCacheRead = Just 0.3,
                pricePerMillionCacheWrite = Just 3.75
              }
          usage =
            Usage
              { usageInputTokens = 1_000_000,
                usageOutputTokens = 0,
                usageCacheReadTokens = 400_000,
                usageCacheCreationTokens = 100_000,
                usageTotalCost = 0
              }
      -- ordinary 500k * 3 + read 400k * 0.3 + write 100k * 3.75 = 1.5 + 0.12 + 0.375
      estimateCost pricing usage `shouldBe` 1.995
      usageOrdinaryInputTokens usage `shouldBe` 500_000

    it "falls back to input rate when cache rates are absent" $ do
      let pricing = defaultPricingInfo 2.0 0.0
          usage =
            Usage
              { usageInputTokens = 1_000_000,
                usageOutputTokens = 0,
                usageCacheReadTokens = 250_000,
                usageCacheCreationTokens = 250_000,
                usageTotalCost = 0
              }
      estimateCost pricing usage `shouldBe` 2.0

  describe "PricingInfo JSON" $ do
    it "decodes catalogs without cache rates" $ do
      let json = "{\"pricePerMillionInput\":1.0,\"pricePerMillionOutput\":5.0}"
      eitherDecode json `shouldBe` Right (defaultPricingInfo 1.0 5.0)

    it "round-trips optional cache rates" $ do
      let pricing =
            PricingInfo
              { pricePerMillionInput = 3.0,
                pricePerMillionOutput = 15.0,
                pricePerMillionCacheRead = Just 0.3,
                pricePerMillionCacheWrite = Just 3.75
              }
      eitherDecode (encode pricing) `shouldBe` Right pricing

  describe "hasToolCalls / getToolCalls" $ do
    it "returns False for text-only response" $ do
      let resp = ChatResponse "hello" [textPart "hello"] Nothing Nothing
      hasToolCalls resp `shouldBe` False
      getToolCalls resp `shouldBe` []

    it "returns True when tool calls present" $ do
      let tc = mkToolCall "id1" "get_weather" (object ["location" .= ("London" :: String)])
          resp = ChatResponse "" [toolCallPart tc] Nothing Nothing
      hasToolCalls resp `shouldBe` True
      getToolCalls resp `shouldBe` [tc]

  describe "cache hints" $ do
    it "round-trips CacheEphemeral on ContentPart / Turn JSON" $ do
      let part = cacheEphemeral (textPart "cached prefix")
          turn = UserMessage [part, textPart "question"]
      eitherDecode (encode part)
        `shouldBe` Right (ContentPart (TextPart "cached prefix") (Just CacheEphemeral))
      eitherDecode (encode turn) `shouldBe` Right turn

    it "UserTurn does not match a cache-annotated single text part" $ do
      let annotated = UserMessage [cacheEphemeral (textPart "hi")]
      case annotated of
        UserTurn _ -> expectationFailure "UserTurn should not match annotated text"
        UserMessage [ContentPart (TextPart "hi") (Just CacheEphemeral)] -> pure ()
        other -> expectationFailure $ "unexpected: " <> show other

  describe "UserTurn / validateTurn" $ do
    it "UserTurn constructs a single text user message" $ do
      UserTurn "hi" `shouldBe` UserMessage [textPart "hi"]

    it "rejects thinking parts on user messages" $ do
      validateTurn
        ( UserMessage
            [thinkingPart (ThinkingContent (Just "nope") Nothing)]
        )
        `shouldBe` Left "user messages may not contain thinking parts"

    it "allows image parts on user messages" $ do
      validateTurn
        ( UserMessage
            [imageUrlPart "https://example.com/a.png", textPart "look"]
        )
        `shouldBe` Right ()

    it "rejects image parts on assistant messages" $ do
      validateTurn
        (AssistantMessage [imageUrlPart "https://example.com/a.png"])
        `shouldBe` Left "assistant messages may not contain image parts"

  describe "imageBase64Part" $ do
    it "accepts valid png base64" $ do
      imageBase64Part "image/png" "aGVsbG8=" `shouldSatisfy` either (const False) (const True)

    it "rejects unsupported MIME types" $ do
      case imageBase64Part "image/svg+xml" "aGVsbG8=" of
        Left msg -> msg `shouldSatisfy` T.isInfixOf "unsupported image media type"
        Right _ -> fail "expected MIME failure"

    it "rejects empty base64" $ do
      imageBase64Part "image/png" "   " `shouldBe` Left "image base64 data must not be empty"

    it "rejects invalid base64 characters" $ do
      imageBase64Part "image/png" "!!!!" `shouldBe` Left "image data is not valid base64"

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
