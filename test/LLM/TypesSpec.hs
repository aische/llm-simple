module LLM.TypesSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (eitherDecode, encode, object, (.=))
import Data.Text qualified as T
import LLM.Core.Types
  ( CacheHint (..),
    ChatResponse (ChatResponse),
    ContentPart (..),
    LLMError (EmptyResponse, HttpError, NetworkError, TimeoutError),
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
    ToolResult (..),
    validateConversation,
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

  describe "validateConversation" $ do
    let call i = toolCallPart (mkToolCall i "get_weather" (object []))
        result i = ToolResult i "get_weather" "sunny"
    it "accepts a valid multi-round history" $
      validateConversation
        [ UserTurn "weather?",
          AssistantMessage [textPart "checking", call "c1", call "c2"],
          ToolTurn [result "c2", result "c1"],
          AssistantMessage [call "c3"],
          ToolTurn [result "c3"],
          AssistantMessage [textPart "sunny"],
          UserTurn "thanks"
        ]
        `shouldBe` []

    it "reports tool results that do not follow tool calls" $ do
      validateConversation [UserTurn "hi", ToolTurn [result "c1"]]
        `shouldBe` [(1, "tool results do not follow an assistant turn with tool calls")]
      validateConversation [UserTurn "hi", AssistantMessage [textPart "no calls"], ToolTurn [result "c1"]]
        `shouldBe` [(2, "tool results do not follow an assistant turn with tool calls")]

    it "reports tool calls without a following tool turn" $
      validateConversation [UserTurn "hi", AssistantMessage [call "c1"], UserTurn "again"]
        `shouldBe` [(1, "tool calls are not followed by tool results")]

    it "reports a call without a result" $
      validateConversation [UserTurn "hi", AssistantMessage [call "c1", call "c2"], ToolTurn [result "c1"]]
        `shouldBe` [(2, "no result for tool call \"c2\"")]

    it "reports a call answered twice" $
      validateConversation [UserTurn "hi", AssistantMessage [call "c1"], ToolTurn [result "c1", result "c1"]]
        `shouldBe` [(2, "tool call \"c1\" is answered more than once")]

    it "reports a result for an unknown call id" $
      validateConversation [UserTurn "hi", AssistantMessage [call "c1"], ToolTurn [result "c1", result "zz"]]
        `shouldBe` [(2, "tool result for unknown call id \"zz\"")]

    it "reports duplicate call ids in one assistant turn" $
      validateConversation [UserTurn "hi", AssistantMessage [call "c1", call "c1"], ToolTurn [result "c1"]]
        `shouldBe` [(1, "duplicate tool call id \"c1\"")]

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

    it "allows '=' only as the last one or two characters" $ do
      forM_ ["====", "A===", "ab=c", "=abc", "ab==cdef"] $ \bad ->
        imageBase64Part "image/png" bad `shouldBe` Left "image data is not valid base64"
      forM_ ["aGVsbG8=", "aGVsbA==", "aGVs", "aGVs\nbG8="] $ \good ->
        imageBase64Part "image/png" good `shouldSatisfy` either (const False) (const True)

  describe "isRetryable" $ do
    it "retries on 429" $ do
      isRetryable (HttpError 429 "rate limited") `shouldBe` True
    it "retries on 503" $ do
      isRetryable (HttpError 503 "overloaded") `shouldBe` True
    it "retries on transient server errors (500, 502, 504)" $ do
      map (\s -> isRetryable (HttpError s "")) [500, 502, 504] `shouldBe` [True, True, True]
    it "retries on network errors" $ do
      isRetryable (NetworkError "connection refused") `shouldBe` True
    it "retries on timeouts" $ do
      isRetryable TimeoutError `shouldBe` True
    it "does not retry on 400" $ do
      isRetryable (HttpError 400 "bad request") `shouldBe` False
    it "does not retry on empty response" $ do
      isRetryable EmptyResponse `shouldBe` False
