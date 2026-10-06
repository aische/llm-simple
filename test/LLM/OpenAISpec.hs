{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module LLM.OpenAISpec (spec) where

import Data.Aeson
  ( Value (Array, Object, String),
    eitherDecodeFileStrict',
    object,
    (.=),
  )
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Vector qualified as V
import LLM.Core.Types
  ( ChatResponse (respText),
    ToolCall (tcId, tcName),
    Turn (..),
    cacheEphemeral,
    defaultMessageEncodeOptions,
    imageBase64Part,
    imageUrlPart,
    textPart,
  )
import LLM.Core.Usage (Usage (..), mkUsage)
import LLM.Core.Utils (getToolCalls, hasToolCalls)
import LLM.Providers.OpenAI (encodeTurn, parseOpenAIResponse, parseOpenAIUsage)
import Test.Hspec
  ( Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
    shouldSatisfy,
  )

spec :: Spec
spec = describe "OpenAI" $ do
  describe "parseOpenAIResponse" $ do
    it "parses a text response" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/openai-text.json"
      case parseOpenAIResponse val of
        Right resp -> do
          resp.respText `shouldBe` "Hello! How can I help you today?"
          hasToolCalls resp `shouldBe` False
        Left err -> expectationFailure $ "Parse failed: " <> show err

    it "parses a tool_calls response" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/openai-tool-use.json"
      case parseOpenAIResponse val of
        Right resp -> do
          hasToolCalls resp `shouldBe` True
          let [tc] = getToolCalls resp
          tc.tcName `shouldBe` "get_weather"
          tc.tcId `shouldBe` "call_abc123"
        Left err -> expectationFailure $ "Parse failed: " <> show err

  describe "image request encoding" $ do
    it "encodes URL and base64 data-URL image parts" $ do
      Right b64 <- pure $ imageBase64Part "image/png" "aGVsbG8="
      let turn =
            UserMessage
              [ textPart "What is in this image?",
                imageUrlPart "https://example.com/boardwalk.jpg",
                b64
              ]
          content = userContent (head (encodeTurn defaultMessageEncodeOptions turn))
      contentTypes content `shouldBe` ["text", "image_url", "image_url"]
      case content of
        [Object textO, Object urlO, Object b64O] -> do
          lookupText "text" (Object textO) `shouldBe` Just "What is in this image?"
          nestedText ["image_url", "url"] (Object urlO)
            `shouldBe` Just "https://example.com/boardwalk.jpg"
          nestedText ["image_url", "url"] (Object b64O)
            `shouldBe` Just "data:image/png;base64,aGVsbG8="
        _ -> expectationFailure "expected text + two image_url parts"

  describe "cache hint encoding" $ do
    it "omits cache hints from the wire while keeping content identical" $ do
      Right b64 <- pure $ imageBase64Part "image/png" "aGVsbG8="
      let plain =
            UserMessage
              [ textPart "What is in this image?",
                imageUrlPart "https://example.com/boardwalk.jpg",
                b64
              ]
          hinted =
            UserMessage
              [ cacheEphemeral (textPart "What is in this image?"),
                imageUrlPart "https://example.com/boardwalk.jpg",
                cacheEphemeral b64
              ]
          plainMsg = head (encodeTurn defaultMessageEncodeOptions plain)
          hintedMsg = head (encodeTurn defaultMessageEncodeOptions hinted)
      hintedMsg `shouldBe` plainMsg
      case userContent hintedMsg of
        parts -> do
          contentTypes parts `shouldBe` ["text", "image_url", "image_url"]
          mapM_ (\p -> lookupKey "cache_control" p `shouldBe` Nothing) parts

    it "Ollama (shared OpenAI encoder) also omits cache hints" $ do
      -- Ollama requests use the same encodeTurn + defaultMessageEncodeOptions path.
      let plain = UserMessage [textPart "hello"]
          hinted = UserMessage [cacheEphemeral (textPart "hello")]
      head (encodeTurn defaultMessageEncodeOptions hinted)
        `shouldBe` head (encodeTurn defaultMessageEncodeOptions plain)

  describe "parseOpenAIUsage" $ do
    it "extracts token counts" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/openai-text.json"
      parseOpenAIUsage val `shouldBe` Just (mkUsage 15 9)

    it "reports cached_tokens without double-counting prompt_tokens" $ do
      let val =
            object
              [ "usage"
                  .= object
                    [ "prompt_tokens" .= (2006 :: Int),
                      "completion_tokens" .= (300 :: Int),
                      "prompt_tokens_details"
                        .= object
                          [ "cached_tokens" .= (1920 :: Int),
                            "cache_write_tokens" .= (40 :: Int)
                          ]
                    ]
              ]
      parseOpenAIUsage val
        `shouldBe` Just
          Usage
            { usageInputTokens = 2006,
              usageOutputTokens = 300,
              usageCacheReadTokens = 1920,
              usageCacheCreationTokens = 40,
              usageTotalCost = 0
            }

    it "reads zero cached_tokens from recorded conversation fixtures" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/openai-conversation-generated.json"
      case val of
        Array arr ->
          case V.toList arr of
            (Object first : _) ->
              case KM.lookup "response" first of
                Just resp ->
                  case parseOpenAIUsage resp of
                    Just u -> do
                      u.usageCacheReadTokens `shouldBe` 0
                      u.usageInputTokens `shouldSatisfy` (> 0)
                    Nothing -> expectationFailure "failed to parse usage"
                _ -> expectationFailure "missing response"
            _ -> expectationFailure "empty conversation array"
        _ -> expectationFailure "expected conversation array"

userContent :: Value -> [Value]
userContent (Object o) =
  case KM.lookup "content" o of
    Just (Array arr) -> V.toList arr
    _ -> []
userContent _ = []

contentTypes :: [Value] -> [Text]
contentTypes = mapMaybe typ
  where
    typ (Object o) = case KM.lookup "type" o of
      Just (String t) -> Just t
      _ -> Nothing
    typ _ = Nothing

lookupText :: Text -> Value -> Maybe Text
lookupText key (Object o) =
  case KM.lookup (K.fromText key) o of
    Just (String t) -> Just t
    _ -> Nothing
lookupText _ _ = Nothing

lookupKey :: Text -> Value -> Maybe Value
lookupKey key (Object o) = KM.lookup (K.fromText key) o
lookupKey _ _ = Nothing

nestedText :: [Text] -> Value -> Maybe Text
nestedText [key] v = lookupText key v
nestedText (key : rest) (Object o) =
  KM.lookup (K.fromText key) o >>= nestedText rest
nestedText _ _ = Nothing
