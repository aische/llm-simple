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
    defaultMessageEncodeOptions,
    imageBase64Part,
    imageUrlPart,
    textPart,
  )
import LLM.Core.Usage (Usage (Usage))
import LLM.Core.Utils (getToolCalls, hasToolCalls)
import LLM.Providers.OpenAI (encodeTurn, parseOpenAIResponse, parseOpenAIUsage)
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

  describe "parseOpenAIUsage" $ do
    it "extracts token counts" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/openai-text.json"
      parseOpenAIUsage val `shouldBe` Just (Usage 15 9 0)

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

nestedText :: [Text] -> Value -> Maybe Text
nestedText [key] v = lookupText key v
nestedText (key : rest) (Object o) =
  KM.lookup (K.fromText key) o >>= nestedText rest
nestedText _ _ = Nothing
