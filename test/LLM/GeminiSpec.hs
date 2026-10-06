{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module LLM.GeminiSpec (spec) where

import Data.Aeson (Value (Array, Object), eitherDecodeFileStrict', object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Vector qualified as V
import LLM.Core.Types
  ( ChatResponse (respText),
    ProviderOpaque (..),
    ThinkingContent (..),
    ToolCall (..),
    Turn (..),
    mkToolCall,
    textPart,
    thinkingPart,
    toolCallPart,
  )
import LLM.Core.Usage (Usage (Usage))
import LLM.Core.Utils (getToolCalls, hasToolCalls)
import LLM.Providers.Gemini (encodeTurn, parseGeminiResponse, parseGeminiUsage, signatureForModel)
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
          r.respText `shouldBe` "Hello! How can I help you today?"
          hasToolCalls r `shouldBe` False
        Left err -> expectationFailure $ "Parse failed: " <> show err

    it "parses a function call response" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/gemini-tool-use.json"
      resp <- parseGeminiResponse val
      case resp of
        Right r -> do
          hasToolCalls r `shouldBe` True
          let [tc] = getToolCalls r
          tc.tcName `shouldBe` "get_weather"
        Left err -> expectationFailure $ "Parse failed: " <> show err

  describe "thought signature round-trip" $ do
    it "replays signature only for a matching model" $ do
      let tc =
            (mkToolCall "call_1" "get_weather" (object ["location" .= ("London" :: Text)]))
              { tcProviderMeta =
                  Just
                    ProviderOpaque
                      { poProvider = "gemini",
                        poModel = Just "gemini-2.5-pro-001",
                        poPayload = object ["thoughtSignature" .= ("sig-xyz" :: Text)]
                      }
              }
          turn = AssistantMessage [toolCallPart tc]
          matching = head (encodeTurn "gemini-2.5-pro" turn)
          mismatch = head (encodeTurn "gemini-2.5-flash" turn)
      hasThoughtSignature matching `shouldBe` True
      hasThoughtSignature mismatch `shouldBe` False
      signatureForModel "gemini-2.5-pro" tc.tcProviderMeta `shouldBe` Just "sig-xyz"
      signatureForModel "gemini-2.5-flash" tc.tcProviderMeta `shouldBe` Nothing

    it "omits foreign Claude thinking opaque while keeping text and tool calls" $ do
      let turn =
            AssistantMessage
              [ thinkingPart
                  ThinkingContent
                    { thinkingText = Just "claude thoughts",
                      thinkingOpaque =
                        Just
                          ProviderOpaque
                            { poProvider = "claude",
                              poModel = Just "claude-haiku-4-5-20251001",
                              poPayload =
                                object
                                  [ "type" .= ("thinking" :: Text),
                                    "signature" .= ("sig" :: Text)
                                  ]
                            }
                    },
                textPart "hello",
                toolCallPart (mkToolCall "c1" "get_weather" (object []))
              ]
          encoded = head (encodeTurn "gemini-2.5-flash" turn)
          parts = assistantParts encoded
      -- Foreign opaque dropped; plain thinking text may remain as thought part,
      -- plus text and functionCall => at least text + functionCall.
      length parts `shouldBe` 3
      hasFunctionCall encoded `shouldBe` True
      hasThoughtSignature encoded `shouldBe` False

  describe "parseGeminiUsage" $ do
    it "extracts token counts" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/gemini-text.json"
      parseGeminiUsage val `shouldBe` Just (Usage 20 8 0)

hasThoughtSignature :: Value -> Bool
hasThoughtSignature (Object o) =
  case KM.lookup "parts" o of
    Just (Array arr) ->
      any
        ( \case
            Object p -> KM.member "thoughtSignature" p
            _ -> False
        )
        arr
    _ -> False
hasThoughtSignature _ = False

hasFunctionCall :: Value -> Bool
hasFunctionCall (Object o) =
  case KM.lookup "parts" o of
    Just (Array arr) ->
      any
        ( \case
            Object p -> KM.member "functionCall" p
            _ -> False
        )
        arr
    _ -> False
hasFunctionCall _ = False

assistantParts :: Value -> [Value]
assistantParts (Object o) =
  case KM.lookup "parts" o of
    Just (Array arr) -> V.toList arr
    _ -> []
assistantParts _ = []
