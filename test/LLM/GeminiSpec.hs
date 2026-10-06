{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module LLM.GeminiSpec (spec) where

import Data.Aeson (Value (Array, Object, String), eitherDecodeFileStrict', object, (.=))
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
    imageUrlPart,
    imageBase64Part,
  )
import LLM.Core.Usage (Usage (..), mkUsage)
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

  describe "image request encoding" $ do
    it "encodes URL fileData and base64 inlineData parts" $ do
      Right b64 <- pure $ imageBase64Part "image/jpeg" "aGVsbG8="
      let turn =
            UserMessage
              [ imageUrlPart "https://example.com/photo.png",
                b64,
                textPart "caption"
              ]
          parts = assistantParts (head (encodeTurn "gemini-3.1-flash-lite" turn))
      length parts `shouldBe` 3
      case parts of
        [Object urlP, Object b64P, Object textP] -> do
          KM.member "fileData" urlP `shouldBe` True
          KM.member "inlineData" b64P `shouldBe` True
          KM.lookup "text" textP `shouldBe` Just (String "caption")
          case KM.lookup "fileData" urlP of
            Just (Object fd) -> do
              KM.lookup "fileUri" fd
                `shouldBe` Just (String "https://example.com/photo.png")
              KM.lookup "mimeType" fd `shouldBe` Just (String "image/png")
            _ -> expectationFailure "expected fileData"
          case KM.lookup "inlineData" b64P of
            Just (Object idata) -> do
              KM.lookup "mimeType" idata `shouldBe` Just (String "image/jpeg")
              KM.lookup "data" idata `shouldBe` Just (String "aGVsbG8=")
            _ -> expectationFailure "expected inlineData"
        _ -> expectationFailure "expected three parts"

  describe "parseGeminiUsage" $ do
    it "extracts token counts" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/gemini-text.json"
      parseGeminiUsage val `shouldBe` Just (mkUsage 20 8)

    it "reports cachedContentTokenCount without double-counting promptTokenCount" $ do
      let val =
            object
              [ "usageMetadata"
                  .= object
                    [ "promptTokenCount" .= (1000 :: Int),
                      "candidatesTokenCount" .= (50 :: Int),
                      "cachedContentTokenCount" .= (800 :: Int)
                    ]
              ]
      parseGeminiUsage val
        `shouldBe` Just
          Usage
            { usageInputTokens = 1000,
              usageOutputTokens = 50,
              usageCacheReadTokens = 800,
              usageCacheCreationTokens = 0,
              usageTotalCost = 0
            }

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
