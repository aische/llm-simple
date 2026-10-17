{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module LLM.GeminiSpec (spec) where

import Data.Aeson (Value (Array, Object, String), eitherDecodeFileStrict', object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import LLM.Core.LLMProvider (LLMProvider (..))
import LLM.Core.ProviderUtils (normalizeSchemaOpenAI, stripBoundsAndComments)
import LLM.Core.Types
  ( ChatRequest (..),
    ChatResponse (respText),
    ToolDef (..),
    ProviderOpaque (..),
    ThinkingContent (..),
    ThinkingMode (..),
    ToolCall (..),
    Turn (..),
    mkToolCall,
    textPart,
    thinkingPart,
    toolCallPart,
    imageUrlPart,
    imageBase64Part,
    cacheEphemeral,
  )
import LLM.Core.Usage (Usage (..), mkUsage)
import LLM.Core.Utils (getToolCalls, hasToolCalls)
import LLM.Providers.Gemini (encodeTurn, geminiProvider, parseGeminiResponse, parseGeminiUsage, signatureForModel, skipThoughtSignatureValidator)
import Test.Hspec
  ( Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
  )

spec :: Spec
spec = describe "Gemini" $ do
  describe "stripBoundsAndComments" $ do
    it "keeps properties named minimum/maximum and strips their constraints" $ do
      let schema =
            object
              [ "type" .= ("object" :: Text),
                "properties"
                  .= object
                    [ "minimum" .= object ["type" .= ("number" :: Text), "minimum" .= (0 :: Int), "$comment" .= ("c" :: Text)],
                      "maximum" .= object ["type" .= ("string" :: Text)]
                    ]
              ]
      stripBoundsAndComments schema
        `shouldBe` object
          [ "type" .= ("object" :: Text),
            "properties"
              .= object
                [ "minimum" .= object ["type" .= ("number" :: Text)],
                  "maximum" .= object ["type" .= ("string" :: Text)]
                ]
          ]

    it "recurses into items and anyOf but leaves default values alone" $ do
      let schema =
            object
              [ "type" .= ("array" :: Text),
                "items"
                  .= object
                    [ "anyOf"
                        .= [ object ["type" .= ("integer" :: Text), "maximum" .= (9 :: Int)],
                             object ["type" .= ("null" :: Text)]
                           ]
                    ],
                "default" .= [object ["minimum" .= (1 :: Int)]]
              ]
      stripBoundsAndComments schema
        `shouldBe` object
          [ "type" .= ("array" :: Text),
            "items"
              .= object
                [ "anyOf"
                    .= [ object ["type" .= ("integer" :: Text)],
                         object ["type" .= ("null" :: Text)]
                       ]
                ],
            "default" .= [object ["minimum" .= (1 :: Int)]]
          ]

  describe "normalizeSchemaOpenAI" $ do
    it "does not treat a property named type as an object schema" $ do
      let schema =
            object
              [ "type" .= ("object" :: Text),
                "properties" .= object ["type" .= object ["type" .= ("string" :: Text)]]
              ]
      normalizeSchemaOpenAI schema
        `shouldBe` object
          [ "type" .= ("object" :: Text),
            "properties" .= object ["type" .= object ["type" .= ("string" :: Text)]],
            "additionalProperties" .= False,
            "required" .= (["type"] :: [Text])
          ]

  describe "request body schema stripping" $ do
    it "strips bounds from tool parameters but not from conversation content" $ do
      let params =
            object
              [ "type" .= ("object" :: Text),
                "properties" .= object ["minimum" .= object ["type" .= ("number" :: Text), "minimum" .= (0 :: Int)]]
              ]
          args = object ["minimum" .= (5 :: Int)]
          req =
            ChatRequest
              { reqModel = "gemini-2.5-flash",
                reqConversation = [AssistantMessage [toolCallPart (mkToolCall "c1" "f" args)]],
                reqSystem = Nothing,
                reqMaxTokens = 100,
                reqTemperature = Nothing,
                reqTools = [ToolDef {toolName = "f", toolDescription = "d", toolReadonly = True, toolParameters = params}],
                reqThinking = Nothing
              }
          body = (geminiProvider "").buildBody False req
          strippedParams =
            object
              [ "type" .= ("object" :: Text),
                "properties" .= object ["minimum" .= object ["type" .= ("number" :: Text)]]
              ]
      lookupPath ["tools", "0", "function_declarations", "0", "parameters"] body `shouldBe` Just strippedParams
      lookupPath ["contents", "0", "parts", "0", "functionCall", "args"] body `shouldBe` Just args

    it "strips bounds from the object responseSchema" $ do
      let schema = object ["type" .= ("integer" :: Text), "maximum" .= (3 :: Int)]
          req =
            ChatRequest
              { reqModel = "gemini-2.5-flash",
                reqConversation = [],
                reqSystem = Nothing,
                reqMaxTokens = 100,
                reqTemperature = Nothing,
                reqTools = [],
                reqThinking = Nothing
              }
          body = (geminiProvider "").buildObjectBody req schema
      lookupPath ["generationConfig", "responseSchema"] body
        `shouldBe` Just (object ["type" .= ("integer" :: Text)])

  describe "structured output with thinking" $ do
    it "decodes the object when a thought summary part comes first" $ do
      let resp =
            object
              [ "candidates"
                  .= [ object
                         [ "content"
                             .= object
                               [ "role" .= ("model" :: Text),
                                 "parts"
                                   .= [ object ["text" .= ("Let me think about the answer." :: Text), "thought" .= True],
                                        object ["text" .= ("{\"answer\": " :: Text)],
                                        object ["text" .= ("42}" :: Text)]
                                      ]
                               ]
                         ]
                     ]
              ]
      result <- (geminiProvider "").parseObjectResponse resp
      fmap fst result `shouldBe` Right (object ["answer" .= (42 :: Int)])

    it "does not request thought summaries for object requests" $ do
      let req =
            ChatRequest
              { reqModel = "gemini-2.5-flash",
                reqConversation = [],
                reqSystem = Nothing,
                reqMaxTokens = 100,
                reqTemperature = Nothing,
                reqTools = [],
                reqThinking = Just ThinkingMode {tmEnabled = True, tmEffort = Just "1024"}
              }
          body = (geminiProvider "").buildObjectBody req (object ["type" .= ("object" :: Text)])
      lookupPath ["generationConfig", "thinkingConfig"] body
        `shouldBe` Just (object ["thinkingBudget" .= (1024 :: Int)])

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
    it "replays signature only for a matching model, else sends the placeholder" $ do
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
      thoughtSignatures matching `shouldBe` ["sig-xyz"]
      thoughtSignatures mismatch `shouldBe` [skipThoughtSignatureValidator]
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
      -- The Claude signature must not leak; the call gets the placeholder.
      thoughtSignatures encoded `shouldBe` [skipThoughtSignatureValidator]

    it "sends the placeholder signature on every call without metadata" $ do
      let turn =
            AssistantMessage
              [ toolCallPart (mkToolCall "c1" "get_weather" (object [])),
                toolCallPart (mkToolCall "c2" "get_time" (object []))
              ]
          encoded = head (encodeTurn "gemini-3.1-flash-lite" turn)
      thoughtSignatures encoded `shouldBe` [skipThoughtSignatureValidator, skipThoughtSignatureValidator]

    it "drops an assistant turn whose content encodes to nothing" $ do
      -- A Claude turn holding only redacted thinking: the foreign opaque is
      -- stripped and there is no text, so "parts": [] would be a 400.
      let redacted =
            ThinkingContent
              { thinkingText = Nothing,
                thinkingOpaque =
                  Just
                    ProviderOpaque
                      { poProvider = "claude",
                        poModel = Just "claude-haiku-4-5",
                        poPayload = object ["type" .= ("redacted_thinking" :: Text), "data" .= ("xyz" :: Text)]
                      }
              }
          turn = AssistantMessage [thinkingPart redacted, textPart ""]
      encodeTurn "gemini-3.1-flash-lite" turn `shouldBe` []

    it "drops a user turn whose content encodes to nothing" $ do
      let turn = UserMessage [thinkingPart (ThinkingContent (Just "stray") Nothing)]
      encodeTurn "gemini-3.1-flash-lite" turn `shouldBe` []

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

  describe "cache hint encoding" $ do
    it "omits cache hints from the wire while keeping content identical" $ do
      Right b64 <- pure $ imageBase64Part "image/jpeg" "aGVsbG8="
      let plain =
            UserMessage
              [ imageUrlPart "https://example.com/photo.png",
                b64,
                textPart "caption"
              ]
          hinted =
            UserMessage
              [ cacheEphemeral (imageUrlPart "https://example.com/photo.png"),
                b64,
                cacheEphemeral (textPart "caption")
              ]
          plainMsg = head (encodeTurn "gemini-3.1-flash-lite" plain)
          hintedMsg = head (encodeTurn "gemini-3.1-flash-lite" hinted)
      hintedMsg `shouldBe` plainMsg

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

-- | The @thoughtSignature@ values of an encoded turn's parts, in order.
thoughtSignatures :: Value -> [Text]
thoughtSignatures v =
  [sig | Object p <- assistantParts v, Just (String sig) <- [KM.lookup "thoughtSignature" p]]

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

-- | Follow object keys / array indices (as decimal strings).
lookupPath :: [Text] -> Value -> Maybe Value
lookupPath [] v = Just v
lookupPath (k : ks) (Object o) = KM.lookup (K.fromText k) o >>= lookupPath ks
lookupPath (k : ks) (Array a) = case reads (T.unpack k) of
  [(i, "")] -> a V.!? i >>= lookupPath ks
  _ -> Nothing
lookupPath _ _ = Nothing
