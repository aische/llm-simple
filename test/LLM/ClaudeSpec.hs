{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module LLM.ClaudeSpec (spec) where

import Data.Aeson
  ( Value (Array, Number, Object, String),
    eitherDecodeFileStrict',
    object,
    (.=),
  )
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (mapMaybe)
import Data.Scientific (toBoundedInteger, toRealFloat)
import Data.Text (Text)
import Data.Vector qualified as V
import LLM.Core.Types
  ( ChatRequest (..),
    ChatResponse (respContent, respReasoning, respText),
    ContentPart (..),
    PartBody (..),
    ProviderOpaque (..),
    ThinkingContent (..),
    ThinkingMode (..),
    ToolCall (..),
    ToolResult (..),
    Turn (..),
    mkToolCall,
    textPart,
    thinkingPart,
    toolCallPart,
    imageUrlPart,
    imageBase64Part,
    cacheEphemeral,
    pattern UserTurn,
  )
import LLM.Core.Usage (Usage (..), mkUsage)
import LLM.Core.Utils (getToolCalls, hasToolCalls)
import LLM.Providers.Claude
  ( claudeBuildBody,
    encodeTurn,
    effortToBudgetTokens,
    parseClaudeResponse,
    parseClaudeUsage,
  )
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
          resp.respText `shouldBe` "Hello! How can I help you today?"
          hasToolCalls resp `shouldBe` False
        Left err -> expectationFailure $ "Parse failed: " <> show err

    it "parses a tool_use response" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/claude-tool-use.json"
      case parseClaudeResponse val of
        Right resp -> do
          resp.respText `shouldBe` "Let me check the weather for you."
          hasToolCalls resp `shouldBe` True
          let [tc] = getToolCalls resp
          tc.tcName `shouldBe` "get_weather"
          tc.tcId `shouldBe` "toolu_01A09q90qw90lq917835lq9"
        Left err -> expectationFailure $ "Parse failed: " <> show err

    it "preserves thinking -> text -> tool_use order and opaque signature" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/claude-thinking-tool-use.json"
      case parseClaudeResponse val of
        Right resp -> do
          resp.respReasoning `shouldBe` Just "I should call the weather tool."
          resp.respText `shouldBe` "Checking the weather."
          case resp.respContent of
            [ ContentPart (ThinkingPart tc) Nothing,
              ContentPart (TextPart "Checking the weather.") Nothing,
              ContentPart (ToolCallPart tool) Nothing
              ] -> do
                tc.thinkingText `shouldBe` Just "I should call the weather tool."
                case tc.thinkingOpaque of
                  Just o -> do
                    o.poProvider `shouldBe` "claude"
                    o.poModel `shouldBe` Just "claude-haiku-4-5-20251001"
                    lookupText "signature" o.poPayload `shouldBe` Just "sig_thinking_abc123"
                  Nothing -> expectationFailure "expected thinking opaque"
                tool.tcId `shouldBe` "toolu_weather_1"
            other -> expectationFailure $ "unexpected parts: " <> show other
        Left err -> expectationFailure $ "Parse failed: " <> show err

  describe "encodeTurn replay" $ do
    it "replays signed thinking blocks before tool_use for the same model" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/claude-thinking-tool-use.json"
      case parseClaudeResponse val of
        Right resp -> do
          let encoded = encodeTurn "claude-haiku-4-5-20251001" (AssistantMessage resp.respContent)
              content = messageContent (head encoded)
          contentTypes content `shouldBe` ["thinking", "text", "tool_use"]
          case content of
            (Object o : _) ->
              lookupText "signature" (Object o) `shouldBe` Just "sig_thinking_abc123"
            _ -> expectationFailure "expected thinking object"
          let withResult =
                encodeTurn
                  "claude-haiku-4-5-20251001"
                  ( ToolTurn
                      [ ToolResult
                          { trCallId = "toolu_weather_1",
                            trName = "get_weather",
                            trContent = "sunny"
                          }
                      ]
                  )
          length withResult `shouldBe` 1
        Left err -> expectationFailure $ show err

    it "omits foreign thinking opaque and foreign tool meta on encode" $ do
      let foreignThinking =
            thinkingPart
              ThinkingContent
                { thinkingText = Just "gemini thoughts",
                  thinkingOpaque =
                    Just
                      ProviderOpaque
                        { poProvider = "gemini",
                          poModel = Just "gemini-2.5-flash",
                          poPayload = object ["thoughtSignature" .= ("sig" :: Text)]
                        }
                }
          foreignTool =
            let base = mkToolCall "c1" "get_weather" (object [])
             in toolCallPart
                  base
                    { tcProviderMeta =
                        Just
                          ProviderOpaque
                            { poProvider = "gemini",
                              poModel = Just "gemini-2.5-flash",
                              poPayload = object ["thoughtSignature" .= ("sig" :: Text)]
                            }
                    }
          turn =
            AssistantMessage
              [ foreignThinking,
                textPart "hello",
                foreignTool
              ]
          content = messageContent (head (encodeTurn "claude-haiku-4-5-20251001" turn))
      contentTypes content `shouldBe` ["text", "tool_use"]
      case content of
        [Object textO, Object toolO] -> do
          lookupText "text" (Object textO) `shouldBe` Just "hello"
          lookupText "name" (Object toolO) `shouldBe` Just "get_weather"
          KM.lookup "thoughtSignature" toolO `shouldBe` Nothing
        _ -> expectationFailure "expected text + tool_use only"

  describe "thinking request mapping" $ do
    it "maps effort to budget_tokens and omits temperature when thinking is on" $ do
      let req =
            ChatRequest
              { reqModel = "claude-haiku-4-5-20251001",
                reqConversation = [UserTurn "hi"],
                reqSystem = Nothing,
                reqMaxTokens = 4096,
                reqTemperature = Just 0.5,
                reqTools = [],
                reqThinking = Just ThinkingMode {tmEnabled = True, tmEffort = Just "high"}
              }
          body = claudeBuildBody False req
      nestedText ["thinking", "type"] body `shouldBe` Just "enabled"
      nestedInt ["thinking", "budget_tokens"] body `shouldBe` Just (effortToBudgetTokens "high")
      lookupKey "temperature" body `shouldBe` Nothing

    it "keeps temperature when thinking is off" $ do
      let req =
            ChatRequest
              { reqModel = "claude-haiku-4-5-20251001",
                reqConversation = [UserTurn "hi"],
                reqSystem = Nothing,
                reqMaxTokens = 4096,
                reqTemperature = Just 0.5,
                reqTools = [],
                reqThinking = Nothing
              }
          body = claudeBuildBody False req
      lookupNumber "temperature" body `shouldBe` Just 0.5

  describe "image request encoding" $ do
    it "encodes URL and base64 image parts" $ do
      Right b64 <- pure $ imageBase64Part "image/png" "aGVsbG8="
      let turn =
            UserMessage
              [ imageUrlPart "https://example.com/cat.jpg",
                b64,
                textPart "describe"
              ]
          content = messageContent (head (encodeTurn "claude-haiku-4-5-20251001" turn))
      contentTypes content `shouldBe` ["image", "image", "text"]
      case content of
        [Object urlO, Object b64O, Object textO] -> do
          nestedText ["source", "type"] (Object urlO) `shouldBe` Just "url"
          nestedText ["source", "url"] (Object urlO)
            `shouldBe` Just "https://example.com/cat.jpg"
          nestedText ["source", "type"] (Object b64O) `shouldBe` Just "base64"
          nestedText ["source", "media_type"] (Object b64O) `shouldBe` Just "image/png"
          nestedText ["source", "data"] (Object b64O) `shouldBe` Just "aGVsbG8="
          lookupText "text" (Object textO) `shouldBe` Just "describe"
        _ -> expectationFailure "expected image/image/text blocks"

  describe "cache_control breakpoints" $ do
    it "places cache_control on the marked block without reordering" $ do
      Right b64 <- pure $ imageBase64Part "image/png" "aGVsbG8="
      let turn =
            UserMessage
              [ imageUrlPart "https://example.com/cat.jpg",
                cacheEphemeral b64,
                cacheEphemeral (textPart "describe")
              ]
          content = messageContent (head (encodeTurn "claude-haiku-4-5-20251001" turn))
      contentTypes content `shouldBe` ["image", "image", "text"]
      case content of
        [Object urlO, Object b64O, Object textO] -> do
          KM.lookup "cache_control" urlO `shouldBe` Nothing
          nestedText ["cache_control", "type"] (Object b64O) `shouldBe` Just "ephemeral"
          nestedText ["cache_control", "type"] (Object textO) `shouldBe` Just "ephemeral"
          nestedText ["source", "data"] (Object b64O) `shouldBe` Just "aGVsbG8="
          lookupText "text" (Object textO) `shouldBe` Just "describe"
        _ -> expectationFailure "expected image/image/text blocks"

    it "uses a content-block array for a single cached text part" $ do
      let turn = UserMessage [cacheEphemeral (textPart "static context")]
          msg = head (encodeTurn "claude-haiku-4-5-20251001" turn)
      case lookupKey "content" msg of
        Just (Array arr) -> do
          V.length arr `shouldBe` 1
          nestedText ["cache_control", "type"] (V.head arr) `shouldBe` Just "ephemeral"
          lookupText "text" (V.head arr) `shouldBe` Just "static context"
        Just (String _) ->
          expectationFailure "cached single text must not encode as a bare string"
        _ -> expectationFailure "expected content array"

  describe "parseClaudeUsage" $ do
    it "extracts token counts" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/claude-text.json"
      parseClaudeUsage val `shouldBe` Just (mkUsage 25 10)

    it "extracts token counts from tool_use response" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/claude-tool-use.json"
      parseClaudeUsage val `shouldBe` Just (mkUsage 50 35)

    it "normalizes cache read/creation into total input without double-counting" $ do
      let val =
            object
              [ "usage"
                  .= object
                    [ "input_tokens" .= (50 :: Int),
                      "output_tokens" .= (20 :: Int),
                      "cache_read_input_tokens" .= (100_000 :: Int),
                      "cache_creation_input_tokens" .= (1_200 :: Int)
                    ]
              ]
      parseClaudeUsage val
        `shouldBe` Just
          Usage
            { usageInputTokens = 101_250,
              usageOutputTokens = 20,
              usageCacheReadTokens = 100_000,
              usageCacheCreationTokens = 1_200,
              usageTotalCost = 0
            }

    it "treats missing or zero cache fields as zero on recorded fixtures" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/claude-conversation-generated.json"
      case val of
        Array arr ->
          case V.toList arr of
            (Object first : _) ->
              case KM.lookup "response" first of
                Just resp ->
                  parseClaudeUsage resp
                    `shouldBe` Just (mkUsage 623 54)
                _ -> expectationFailure "missing response"
            _ -> expectationFailure "empty conversation array"
        _ -> expectationFailure "expected conversation array"

messageContent :: Value -> [Value]
messageContent (Object o) =
  case KM.lookup "content" o of
    Just (Array a) -> V.toList a
    _ -> []
messageContent _ = []

contentTypes :: [Value] -> [Text]
contentTypes = mapMaybe typ
  where
    typ (Object o) = case KM.lookup "type" o of
      Just (String t) -> Just t
      _ -> Nothing
    typ _ = Nothing

lookupText :: Text -> Value -> Maybe Text
lookupText key (Object o) =
  KM.lookup (K.fromText key) o >>= \case
    String t -> Just t
    _ -> Nothing
lookupText _ _ = Nothing

lookupNumber :: Text -> Value -> Maybe Double
lookupNumber key (Object o) =
  KM.lookup (K.fromText key) o >>= \case
    Number sci -> Just (toRealFloat sci)
    _ -> Nothing
lookupNumber _ _ = Nothing

lookupKey :: Text -> Value -> Maybe Value
lookupKey key (Object o) = KM.lookup (K.fromText key) o
lookupKey _ _ = Nothing

nestedText :: [Text] -> Value -> Maybe Text
nestedText [key] v = lookupText key v
nestedText (key : rest) (Object o) =
  KM.lookup (K.fromText key) o >>= nestedText rest
nestedText _ _ = Nothing

nestedInt :: [Text] -> Value -> Maybe Int
nestedInt [key] (Object o) =
  KM.lookup (K.fromText key) o >>= \case
    Number sci -> toBoundedInteger sci
    _ -> Nothing
nestedInt (key : rest) (Object o) =
  KM.lookup (K.fromText key) o >>= nestedInt rest
nestedInt _ _ = Nothing
