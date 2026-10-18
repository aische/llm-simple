module LLM.DeepSeekSpec (spec) where

import Data.Aeson (Value (Array, Object, String), eitherDecodeFileStrict', object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Vector qualified as V
import LLM.Core.Types
  ( ChatRequest (..),
    ChatResponse (respContent, respReasoning, respText),
    ContentPart (..),
    PartBody (..),
    ThinkingContent (..),
    ThinkingMode (..),
    Turn (..),
    assistantTurn,
    cacheEphemeral,
    deepSeekMessageEncodeOptions,
    defaultMessageEncodeOptions,
    mkToolCall,
    textPart,
    toolCallPart,
    pattern UserTurn,
  )
import LLM.Core.Usage (Usage (..))
import LLM.Providers.DeepSeek (deepSeekBuildBodyPairs)
import LLM.Providers.OpenAI (encodeTurn, parseOpenAIResponse, parseOpenAIUsage)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

spec :: Spec
spec = describe "DeepSeek thinking mode" $ do
  describe "message encoding" $ do
    it "includes reasoning_content when replaying assistant tool turns" $ do
      let turn =
            assistantTurn
              "Let me check."
              (Just "I should call the weather tool.")
              [mkToolCall "call_1" "get_weather" (object ["location" .= ("London" :: Text)])]
          msg = head (encodeTurn deepSeekMessageEncodeOptions turn)
      lookupText "reasoning_content" msg `shouldBe` Just "I should call the weather tool."

    it "replays the reasoning of every thinking part, not just the first" $ do
      let turn =
            AssistantMessage
              [ ContentPart (ThinkingPart (ThinkingContent (Just "first, ") Nothing)) Nothing,
                ContentPart (ThinkingPart (ThinkingContent (Just "second") Nothing)) Nothing,
                textPart "Done."
              ]
          msg = head (encodeTurn deepSeekMessageEncodeOptions turn)
      lookupText "reasoning_content" msg `shouldBe` Just "first, second"

    it "omits reasoning_content for OpenAI-compatible default encoding" $ do
      let turn = assistantTurn "Hello" (Just "thinking") []
          msg = head (encodeTurn defaultMessageEncodeOptions turn)
      lookupText "reasoning_content" msg `shouldBe` Nothing

    it "omits cache hints from the wire while keeping content identical" $ do
      let plain = UserMessage [textPart "static context"]
          hinted = UserMessage [cacheEphemeral (textPart "static context")]
          plainMsg = head (encodeTurn deepSeekMessageEncodeOptions plain)
          hintedMsg = head (encodeTurn deepSeekMessageEncodeOptions hinted)
      hintedMsg `shouldBe` plainMsg
      lookupText "content" hintedMsg `shouldBe` Just "static context"

  describe "request body" $ do
    it "disables thinking by default" $ do
      let body = object (deepSeekBuildBodyPairs False sampleRequest)
      nestedText ["thinking", "type"] body `shouldBe` Just "disabled"

    it "supports explicit thinking configuration" $ do
      let req =
            sampleRequest
              { reqThinking = Just ThinkingMode {tmEnabled = True, tmEffort = Just "max"}
              }
          body = object (deepSeekBuildBodyPairs False req)
      nestedText ["thinking", "type"] body `shouldBe` Just "enabled"
      lookupText "reasoning_effort" body `shouldBe` Just "max"

    it "can disable thinking explicitly" $ do
      let req =
            sampleRequest
              { reqThinking = Just ThinkingMode {tmEnabled = False, tmEffort = Nothing}
              }
          body = object (deepSeekBuildBodyPairs False req)
      nestedText ["thinking", "type"] body `shouldBe` Just "disabled"

  describe "reasoning_content on assistant turns without thinking" $ do
    let foreignToolTurn = AssistantMessage [toolCallPart (mkToolCall "c1" "get_date" (object []))]
        withThinking enabled conv =
          sampleRequest
            { reqConversation = conv,
              reqThinking = Just ThinkingMode {tmEnabled = enabled, tmEffort = Nothing}
            }
        assistantMsgs r =
          [m | m@(Object o) <- bodyMessages (object (deepSeekBuildBodyPairs False r)), KM.lookup "role" o == Just (String "assistant")]

    it "sends an empty reasoning_content when thinking is enabled" $ do
      let msgs = assistantMsgs (withThinking True [UserTurn "date?", foreignToolTurn])
      map (lookupText "reasoning_content") msgs `shouldBe` [Just ""]

    it "omits reasoning_content when thinking is disabled" $ do
      let msgs = assistantMsgs (withThinking False [UserTurn "date?", foreignToolTurn])
      map (lookupText "reasoning_content") msgs `shouldBe` [Nothing]
      let msgsDefault = assistantMsgs sampleRequest {reqConversation = [UserTurn "date?", foreignToolTurn]}
      map (lookupText "reasoning_content") msgsDefault `shouldBe` [Nothing]

    it "sends an existing thinking text unchanged" $ do
      let turn = assistantTurn "" (Just "CoT") [mkToolCall "c1" "get_date" (object [])]
          msgs = assistantMsgs (withThinking True [UserTurn "date?", turn])
      map (lookupText "reasoning_content") msgs `shouldBe` [Just "CoT"]

  describe "response parsing" $ do
    it "extracts reasoning_content from provider responses" $ do
      let response =
            object
              [ "choices"
                  .= [ object
                         [ "message"
                             .= object
                               [ "role" .= ("assistant" :: Text),
                                 "reasoning_content" .= ("Let me think." :: Text),
                                 "content" .= ("The answer is 42." :: Text)
                               ]
                         ]
                     ]
              ]
      case parseOpenAIResponse response of
        Right resp -> do
          resp.respReasoning `shouldBe` Just "Let me think."
          resp.respText `shouldBe` "The answer is 42."
          case resp.respContent of
            [ ContentPart (ThinkingPart (ThinkingContent (Just "Let me think.") Nothing)) Nothing,
              ContentPart (TextPart "The answer is 42.") Nothing
              ] -> pure ()
            other -> fail $ "unexpected ordered parts: " <> show other
        Left err -> fail $ show err

  describe "parseOpenAIUsage (DeepSeek cache fields)" $ do
    it "maps prompt_cache_hit_tokens without double-counting prompt_tokens" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/deepseek-conversation-generated.json"
      case val of
        Array arr ->
          case V.toList arr of
            (Object first : _) ->
              case KM.lookup "response" first of
                Just resp ->
                  parseOpenAIUsage resp
                    `shouldBe` Just
                      Usage
                        { usageInputTokens = 336,
                          usageOutputTokens = 68,
                          usageCacheReadTokens = 256,
                          usageCacheCreationTokens = 0,
                          usageTotalCost = 0
                        }
                _ -> expectationFailure "missing response"
            _ -> expectationFailure "empty conversation array"
        _ -> expectationFailure "expected conversation array"

sampleRequest :: ChatRequest
sampleRequest =
  ChatRequest
    { reqModel = "deepseek-v4-pro",
      reqConversation =
        [ assistantTurn "Hi" (Just "CoT") [mkToolCall "c1" "get_date" (object [])],
          ToolTurn []
        ],
      reqSystem = Nothing,
      reqMaxTokens = 1024,
      reqTemperature = Nothing,
      reqTools = [],
      reqThinking = Nothing
    }

bodyMessages :: Value -> [Value]
bodyMessages (Object o)
  | Just (Array arr) <- KM.lookup "messages" o = V.toList arr
bodyMessages _ = []

lookupText :: Text -> Value -> Maybe Text
lookupText key (Object o) =
  KM.lookup (K.fromText key) o >>= \case
    String t -> Just t
    _ -> Nothing
lookupText _ _ = Nothing

nestedText :: [Text] -> Value -> Maybe Text
nestedText [key] v = lookupText key v
nestedText (key : rest) (Object o) =
  KM.lookup (K.fromText key) o >>= nestedText rest
nestedText _ _ = Nothing
