module LLM.DeepSeekSpec (spec) where

import Data.Aeson (Value (Object, String), object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import LLM.Core.Types
  ( ChatRequest (..),
    ChatResponse (respReasoning, respText),
    ThinkingMode (..),
    ToolCall (..),
    Turn (..),
    deepSeekMessageEncodeOptions,
    defaultMessageEncodeOptions,
  )
import LLM.Providers.DeepSeek (deepSeekBuildBodyPairs)
import LLM.Providers.OpenAI (encodeTurn, parseOpenAIResponse)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "DeepSeek thinking mode" $ do
  describe "message encoding" $ do
    it "includes reasoning_content when replaying assistant tool turns" $ do
      let turn =
            AssistantTurn
              "Let me check."
              (Just "I should call the weather tool.")
              [ToolCall "call_1" "get_weather" (object ["location" .= ("London" :: Text)])]
          msg = head (encodeTurn deepSeekMessageEncodeOptions turn)
      lookupText "reasoning_content" msg `shouldBe` Just "I should call the weather tool."

    it "omits reasoning_content for OpenAI-compatible default encoding" $ do
      let turn = AssistantTurn "Hello" (Just "thinking") []
          msg = head (encodeTurn defaultMessageEncodeOptions turn)
      lookupText "reasoning_content" msg `shouldBe` Nothing

  describe "request body" $ do
    it "enables thinking by default" $ do
      let body = object (deepSeekBuildBodyPairs False sampleRequest)
      nestedText ["thinking", "type"] body `shouldBe` Just "enabled"

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
        Left err -> fail $ show err

sampleRequest :: ChatRequest
sampleRequest =
  ChatRequest
    { reqModel = "deepseek-v4-pro",
      reqConversation =
        [ AssistantTurn "Hi" (Just "CoT") [ToolCall "c1" "get_date" (object [])],
          ToolTurn []
        ],
      reqSystem = Nothing,
      reqMaxTokens = 1024,
      reqTemperature = Nothing,
      reqTools = [],
      reqThinking = Nothing
    }

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
