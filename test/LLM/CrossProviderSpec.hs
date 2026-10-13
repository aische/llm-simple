-- | Mixed-provider history regression test.
--
-- Fallback happens per model call, so one agent run can contain assistant
-- turns from several providers. This feeds one realistic mixed history
-- through every provider's 'buildBody' and checks the rules the live APIs
-- enforce (see .notes/fallback-fix-plan.md, items 2–4).
module LLM.CrossProviderSpec (spec) where

import Data.Aeson (Value (Array, Object, String), encode, object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector qualified as V
import LLM.Core.LLMProvider (LLMProvider (..))
import LLM.Core.Types
  ( ChatRequest (..),
    ProviderOpaque (..),
    ThinkingContent (..),
    ThinkingMode (..),
    ToolCall (..),
    ToolResult (..),
    Turn (..),
    imageBase64Part,
    mkToolCall,
    textPart,
    thinkingPart,
    toolCallPart,
    pattern UserTurn,
  )
import LLM.Providers.Claude (claudeProvider)
import LLM.Providers.DeepSeek (deepSeekProvider)
import LLM.Providers.Gemini (geminiProvider)
import LLM.Providers.Ollama (ollamaProvider)
import LLM.Providers.OpenAI (openAIProvider)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

-- Fake opaque payloads; none may reach another provider's request.

claudeSig, claudeRedacted, geminiThoughtSig, geminiCallSig :: Text
claudeSig = "FAKE_CLAUDE_THINKING_SIGNATURE"
claudeRedacted = "FAKE_CLAUDE_REDACTED_DATA"
geminiThoughtSig = "FAKE_GEMINI_THOUGHT_SIGNATURE"
geminiCallSig = "FAKE_GEMINI_CALL_SIGNATURE"

-- | One run: Claude → Gemini → OpenAI → Claude, then the next user turn.
-- Each assistant turn has the shape that provider's parser produces.
mixedHistory :: [Turn]
mixedHistory =
  [ -- 1. user text + base64 png
    UserMessage [textPart "What's in this picture, and what's the weather?", png],
    -- 2. Claude: thinking + text + tool call
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
                            "thinking" .= ("claude thoughts" :: Text),
                            "signature" .= claudeSig
                          ]
                    }
            },
        textPart "Let me check the weather.",
        toolCallPart (mkToolCall "toolu_01AbCdEf" "get_weather" (object ["location" .= ("London" :: Text)]))
      ],
    -- 3.
    ToolTurn [ToolResult "toolu_01AbCdEf" "get_weather" "Sunny, 22°C"],
    -- 4. Gemini: thought part + function call with Gemini metadata
    --    (the Gemini parser uses the function name as the call id).
    AssistantMessage
      [ thinkingPart
          ThinkingContent
            { thinkingText = Just "gemini thoughts",
              thinkingOpaque =
                Just
                  ProviderOpaque
                    { poProvider = "gemini",
                      poModel = Just "gemini-2.5-flash",
                      poPayload =
                        object
                          [ "text" .= ("gemini thoughts" :: Text),
                            "thought" .= True,
                            "thoughtSignature" .= geminiThoughtSig
                          ]
                    }
            },
        toolCallPart
          (mkToolCall "get_weather" "get_weather" (object ["location" .= ("Paris" :: Text)]))
            { tcProviderMeta =
                Just
                  ProviderOpaque
                    { poProvider = "gemini",
                      poModel = Just "gemini-2.5-flash",
                      poPayload = object ["thoughtSignature" .= geminiCallSig]
                    }
            }
      ],
    -- 5.
    ToolTurn [ToolResult "get_weather" "get_weather" "Rainy, 14°C"],
    -- 6. OpenAI: tool call only
    AssistantMessage [toolCallPart (mkToolCall "call_aBc123" "get_time" (object []))],
    -- 7.
    ToolTurn [ToolResult "call_aBc123" "get_time" "12:00"],
    -- 8. Claude: only redacted thinking
    AssistantMessage
      [ thinkingPart
          ThinkingContent
            { thinkingText = Nothing,
              thinkingOpaque =
                Just
                  ProviderOpaque
                    { poProvider = "claude",
                      poModel = Just "claude-haiku-4-5-20251001",
                      poPayload =
                        object
                          [ "type" .= ("redacted_thinking" :: Text),
                            "data" .= claudeRedacted
                          ]
                    }
            }
      ],
    UserTurn "Thanks. Anything else?"
  ]
  where
    png = either (error . show) id (imageBase64Part "image/png" "iVBORw0KGgo=")

mkReq :: Text -> Maybe ThinkingMode -> ChatRequest
mkReq model thinking =
  ChatRequest
    { reqModel = model,
      reqConversation = mixedHistory,
      reqSystem = Just "You are helpful.",
      reqMaxTokens = 4096,
      reqTemperature = Nothing,
      reqTools = [],
      reqThinking = thinking
    }

thinkingOn, thinkingOff :: Maybe ThinkingMode
thinkingOn = Just ThinkingMode {tmEnabled = True, tmEffort = Nothing}
thinkingOff = Just ThinkingMode {tmEnabled = False, tmEffort = Nothing}

claudeBody, geminiBody, openAIBody, ollamaBody, deepSeekThinkingBody, deepSeekPlainBody :: Value
claudeBody = (claudeProvider "").buildBody False (mkReq "claude-haiku-4-5" Nothing)
geminiBody = (geminiProvider "").buildBody False (mkReq "gemini-3.1-flash-lite" Nothing)
openAIBody = (openAIProvider "").buildBody False (mkReq "gpt-4.1" Nothing)
ollamaBody = ollamaProvider.buildBody False (mkReq "qwen2.5vl" Nothing)
deepSeekThinkingBody = (deepSeekProvider "").buildBody False (mkReq "deepseek-v4-flash" thinkingOn)
deepSeekPlainBody = (deepSeekProvider "").buildBody False (mkReq "deepseek-v4-flash" thinkingOff)

openAIProtocolBodies :: [(String, Value)]
openAIProtocolBodies =
  [ ("OpenAI", openAIBody),
    ("Ollama", ollamaBody),
    ("DeepSeek (thinking)", deepSeekThinkingBody),
    ("DeepSeek (no thinking)", deepSeekPlainBody)
  ]

spec :: Spec
spec = describe "Cross-provider history" $ do
  describe "Gemini" $ do
    it "sends a thoughtSignature on every functionCall part" $ do
      let parts = concatMap (arrayAt "parts") (arrayAt "contents" geminiBody)
          calls = filter (isJust . key "functionCall") parts
      length calls `shouldBe` 3
      mapM_ (\p -> key "thoughtSignature" p `shouldSatisfy` isJust) calls

  describe "DeepSeek with thinking" $ do
    it "sends reasoning_content on every assistant message" $ do
      let msgs = assistantMessages deepSeekThinkingBody
      length msgs `shouldBe` 4
      mapM_ (\m -> key "reasoning_content" m `shouldSatisfy` isJust) msgs

  describe "OpenAI-protocol providers" $
    mapM_
      ( \(name, body) ->
          it (name <> ": every assistant message has content or tool_calls") $ do
            let msgs = assistantMessages body
            length msgs `shouldBe` 4
            mapM_ (\m -> m `shouldSatisfy` \v -> isJust (key "content" v) || isJust (key "tool_calls" v)) msgs
      )
      openAIProtocolBodies

  describe "Claude" $ do
    it "sends only Claude thinking blocks" $ do
      let blocks = concatMap (arrayAt "content") (assistantMessages' "messages" claudeBody)
          thinking = filter (\b -> textAt "type" b `elem` [Just "thinking", Just "redacted_thinking"]) blocks
      map (\b -> (textAt "type" b, textAt "signature" b, textAt "data" b)) thinking
        `shouldBe` [ (Just "thinking", Just claudeSig, Nothing),
                     (Just "redacted_thinking", Nothing, Just claudeRedacted)
                   ]

    it "pairs every tool_use with a tool_result in the next message" $ do
      let msgs = arrayAt "messages" claudeBody
          blockIds ty field m = [i | b <- arrayAt "content" m, textAt "type" b == Just ty, Just i <- [textAt field b]]
          pairs = zip msgs (drop 1 msgs)
          checked =
            [ (useIds, blockIds "tool_result" "tool_use_id" next)
              | (m, next) <- pairs,
                textAt "role" m == Just "assistant",
                let useIds = blockIds "tool_use" "id" m,
                not (null useIds)
            ]
      length checked `shouldBe` 3
      mapM_ (\(useIds, resultIds) -> resultIds `shouldBe` useIds) checked

  describe "opaque payloads" $ do
    let claudeSecrets = [claudeSig, claudeRedacted]
        geminiSecrets = [geminiThoughtSig, geminiCallSig]
        others = [("Claude", claudeBody), ("Gemini", geminiBody)] ++ openAIProtocolBodies
    mapM_
      ( \(name, body) ->
          it (name <> ": contains no other provider's opaque payload") $ do
            let foreignSecrets = case name of
                  "Claude" -> geminiSecrets
                  "Gemini" -> claudeSecrets
                  _ -> claudeSecrets ++ geminiSecrets
                leaked = filter (`containedIn` body) foreignSecrets
            leaked `shouldBe` []
      )
      others

    it "Claude still replays its own signatures (sanity check of the search)" $
      if all (`containedIn` claudeBody) claudeSecrets
        then pure ()
        else expectationFailure "Claude signatures missing from Claude request"

-- JSON helpers

containedIn :: Text -> Value -> Bool
containedIn needle v = encodeUtf8 needle `BS.isInfixOf` BSL.toStrict (encode v)

key :: Text -> Value -> Maybe Value
key k (Object o) = KM.lookup (K.fromText k) o
key _ _ = Nothing

textAt :: Text -> Value -> Maybe Text
textAt k v = case key k v of
  Just (String t) -> Just t
  _ -> Nothing

arrayAt :: Text -> Value -> [Value]
arrayAt k v = case key k v of
  Just (Array a) -> V.toList a
  _ -> []

assistantMessages :: Value -> [Value]
assistantMessages = assistantMessages' "messages"

assistantMessages' :: Text -> Value -> [Value]
assistantMessages' field body =
  filter (\m -> textAt "role" m == Just "assistant") (arrayAt field body)
