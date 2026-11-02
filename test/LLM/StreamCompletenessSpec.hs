{-# OPTIONS_GHC -Wno-missing-fields #-}

-- | Truncated or corrupt streams must fail with a typed error (and so reach
-- a fallback model) instead of returning a partial answer or throwing.
module LLM.StreamCompletenessSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (object)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.LLMProvider (LLMProvider (..), toGateway)
import LLM.Core.Types (ChatResponse (..), LLMError (..), LLMGateway, LLMTextResult)
import LLM.Core.Usage (defaultPricingInfo)
import LLM.Generate.Generate (streamTextWithFallbacks)
import LLM.Generate.GenerateUtils (llmHooks)
import LLM.Generate.Logger (noHooks)
import LLM.Generate.ModelConfig (ModelConfig (..), ModelWithFallbacks (..), defaultModelCapabilities)
import LLM.Generate.Types (GenRequest (..))
import LLM.Providers.Claude (claudeProvider)
import LLM.Providers.Gemini (geminiProvider)
import LLM.Providers.OpenAI (openAIProvider)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

-- | The wire shape of one streaming protocol.
data Protocol = Protocol
  { name :: String,
    provider :: LLMProvider,
    -- | Events sent before any text.
    start :: ByteString,
    -- | One event carrying a text delta.
    text :: ByteString -> ByteString,
    -- | Events sent after the text, except the end-of-stream marker.
    tailEvents :: ByteString,
    -- | The end-of-stream marker event(s).
    end :: ByteString,
    -- | How the marker is named in the error.
    markerName :: Text
  }

protocols :: [Protocol]
protocols = [openAI, gemini, claude]

openAI :: Protocol
openAI =
  Protocol
    { name = "OpenAI-compatible",
      provider = openAIProvider "key",
      start = "",
      text = \t -> "data: {\"choices\":[{\"index\":0,\"delta\":{\"content\":\"" <> t <> "\"},\"finish_reason\":null}]}\n\n",
      tailEvents = "data: {\"choices\":[],\"usage\":{\"prompt_tokens\":3,\"completion_tokens\":2,\"total_tokens\":5}}\n\n",
      end = "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}\n\ndata: [DONE]\n\n",
      markerName = "[DONE] or finish_reason"
    }

gemini :: Protocol
gemini =
  Protocol
    { name = "Gemini",
      provider = geminiProvider "key",
      start = "",
      text = \t -> "data: {\"candidates\":[{\"content\":{\"role\":\"model\",\"parts\":[{\"text\":\"" <> t <> "\"}]}}]}\n\n",
      tailEvents = "data: {\"usageMetadata\":{\"promptTokenCount\":3,\"candidatesTokenCount\":2}}\n\n",
      end = "data: {\"candidates\":[{\"finishReason\":\"STOP\"}]}\n\n",
      markerName = "finishReason"
    }

claude :: Protocol
claude =
  Protocol
    { name = "Claude",
      provider = claudeProvider "key",
      start =
        "event: message_start\ndata: {\"type\":\"message_start\",\"message\":{\"model\":\"claude-test\",\"usage\":{\"input_tokens\":3,\"output_tokens\":0}}}\n\n"
          <> "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n",
      text = \t -> "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"" <> t <> "\"}}\n\n",
      tailEvents =
        "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
          <> "event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":2}}\n\n",
      end = "event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n",
      markerName = "message_stop"
    }

-- | Text, then the tail and end of a well-formed stream.
complete :: Protocol -> ByteString -> ByteString
complete p t = p.start <> p.text t <> p.tailEvents <> p.end

-- | Feed @bytes@ to the protocol's stream parser in small chunks, so event
-- boundaries fall in the middle of reads.
parseBytes :: Protocol -> ByteString -> IO LLMTextResult
parseBytes p bytes = do
  reader <- chunkedReader 7 bytes
  p.provider.parseStream (object []) reader (\_ -> pure ())

chunkedReader :: Int -> ByteString -> IO (IO ByteString)
chunkedReader n bytes = do
  ref <- newIORef bytes
  pure $ do
    rest <- readIORef ref
    let (chunk, rest') = BS.splitAt n rest
    writeIORef ref rest'
    pure chunk

-- | The protocol's provider, answering every streaming call with @bytes@.
gatewayFor :: Protocol -> ByteString -> ModelConfig
gatewayFor p bytes =
  testModelConfig $
    toGateway
      p.provider
        { openStream = \_ k -> k =<< chunkedReader 7 bytes
        }

-- | Malformed streams, each paired with a check on the parser's result.
badStreams :: Protocol -> [(String, ByteString, LLMTextResult -> Bool)]
badStreams p =
  [ ( "partial text, then EOF mid-event",
      p.start <> p.text "Hel" <> BS.take 30 (p.text "lo"),
      isTruncated
    ),
    ( "every event except the end-of-stream marker",
      p.start <> p.text "Hello" <> p.tailEvents,
      isTruncated
    ),
    ( "a malformed JSON event after valid text",
      p.start <> p.text "Hel" <> malformedEvent <> p.text "lo" <> p.tailEvents <> p.end,
      isParseError "not valid JSON"
    ),
    ( "invalid UTF-8 in a data line",
      p.start <> p.text "Hel" <> p.text "\xff\xfe" <> p.tailEvents <> p.end,
      isParseError "UTF-8"
    )
  ]
  where
    isTruncated = \case
      Left (NetworkError msg) -> msg == "stream ended before " <> p.markerName
      _ -> False
    isParseError s = \case
      Left (ParseError msg) -> s `T.isInfixOf` msg
      _ -> False
    malformedEvent
      | p.name == "Claude" = "event: content_block_delta\ndata: {\"type\":\n\n"
      | otherwise = "data: {\"candidates\":\n\n"

spec :: Spec
spec = describe "Stream completeness" $ do
  forM_ protocols $ \p -> describe p.name $ do
    it "accepts a complete stream" $ do
      r <- parseBytes p (complete p "Hello")
      fmap (.respText) r `shouldBe` Right "Hello"

    forM_ (badStreams p) $ \(label, bytes, check) -> do
      it ("fails on " <> label) $ do
        r <- parseBytes p bytes
        if check r
          then pure ()
          else expectationFailure ("unexpected result: " <> show r)

      it ("falls back to the next model on " <> label) $ do
        let models = ModelWithFallbacks (gatewayFor p bytes) [gatewayFor p (complete p "Fallback")]
        r <- streamTextWithFallbacks (\_ -> pure ()) testGenRequest models
        either (Left . show) (Right . (.respText)) r `shouldBe` Right "Fallback"

  describe "OpenAI-compatible end markers" $ do
    it "accepts [DONE] without a finish_reason" $ do
      r <- parseBytes openAI (openAI.text "Hello" <> "data: [DONE]\n\n")
      fmap (.respText) r `shouldBe` Right "Hello"

    it "accepts a finish_reason without [DONE]" $ do
      r <- parseBytes openAI (openAI.text "Hello" <> "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}\n\n")
      fmap (.respText) r `shouldBe` Right "Hello"

  describe "Gemini end markers" $ do
    it "accepts a blocked prompt, which has a blockReason and no finishReason" $ do
      r <- parseBytes gemini "data: {\"promptFeedback\":{\"blockReason\":\"SAFETY\"}}\n\n"
      case r of
        Left (ResponseBlocked _) -> pure ()
        other -> expectationFailure ("expected ResponseBlocked, got " <> show other)

  describe "precedence" $ do
    it "reports an in-band provider error rather than the missing marker" $ do
      r <- parseBytes claude (claude.start <> claude.text "Hel" <> "event: error\ndata: {\"type\":\"error\",\"error\":{\"type\":\"overloaded_error\",\"message\":\"Overloaded\"}}\n\n")
      case r of
        Left (HttpError 529 _) -> pure ()
        other -> expectationFailure ("expected HttpError 529, got " <> show other)

testGenRequest :: GenRequest
testGenRequest =
  GenRequest
    { grSystemPrompt = Nothing,
      grMessages = [],
      grTools = [],
      grAbortSignal = Nothing,
      grLLMHooks = llmHooks noHooks,
      grHooks = noHooks
    }

testModelConfig :: LLMGateway -> ModelConfig
testModelConfig gw =
  ModelConfig
    { mcGateway = gw,
      mcModel = "test",
      mcPricing = defaultPricingInfo 0 0,
      mcMaxTokens = 256,
      mcTemperature = Nothing,
      mcThinking = Nothing,
      mcCapabilities = defaultModelCapabilities,
      mcRequestTimeout = Nothing,
      mcThrottleDelay = Nothing,
      mcRetryCount = 0,
      mcJitterBackoff = 0
    }
