{-# OPTIONS_GHC -Wno-missing-fields #-}

module LLM.ChatSpec (spec) where

import Data.Aeson (object, (.=))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map qualified as Map
import Data.Text (Text)
import Heptapod (generate)
import LLM.Agent.Events (noEventObserver)
import LLM.Agent.Generate (generateText, streamText)
import LLM.Agent.Types
  ( Agent (..),
    RuntimeArgs (..),
    Tool (..),
    ToolMap,
  )
import LLM.Core.Abort (AbortSignal, abort, newAbortSignal)
import LLM.Core.Types
  ( CacheHint (..),
    ChatRequest (..),
    ChatResponse (..),
    ContentPart (..),
    PartBody (..),
    textPart, toolCallPart, pattern UserTurn,
    Turn (..),
    cacheEphemeral,
    commonImageMediaTypes,
    imageBase64Part,
    imageUrlPart,
    supportedImageMediaTypes,
    LLMError (..),
    LLMGateway (..),
    LLMHooks (..),
    ToolDef (ToolDef, toolDescription, toolName, toolParameters, toolReadonly),
    ToolResult (..),
    mkToolCall,
  )
import LLM.Core.Usage (PricingInfo (..), defaultPricingInfo, mkUsage)
import LLM.Generate.Logger (noHooks)
import LLM.Generate.ModelConfig
  ( ModelCapabilities (..),
    ModelConfig (..),
    ModelWithFallbacks (..),
    defaultModelCapabilities,
  )
import LLM.Generate.Types
  ( GenerateError (..),
    GenerateErrorResult (..),
    GenerateTextResult (..),
  )
import Test.Hspec
  ( Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
    shouldSatisfy,
  )

-- | A mock gateway that returns a fixed response
mockGateway :: ChatResponse -> LLMGateway
mockGateway resp =
  LLMGateway
    { gwName = "mock",
      gwImageMediaTypes = supportedImageMediaTypes,
      gwImageUrlSupport = True,
      gwGenerateText = \_ _ -> pure (Right resp),
      gwStreamText = \_ _ _ -> pure (Right resp),
      gwGenerateObject = \_ _ _ -> pure (Right (object [], Nothing))
    }

-- | A mock gateway that returns an error
mockErrorGateway :: LLMError -> LLMGateway
mockErrorGateway err =
  LLMGateway
    { gwName = "mock-error",
      gwImageMediaTypes = supportedImageMediaTypes,
      gwImageUrlSupport = True,
      gwGenerateText = \_ _ -> pure (Left err),
      gwStreamText = \_ _ _ -> pure (Left err),
      gwGenerateObject = \_ _ _ -> pure (Left err)
    }

-- | A mock gateway that calls a tool, then responds with text
mockToolGateway :: LLMGateway
mockToolGateway =
  LLMGateway
    { gwName = "mock-tool",
      gwImageMediaTypes = supportedImageMediaTypes,
      gwImageUrlSupport = True,
      gwGenerateText = \_ req ->
        if any isToolTurn req.reqConversation
          then pure $ Right (ChatResponse "The weather is sunny." [textPart "The weather is sunny."] (Just (mkUsage 80 15)) Nothing)
          else
            let tc = mkToolCall "call_1" "get_weather" (object ["location" .= ("London" :: Text)])
             in pure $ Right (ChatResponse "" [toolCallPart tc] (Just (mkUsage 50 10)) Nothing),
      gwStreamText = \_ _ _ -> pure $ Right (ChatResponse "" [] Nothing Nothing),
      gwGenerateObject = \_ _ _ -> pure $ Right (object [], Nothing)
    }
  where
    isToolTurn (ToolTurn _) = True
    isToolTurn _ = False

zeroPricing :: PricingInfo
zeroPricing = defaultPricingInfo 0 0

-- | Wrap a gateway in a ModelConfig with test defaults
mockModel :: LLMGateway -> ModelConfig
mockModel gw =
  ModelConfig
    { mcGateway = gw,
      mcModel = "test-model",
      mcPricing = zeroPricing,
      mcMaxTokens = 1024,
      mcTemperature = Nothing,
      mcThinking = Nothing,
      mcCapabilities = defaultModelCapabilities,
      mcRequestTimeout = Nothing,
      mcThrottleDelay = Nothing,
      mcRetryCount = 0,
      mcJitterBackoff = 0
    }

weatherTool :: Tool Text
weatherTool =
  Tool
    { toolDef =
        ToolDef
          { toolName = "get_weather",
            toolDescription = "Get weather",
            toolReadonly = True,
            toolParameters = object ["type" .= ("object" :: Text)]
          },
      toolExecute = \_ _ -> pure "Sunny, 22°C"
    }

defaultAgent :: Agent
defaultAgent =
  Agent
    { agName = "test",
      agSystemPrompt = Nothing,
      agTools = [],
      agMaxToolRounds = 10,
      agContextWindow = Nothing
    }

mkRuntime :: Maybe AbortSignal -> IO RuntimeArgs
mkRuntime mSig = do
  uuid <- generate
  pure
    RuntimeArgs
      { rtGenerationId = uuid,
        rtAbortSignal = mSig,
        rtLLMHooks =
          LLMHooks
            { onLLMRequest = \_ _ -> pure (),
              onLLMResponse = \_ _ -> pure (),
              onLLMResponseError = \_ _ -> pure ()
            },
        rtHooks = noHooks,
        rtOnEvent = noEventObserver,
        rtReadonly = False
      }

runGenerate ::
  Agent ->
  ModelWithFallbacks ->
  ToolMap Text ->
  Maybe AbortSignal ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
runGenerate agent models toolMap mSig turns = do
  rt <- mkRuntime mSig
  generateText agent models toolMap rt turns

runStreamGenerate ::
  Agent ->
  ModelWithFallbacks ->
  ToolMap Text ->
  Maybe AbortSignal ->
  [Turn] ->
  IO (Either GenerateErrorResult GenerateTextResult)
runStreamGenerate agent models toolMap mSig turns = do
  rt <- mkRuntime mSig
  streamText (\_ -> pure ()) agent models toolMap rt turns

-- | Gateway that records each request conversation, then behaves like 'mockToolGateway'.
capturingToolGateway :: IORef [[Turn]] -> LLMGateway
capturingToolGateway ref =
  let respond req = do
        modifyIORef' ref (req.reqConversation :)
        if any isToolTurn req.reqConversation
          then pure $ Right (ChatResponse "The weather is sunny." [textPart "The weather is sunny."] (Just (mkUsage 80 15)) Nothing)
          else
            let tc = mkToolCall "call_1" "get_weather" (object ["location" .= ("London" :: Text)])
             in pure $ Right (ChatResponse "" [toolCallPart tc] (Just (mkUsage 50 10)) Nothing)
   in LLMGateway
        { gwName = "mock-tool-capture",
          gwImageMediaTypes = supportedImageMediaTypes,
          gwImageUrlSupport = True,
          gwGenerateText = \_ -> respond,
          gwStreamText = \_ req _onEvent -> respond req,
          gwGenerateObject = \_ _ _ -> pure $ Right (object [], Nothing)
        }
  where
    isToolTurn (ToolTurn _) = True
    isToolTurn _ = False

-- | Gateway that appends its label to @calls@ on every request, then answers
-- with @respond@ (given the request).
loggingGateway :: IORef [Text] -> Text -> (ChatRequest -> IO (Either LLMError ChatResponse)) -> LLMGateway
loggingGateway calls label respond =
  let call req = modifyIORef' calls (++ [label]) >> respond req
   in LLMGateway
        { gwName = label,
          gwImageMediaTypes = supportedImageMediaTypes,
          gwImageUrlSupport = True,
          gwGenerateText = \_ -> call,
          gwStreamText = \_ req _onEvent -> call req,
          gwGenerateObject = \_ _ _ -> pure $ Right (object [], Nothing)
        }

-- | Tool call when the conversation has no tool turn yet, text afterwards
-- (like 'mockToolGateway').
toolThenText :: ChatRequest -> IO (Either LLMError ChatResponse)
toolThenText req
  | any isToolTurn req.reqConversation =
      pure $ Right (ChatResponse "The weather is sunny." [textPart "The weather is sunny."] (Just (mkUsage 80 15)) Nothing)
  | otherwise =
      let tc = mkToolCall "call_1" "get_weather" (object ["location" .= ("London" :: Text)])
       in pure $ Right (ChatResponse "" [toolCallPart tc] (Just (mkUsage 50 10)) Nothing)
  where
    isToolTurn (ToolTurn _) = True
    isToolTurn _ = False

-- | Fail while the conversation has no tool turn, then behave like 'toolThenText'.
failFirstRound :: ChatRequest -> IO (Either LLMError ChatResponse)
failFirstRound req
  | any isToolTurn req.reqConversation = toolThenText req
  | otherwise = pure $ Left (HttpError 503 "down")
  where
    isToolTurn (ToolTurn _) = True
    isToolTurn _ = False

-- | Tool call in the first round, failure afterwards.
toolThenFail :: ChatRequest -> IO (Either LLMError ChatResponse)
toolThenFail req
  | any isToolTurn req.reqConversation = pure $ Left (HttpError 503 "down")
  | otherwise = toolThenText req
  where
    isToolTurn (ToolTurn _) = True
    isToolTurn _ = False

alwaysFail :: ChatRequest -> IO (Either LLMError ChatResponse)
alwaysFail _ = pure $ Left (HttpError 503 "down")

-- | Shared sticky-fallback checks, run for both 'runGenerate' and 'runStreamGenerate'.
stickyFallbackSpec ::
  (Agent -> ModelWithFallbacks -> ToolMap Text -> Maybe AbortSignal -> [Turn] -> IO (Either GenerateErrorResult GenerateTextResult)) ->
  ToolMap Text ->
  Spec
stickyFallbackSpec run toolMap = do
  let agent = defaultAgent {agTools = ["get_weather"]}
  it "keeps using the fallback that answered for the rest of the run" $ do
    calls <- newIORef []
    let primary = mockModel (loggingGateway calls "primary" alwaysFail)
        fallback = mockModel (loggingGateway calls "fallback" toolThenText)
    result <- run agent (ModelWithFallbacks primary [fallback]) toolMap Nothing [UserTurn "weather?"]
    case result of
      Right r -> r.gtrText `shouldBe` "The weather is sunny."
      Left err -> expectationFailure $ show err
    readIORef calls >>= (`shouldBe` ["primary", "fallback", "fallback"])

  it "tries the remaining models, the earlier-failed one last, when the sticky model fails" $ do
    calls <- newIORef []
    let a = mockModel (loggingGateway calls "a" failFirstRound)
        b = mockModel (loggingGateway calls "b" toolThenFail)
        c = mockModel (loggingGateway calls "c" alwaysFail)
    result <- run agent (ModelWithFallbacks a [b, c]) toolMap Nothing [UserTurn "weather?"]
    case result of
      Right r -> r.gtrText `shouldBe` "The weather is sunny."
      Left err -> expectationFailure $ show err
    readIORef calls >>= (`shouldBe` ["a", "b", "b", "c", "a"])

  it "starts from the caller's chain again on the next call" $ do
    calls <- newIORef []
    let primary = mockModel (loggingGateway calls "primary" alwaysFail)
        fallback = mockModel (loggingGateway calls "fallback" toolThenText)
        models = ModelWithFallbacks primary [fallback]
    _ <- run agent models toolMap Nothing [UserTurn "weather?"]
    _ <- run agent models toolMap Nothing [UserTurn "weather again?"]
    readIORef calls >>= (`shouldBe` ["primary", "fallback", "fallback", "primary", "fallback", "fallback"])

hasCachedUserPrefix :: [Turn] -> Bool
hasCachedUserPrefix =
  any
    ( \case
        UserMessage parts ->
          any
            ( \case
                ContentPart (TextPart _) (Just CacheEphemeral) -> True
                _ -> False
            )
            parts
        _ -> False
    )

spec :: Spec
spec = describe "Chat" $ do
  let toolMap = Map.fromList [("get_weather", weatherTool)]
  describe "generateText" $ do
    it "returns text for a simple response" $ do
      let gw = mockGateway (ChatResponse "Hi there!" [textPart "Hi there!"] (Just (mkUsage 10 5)) Nothing)
          models = ModelWithFallbacks (mockModel gw) []
      result <- runGenerate defaultAgent models toolMap Nothing [UserTurn "hello"]
      case result of
        Right r -> do
          r.gtrText `shouldBe` "Hi there!"
          length r.gtrNewMessages `shouldBe` 1 -- assistantTurn
          r.gtrUsage `shouldBe` mkUsage 10 5
        Left err -> expectationFailure $ show err

    it "propagates errors" $ do
      let gw = mockErrorGateway (HttpError 500 "internal error")
          models = ModelWithFallbacks (mockModel gw) []
      result <- runGenerate defaultAgent models toolMap Nothing [UserTurn "hello"]
      case result of
        Left GenerateErrorResult {gerError = GErrLLM (HttpError 500 _)} -> pure ()
        other -> expectationFailure $ "Expected HttpError 500, got: " <> show other

    it "handles tool call loop" $ do
      let agent = defaultAgent {agTools = ["get_weather"]}
          models = ModelWithFallbacks (mockModel mockToolGateway) []
      result <- runGenerate agent models toolMap Nothing [UserTurn "weather in london?"]
      case result of
        Right r -> do
          r.gtrText `shouldBe` "The weather is sunny."
          -- assistantTurn(tool call) + ToolTurn + assistantTurn(final)
          length r.gtrNewMessages `shouldBe` 3
          r.gtrUsage `shouldBe` mkUsage 130 25 -- 50+80 input, 10+15 output
        Left err -> expectationFailure $ show err

    it "preserves cache hints on user parts across non-streaming tool rounds" $ do
      ref <- newIORef []
      let agent = defaultAgent {agTools = ["get_weather"]}
          models = ModelWithFallbacks (mockModel (capturingToolGateway ref)) []
          msgs =
            [ UserMessage
                [ cacheEphemeral (textPart "static system-like context"),
                  textPart "weather in london?"
                ]
            ]
      result <- runGenerate agent models toolMap Nothing msgs
      case result of
        Right r -> do
          r.gtrText `shouldBe` "The weather is sunny."
          convs <- reverse <$> readIORef ref
          length convs `shouldBe` 2
          mapM_ (`shouldSatisfy` hasCachedUserPrefix) convs
        Left err -> expectationFailure $ show err

    it "respects maxToolRounds" $ do
      let infiniteToolGateway =
            LLMGateway
              { gwName = "mock-infinite",
                gwImageMediaTypes = supportedImageMediaTypes,
                gwImageUrlSupport = True,
                gwGenerateText = \_ _ ->
                  let tc = mkToolCall "call_1" "get_weather" (object [])
                   in pure $ Right (ChatResponse "" [toolCallPart tc] Nothing Nothing),
                gwStreamText = \_ _ _ -> pure $ Right (ChatResponse "" [] Nothing Nothing),
                gwGenerateObject = \_ _ _ -> pure $ Right (object [], Nothing)
              }
          agent = defaultAgent {agMaxToolRounds = 2, agTools = ["get_weather"]}
          models = ModelWithFallbacks (mockModel infiniteToolGateway) []
      result <- runGenerate agent models toolMap Nothing [UserTurn "test"]
      case result of
        Left GenerateErrorResult {gerError = GErrToolExceeded, gerPartialNewMessages = msgs} ->
          -- The last round's results are returned, though the model never saw them.
          case reverse msgs of
            ToolTurn [_] : AssistantMessage _ : _ -> length msgs `shouldBe` 4
            _ -> expectationFailure $ "Expected the last round's ToolTurn last, got: " <> show msgs
        other -> expectationFailure $ "Expected GErrToolExceeded, got: " <> show other

    it "falls back to next model on retryable error" $ do
      let failGw = mockErrorGateway (HttpError 503 "service unavailable")
          okGw = mockGateway (ChatResponse "Fallback worked!" [textPart "Fallback worked!"] (Just (mkUsage 10 5)) Nothing)
          models = ModelWithFallbacks (mockModel failGw) [mockModel okGw]
      result <- runGenerate defaultAgent models toolMap Nothing [UserTurn "hello"]
      case result of
        Right r -> r.gtrText `shouldBe` "Fallback worked!"
        Left err -> expectationFailure $ "Expected fallback success, got: " <> show err

    it "falls back on non-retryable error too" $ do
      let failGw = mockErrorGateway (HttpError 400 "bad request")
          okGw = mockGateway (ChatResponse "Fallback worked!" [textPart "Fallback worked!"] (Just (mkUsage 10 5)) Nothing)
          models = ModelWithFallbacks (mockModel failGw) [mockModel okGw]
      result <- runGenerate defaultAgent models toolMap Nothing [UserTurn "hello"]
      case result of
        Right r -> r.gtrText `shouldBe` "Fallback worked!"
        Left err -> expectationFailure $ "Expected fallback success, got: " <> show err

    it "skips a non-vision model and succeeds with a vision-capable fallback" $ do
      let boomGw =
            LLMGateway
              { gwName = "should-not-run",
                gwImageMediaTypes = supportedImageMediaTypes,
                gwImageUrlSupport = True,
                gwGenerateText = \_ _ -> pure (Left (HttpError 500 "should not be called")),
                gwStreamText = \_ _ _ -> pure (Left (HttpError 500 "should not be called")),
                gwGenerateObject = \_ _ _ -> pure (Left (HttpError 500 "should not be called"))
              }
          okGw = mockGateway (ChatResponse "I see a cat." [textPart "I see a cat."] (Just (mkUsage 10 5)) Nothing)
          noVision = mockModel boomGw
          withVision =
            (mockModel okGw)
              { mcCapabilities = defaultModelCapabilities {capVision = True}
              }
          models = ModelWithFallbacks noVision [withVision]
          msgs = [UserMessage [imageUrlPart "https://example.com/cat.png", textPart "what is this?"]]
      result <- runGenerate defaultAgent models toolMap Nothing msgs
      case result of
        Right r -> r.gtrText `shouldBe` "I see a cat."
        Left err -> expectationFailure $ "Expected vision fallback success, got: " <> show err

    it "returns UnsupportedCapability when no vision-capable model remains" $ do
      let noVision1 = mockModel (mockErrorGateway (HttpError 500 "unused"))
          noVision2 = mockModel (mockErrorGateway (HttpError 500 "unused"))
          models = ModelWithFallbacks noVision1 [noVision2]
          msgs = [UserMessage [imageUrlPart "https://example.com/cat.png", textPart "what?"]]
      result <- runGenerate defaultAgent models toolMap Nothing msgs
      case result of
        Left GenerateErrorResult {gerError = GErrLLM (UnsupportedCapability _)} -> pure ()
        other -> expectationFailure $ "Expected UnsupportedCapability, got: " <> show other

    it "skips a model whose gateway does not accept the image media type" $ do
      calls <- newIORef []
      Right heic <- pure $ imageBase64Part "image/heic" "aGVsbG8="
      let vision = defaultModelCapabilities {capVision = True}
          claudeLike =
            (mockModel ((loggingGateway calls "claude-like" toolThenText) {gwImageMediaTypes = commonImageMediaTypes}))
              { mcCapabilities = vision
              }
          geminiLike =
            (mockModel ((loggingGateway calls "gemini-like" toolThenText) {gwImageMediaTypes = supportedImageMediaTypes}))
              { mcCapabilities = vision
              }
          msgs = [UserMessage [heic, textPart "what is this?"]]
      result <- runGenerate defaultAgent (ModelWithFallbacks claudeLike [geminiLike]) toolMap Nothing msgs
      case result of
        Right r -> r.gtrText `shouldBe` "The weather is sunny."
        Left err -> expectationFailure $ "Expected media-type fallback success, got: " <> show err
      readIORef calls >>= (`shouldBe` ["gemini-like", "gemini-like"])

    it "returns UnsupportedCapability when no model accepts the image media type" $ do
      calls <- newIORef []
      Right heic <- pure $ imageBase64Part "image/heic" "aGVsbG8="
      let claudeLike name =
            (mockModel ((loggingGateway calls name toolThenText) {gwImageMediaTypes = commonImageMediaTypes}))
              { mcCapabilities = defaultModelCapabilities {capVision = True}
              }
          msgs = [UserMessage [heic, textPart "what is this?"]]
      result <- runGenerate defaultAgent (ModelWithFallbacks (claudeLike "a") [claudeLike "b"]) toolMap Nothing msgs
      case result of
        Left GenerateErrorResult {gerError = GErrLLM (UnsupportedCapability _)} -> pure ()
        other -> expectationFailure $ "Expected UnsupportedCapability, got: " <> show other
      readIORef calls >>= (`shouldBe` [])

    it "skips a model whose gateway does not accept image URLs" $ do
      calls <- newIORef []
      let vision = defaultModelCapabilities {capVision = True}
          ollamaLike =
            (mockModel ((loggingGateway calls "ollama-like" toolThenText) {gwImageUrlSupport = False}))
              { mcCapabilities = vision
              }
          urlOk = (mockModel (loggingGateway calls "url-ok" toolThenText)) {mcCapabilities = vision}
          msgs = [UserMessage [imageUrlPart "https://example.com/cat.png", textPart "what is this?"]]
      result <- runGenerate defaultAgent (ModelWithFallbacks ollamaLike [urlOk]) toolMap Nothing msgs
      case result of
        Right r -> r.gtrText `shouldBe` "The weather is sunny."
        Left err -> expectationFailure $ "Expected URL-image fallback success, got: " <> show err
      readIORef calls >>= (`shouldBe` ["url-ok", "url-ok"])

    it "accepts base64 images on a gateway without image URL support" $ do
      calls <- newIORef []
      Right png <- pure $ imageBase64Part "image/png" "aGVsbG8="
      let ollamaLike =
            (mockModel ((loggingGateway calls "ollama-like" toolThenText) {gwImageUrlSupport = False}))
              { mcCapabilities = defaultModelCapabilities {capVision = True}
              }
          msgs = [UserMessage [png, textPart "what is this?"]]
      result <- runGenerate defaultAgent (ModelWithFallbacks ollamaLike []) toolMap Nothing msgs
      case result of
        Right r -> r.gtrText `shouldBe` "The weather is sunny."
        Left err -> expectationFailure $ show err

    it "returns error from last model when all fail" $ do
      let failGw1 = mockErrorGateway (HttpError 503 "service unavailable")
          failGw2 = mockErrorGateway (HttpError 400 "bad request")
          models = ModelWithFallbacks (mockModel failGw1) [mockModel failGw2]
      result <- runGenerate defaultAgent models toolMap Nothing [UserTurn "hello"]
      case result of
        Left GenerateErrorResult {gerError = GErrLLM (HttpError 400 _)} -> pure ()
        other -> expectationFailure $ "Expected HttpError 400 from last model, got: " <> show other

    it "returns Aborted when signal is fired before the call" $ do
      let gw = mockGateway (ChatResponse "Hi!" [textPart "Hi!"] Nothing Nothing)
          models = ModelWithFallbacks (mockModel gw) []
      sig <- newAbortSignal
      abort sig
      result <- runGenerate defaultAgent models toolMap (Just sig) [UserTurn "hello"]
      case result of
        Left GenerateErrorResult {gerError = GErrAborted} -> pure ()
        other -> expectationFailure $ "Expected GErrAborted, got: " <> show other

    it "returns Aborted during tool execution" $ do
      sig <- newAbortSignal
      let slowTool =
            Tool
              { toolDef =
                  ToolDef
                    { toolName = "slow",
                      toolDescription = "A slow tool",
                      toolReadonly = True,
                      toolParameters = object ["type" .= ("object" :: Text)]
                    },
                toolExecute = \_ _ -> do
                  abort sig
                  pure "done"
              }
          twoCallGw =
            LLMGateway
              { gwName = "mock-two",
                gwImageMediaTypes = supportedImageMediaTypes,
                gwImageUrlSupport = True,
                gwGenerateText = \_ _ ->
                  let tc1 = mkToolCall "c1" "slow" (object [])
                      tc2 = mkToolCall "c2" "slow" (object [])
                   in pure $ Right (ChatResponse "" [toolCallPart tc1, toolCallPart tc2] Nothing Nothing),
                gwStreamText = \_ _ _ -> pure $ Right (ChatResponse "" [] Nothing Nothing),
                gwGenerateObject = \_ _ _ -> pure $ Right (object [], Nothing)
              }
          tm = Map.fromList [("slow", slowTool)]
          agent = defaultAgent {agTools = ["slow"]}
          models = ModelWithFallbacks (mockModel twoCallGw) []
      result <- runGenerate agent models tm (Just sig) [UserTurn "go"]
      case result of
        Left GenerateErrorResult {gerError = GErrAborted, gerPartialNewMessages} ->
          -- Every tool call in the history must have a result, or the next
          -- request is rejected. c1 ran; c2 never did.
          case reverse gerPartialNewMessages of
            ToolTurn results : _ ->
              [(r.trCallId, r.trContent) | r <- results]
                `shouldBe` [("c1", "done"), ("c2", "Aborted before execution")]
            _ -> expectationFailure $ "Expected history to end with a ToolTurn, got: " <> show gerPartialNewMessages
        other -> expectationFailure $ "Expected GErrAborted during tools, got: " <> show other

    describe "sticky fallback" $ stickyFallbackSpec runGenerate toolMap

  describe "streamText" $ do
    describe "sticky fallback" $ stickyFallbackSpec runStreamGenerate toolMap

    it "preserves cache hints on user parts across streaming tool rounds" $ do
      ref <- newIORef []
      let agent = defaultAgent {agTools = ["get_weather"]}
          models = ModelWithFallbacks (mockModel (capturingToolGateway ref)) []
          msgs =
            [ UserMessage
                [ cacheEphemeral (textPart "static system-like context"),
                  textPart "weather in london?"
                ]
            ]
      result <- runStreamGenerate agent models toolMap Nothing msgs
      case result of
        Right r -> do
          r.gtrText `shouldBe` "The weather is sunny."
          convs <- reverse <$> readIORef ref
          length convs `shouldBe` 2
          mapM_ (`shouldSatisfy` hasCachedUserPrefix) convs
        Left err -> expectationFailure $ show err
