module LLM.GenericConversationTest (createSpec, GenericConversationTextOps (..)) where

import Data.Text qualified as T
import Heptapod (generate)
import LLM (LLMHooks (..))
import LLM.Agent.Events (noEventObserver)
import LLM.Agent.ToolUtils (toTool)
import LLM.Agent.Types (Agent (..), RuntimeArgs (..))
import LLM.Core.LLMProvider (LLMProvider, toGateway)
import LLM.Core.Usage (PricingInfo (..))
import LLM.Generate.Logger (noHooks)
import LLM.Generate.ModelConfig (ModelConfig (..), ModelWithFallbacks (ModelWithFallbacks))
import LLM.TestKit
  ( loadRecordedConversation,
    mockProvider,
    streamChatLoop,
  )
import LLM.Tools.Weather (weatherToolTyped)
import Test.Hspec (Spec, describe, it, shouldBe)

data GenericConversationTextOps = GenericConversationTextOps
  { specTitle :: String,
    specProvider :: LLMProvider,
    modelName :: String,
    filePathGenerated :: String,
    filePathStreamed :: String
  }

createSpec :: GenericConversationTextOps -> Spec
createSpec opts = describe opts.specTitle $ do
  it "generateText" $ do
    (m, p) <- loadRecordedConversation opts.filePathGenerated
    uuid1 <- generate
    let provider = toGateway $ mockProvider m opts.specProvider
        modelConf =
          ModelConfig
            { mcGateway = provider,
              mcModel = T.pack opts.modelName,
              mcPricing = PricingInfo {pricePerMillionInput = 0.0, pricePerMillionOutput = 0.0},
              mcMaxTokens = 1024,
              mcTemperature = Nothing,
              mcThinking = Nothing,
              mcRequestTimeout = Nothing,
              mcThrottleDelay = Nothing,
              mcRetryCount = 3,
              mcJitterBackoff = 1_000
            }
        systemPrompt = "You are a helpful assistant who answers questions and executes tools for the user. Always use tools when asked to, but use only the tools that are available."
        agent :: Agent =
          Agent
            { agName = "test",
              agSystemPrompt = Just systemPrompt,
              agTools =
                [ toTool weatherToolTyped
                ],
              agUTools = [],
              agMaxToolRounds = 3,
              agContextWindow = Nothing
            }
        models = ModelWithFallbacks modelConf []
        rt =
          RuntimeArgs
            { rtGenerationId = uuid1,
              rtAbortSignal = Nothing,
              rtLLMHooks = LLMHooks {onLLMRequest = \_ _ -> pure (), onLLMResponse = \_ _ -> pure (), onLLMResponseError = \_ _ -> pure ()},
              rtHooks = noHooks,
              rtOnEvent = noEventObserver,
              rtReadonly = False
            }
    turns <- streamChatLoop False (agent, models, rt) p
    length turns `shouldBe` 8

  it "streamText" $ do
    (m, p) <- loadRecordedConversation opts.filePathStreamed
    uuid1 <- generate
    let provider = toGateway $ mockProvider m opts.specProvider
        modelConf =
          ModelConfig
            { mcGateway = provider,
              mcModel = T.pack opts.modelName,
              mcPricing = PricingInfo {pricePerMillionInput = 0.0, pricePerMillionOutput = 0.0},
              mcMaxTokens = 1024,
              mcTemperature = Nothing,
              mcThinking = Nothing,
              mcRequestTimeout = Nothing,
              mcThrottleDelay = Nothing,
              mcRetryCount = 3,
              mcJitterBackoff = 1_000
            }
        systemPrompt = "You are a helpful assistant who answers questions and executes tools for the user. Always use tools when asked to, but use only the tools that are available."
        models = ModelWithFallbacks modelConf []
        agent =
          Agent
            { agName = "test",
              agSystemPrompt = Just systemPrompt,
              agTools = [toTool weatherToolTyped],
              agUTools = [],
              agMaxToolRounds = 3,
              agContextWindow = Nothing
            }
        runtime =
          RuntimeArgs
            { rtGenerationId = uuid1,
              rtAbortSignal = Nothing,
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
    turns <- streamChatLoop True (agent, models, runtime) p
    length turns `shouldBe` 8
