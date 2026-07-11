module LLM.GenericConversationTest (createSpec, GenericConversationTextOps (..)) where

import Data.Map qualified as Map
import Data.Text qualified as T
import Heptapod (generate)
import LLM (LLMHooks (..))
import LLM.Agent.Events (noEventObserver)
import LLM.Agent.ToolUtils (toTool)
import LLM.Agent.Types (Agent (..), RuntimeArgs (..))
import LLM.Core.LLMProvider (LLMProvider, toGateway)
import LLM.Core.Types (LLMGateway, ThinkingMode (..))
import LLM.Core.Usage (PricingInfo (..))
import LLM.Generate.Logger (noHooks)
import LLM.Generate.ModelConfig (ModelConfig (..), ModelWithFallbacks (ModelWithFallbacks))
import LLM.TestKit
  ( loadRecordedConversation,
    mockProvider,
    recordedConversationSystemPrompt,
    streamChatLoop,
  )
import LLM.WeatherTool (weatherToolTyped)
import Test.Hspec (Spec, describe, it, shouldBe)

data GenericConversationTextOps = GenericConversationTextOps
  { specTitle :: String,
    specProvider :: LLMProvider,
    modelName :: String,
    specThinking :: Maybe ThinkingMode,
    filePathGenerated :: String,
    filePathStreamed :: String
  }

mkModelConfig :: GenericConversationTextOps -> LLMGateway -> ModelConfig
mkModelConfig opts provider =
  ModelConfig
    { mcGateway = provider,
      mcModel = T.pack opts.modelName,
      mcPricing = PricingInfo {pricePerMillionInput = 0.0, pricePerMillionOutput = 0.0},
      mcMaxTokens = 1024,
      mcTemperature = Nothing,
      mcThinking = opts.specThinking,
      mcRequestTimeout = Nothing,
      mcThrottleDelay = Nothing,
      mcRetryCount = 3,
      mcJitterBackoff = 1_000
    }

createSpec :: GenericConversationTextOps -> Spec
createSpec opts = describe opts.specTitle $ do
  let toolMap = Map.fromList [("get_weather", toTool weatherToolTyped)]

  it "generateText" $ do
    (m, p) <- loadRecordedConversation opts.filePathGenerated
    uuid1 <- generate
    let provider = toGateway $ mockProvider m opts.specProvider
        modelConf = mkModelConfig opts provider
        systemPrompt = recordedConversationSystemPrompt
        agent :: Agent =
          Agent
            { agName = "test",
              agSystemPrompt = Just systemPrompt,
              agTools =
                [ "get_weather"
                ],
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
    turns <- streamChatLoop False (agent, models, toolMap, rt) p
    length turns `shouldBe` 8

  it "streamText" $ do
    (m, p) <- loadRecordedConversation opts.filePathStreamed
    uuid1 <- generate
    let provider = toGateway $ mockProvider m opts.specProvider
        modelConf = mkModelConfig opts provider
        systemPrompt = recordedConversationSystemPrompt
        models = ModelWithFallbacks modelConf []
        agent =
          Agent
            { agName = "test",
              agSystemPrompt = Just systemPrompt,
              agTools = ["get_weather"],
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
    turns <- streamChatLoop True (agent, models, toolMap, runtime) p
    length turns `shouldBe` 8
