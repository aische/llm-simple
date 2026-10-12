module LLM.ToolUtilsSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Data.Aeson (Value (Null), object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Heptapod (generate)
import LLM.Agent.Events (emitEvent)
import LLM.Agent.ToolUtils (executeTool)
import LLM.Agent.Types (EventObserver, GenerateEventDetail (..), RuntimeArgs (..), Tool (..), ToolContext (..))
import LLM.Core.Types (ToolCall (..), ToolDef (..), ToolResult (..))
import LLM.Generate.GenerateUtils (llmHooks)
import LLM.Generate.Logger (Hooks (..), LogLevel (..), noHooks, safeHooks)
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain)

spec :: Spec
spec = describe "ToolUtils" $ do
  describe "executeTool" $ do
    it "refuses mutating tools when rtReadonly is True" $ do
      genId <- generate
      let writeTool =
            Tool
              { toolDef =
                  ToolDef
                    { toolName = "writefile",
                      toolDescription = "test",
                      toolReadonly = False,
                      toolParameters = Null
                    },
                toolExecute = \_ _ -> pure ("wrote" :: Text)
              }
          readonlyTool =
            Tool
              { toolDef =
                  ToolDef
                    { toolName = "grep",
                      toolDescription = "test",
                      toolReadonly = True,
                      toolParameters = Null
                    },
                toolExecute = \_ _ -> pure ("found" :: Text)
              }
          rt =
            RuntimeArgs
              { rtGenerationId = genId,
                rtAbortSignal = Nothing,
                rtLLMHooks = llmHooks noHooks,
                rtHooks = noHooks,
                rtOnEvent = \_ -> pure (),
                rtReadonly = True
              }
          ctx =
            ToolContext
              { tcConversation = [],
                tcUsage = mempty,
                tcWindowOffset = 0,
                tcRuntimeArgs = rt
              }
          writeCall =
            ToolCall
              { tcId = "1",
                tcName = "writefile",
                tcArguments = object ["path" .= ("x" :: Text), "content" .= ("y" :: Text)],
                tcProviderMeta = Nothing
              }
          readCall =
            ToolCall
              { tcId = "2",
                tcName = "grep",
                tcArguments = object ["pattern" .= ("x" :: Text)],
                tcProviderMeta = Nothing
              }
      writeResult <- executeTool noHooks ctx [writeTool] writeCall
      readResult <- executeTool noHooks ctx [readonlyTool] readCall
      T.unpack writeResult.trContent `shouldContain` "readonly mode"
      T.unpack readResult.trContent `shouldContain` "found"

  describe "async exceptions" $ do
    it "executeTool does not turn a timeout into a tool result" $ do
      ctx <- mkContext (\_ -> pure ())
      let slow = mkTool "slow" (threadDelay 1_000_000 >> pure "done")
      r <- timeout 50_000 (executeTool noHooks ctx [slow] (mkCall "slow"))
      fmap (.trContent) r `shouldBe` Nothing

    it "executeTool still reports synchronous exceptions as tool errors" $ do
      ctx <- mkContext (\_ -> pure ())
      let failing = mkTool "boom" (throwIO (userError "kaput"))
      r <- executeTool noHooks ctx [failing] (mkCall "boom")
      T.unpack r.trContent `shouldContain` "Tool error"

    it "emitEvent does not swallow a timeout in the observer" $ do
      ctx <- mkContext (\_ -> threadDelay 1_000_000)
      r <- timeout 50_000 (emitEvent ctx.tcRuntimeArgs GenerationStarted)
      r `shouldBe` Nothing

    it "safeHooks does not swallow a timeout in a hook" $ do
      let hooks = safeHooks noHooks {onLog = \_ _ -> threadDelay 1_000_000}
      r <- timeout 50_000 (hooks.onLog Info "x")
      r `shouldBe` Nothing

mkTool :: Text -> IO Text -> Tool Text
mkTool name act =
  Tool
    { toolDef =
        ToolDef
          { toolName = name,
            toolDescription = "test",
            toolReadonly = True,
            toolParameters = Null
          },
      toolExecute = \_ _ -> act
    }

mkCall :: Text -> ToolCall
mkCall name =
  ToolCall
    { tcId = "1",
      tcName = name,
      tcArguments = object [],
      tcProviderMeta = Nothing
    }

mkContext :: EventObserver -> IO ToolContext
mkContext onEvent = do
  genId <- generate
  pure
    ToolContext
      { tcConversation = [],
        tcUsage = mempty,
        tcWindowOffset = 0,
        tcRuntimeArgs =
          RuntimeArgs
            { rtGenerationId = genId,
              rtAbortSignal = Nothing,
              rtLLMHooks = llmHooks noHooks,
              rtHooks = noHooks,
              rtOnEvent = onEvent,
              rtReadonly = False
            }
      }
