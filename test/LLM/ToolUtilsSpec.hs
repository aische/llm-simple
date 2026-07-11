module LLM.ToolUtilsSpec (spec) where

import Data.Aeson (Value (Null), object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Heptapod (generate)
import LLM.Agent.ToolUtils (executeTool)
import LLM.Agent.Types (RuntimeArgs (..), Tool (..), ToolContext (..))
import LLM.Core.Types (ToolCall (..), ToolDef (..), ToolResult (..))
import LLM.Generate.GenerateUtils (llmHooks)
import LLM.Generate.Logger (noHooks)
import Test.Hspec (Spec, describe, it, shouldContain)

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
