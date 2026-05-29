module UTool2 (uTool2) where

import Data.Aeson
  ( KeyValue ((.=)),
    Value,
    object,
    withObject,
    (.:),
  )
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import LLM (ModelWithFallbacks)
import LLM.Agent.Generate4
  ( AgentCommand (RunWorkflowCommand),
    AgentNodeInput (..),
    DialogSpec (..),
    MergePolicy (MergeConcat),
    ToolOutcome (ToolCommand),
    UTool (..),
    Workflow (..),
    WorkflowDialog (..),
    WorkflowInput (WInputFromPrior, WInputText),
  )
import LLM.Agent.Types (Agent, RuntimeArgs)
import LLM.Core.Types
  ( ToolDef (..),
  )

uTool2 :: (Agent, Agent, Agent, Agent, ModelWithFallbacks, RuntimeArgs) -> UTool
uTool2 (aAgent, bAgent, cAgent, dAgent, models, runtime) =
  UTool
    { utToolDef =
        ToolDef
          { toolName = "subagent",
            toolDescription = "subagent with file system access",
            toolReadonly = False,
            toolParameters = uTool2Schema
          },
      utToolExecute = const (getUTool2 (aAgent, bAgent, cAgent, dAgent, models, runtime))
    }

uTool2Schema :: Value
uTool2Schema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "prompt"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("prompt for the subagent" :: Text)
                ]
          ],
      "required" .= (["prompt"] :: [Text])
    ]

getUTool2 :: (Agent, Agent, Agent, Agent, ModelWithFallbacks, RuntimeArgs) -> Value -> IO ToolOutcome
getUTool2 (aAgent, bAgent, cAgent, dAgent, models, runtime) args = do
  let prompt = fromMaybe "unknown" $ parseMaybe parsePrompt args
      -- NOTE: RunAgent nodes currently resolve to "run-agent" in workflowNodeId.
      -- This is a limitation in the orchestration engine when multiple RunAgent nodes coexist.
      aNodeId = "run-agent"
  pure $
    ToolCommand $
      RunWorkflowCommand $
        Par
          [ Seq
              [ RunAgent
                  AgentNodeInput
                    { aniAgent = aAgent,
                      aniModels = models,
                      aniRt = runtime,
                      aniInput = WInputText ("[A] Start the workflow and produce concise plan.\n\nUser request:\n" <> prompt)
                    },
                RunAgent
                  AgentNodeInput
                    { aniAgent = bAgent,
                      aniModels = models,
                      aniRt = runtime,
                      aniInput = WInputFromPrior aNodeId
                    }
              ],
            Dialog
              WorkflowDialog
                { wdSpec =
                    DialogSpec
                      { dsAgentA = cAgent,
                        dsAgentB = dAgent,
                        dsModelsA = models,
                        dsModelsB = models,
                        dsRt = runtime,
                        dsTopic = "[C/D dialog] Review and refine this request: " <> prompt,
                        dsSeedTurns = [],
                        dsMaxRounds = 4,
                        dsSummarizer = Nothing
                      },
                  wdNodeId = "node-dialog-cd"
                }
          ]
          MergeConcat

parsePrompt :: Value -> Parser Text
parsePrompt = withObject "args" (.: "prompt")
