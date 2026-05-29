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
import LLM.Agent.Generate5
  ( AgentCommand (CmdRunWorkflow),
    AgentNodeInput (..),
    MergePolicy (MergeConcat),
    ToolOutcome (ToolCommand),
    UTool (..),
    Workflow (..),
    WorkflowInput (WInputFromPrior, WInputText),
  )
import LLM.Agent.Types (Agent, RuntimeArgs)
import LLM.Core.Types (ToolDef (..))

-- | UTool that returns a 'CmdRunWorkflow' command (runs on the same Generate5 stack).
uTool2 :: (Agent, Agent, Agent, ModelWithFallbacks, RuntimeArgs) -> UTool
uTool2 (planAgent, executeAgent, reviewAgent, models, runtime) =
  UTool
    { utToolDef =
        ToolDef
          { toolName = "run_workflow",
            toolDescription =
              "Run a multi-step workflow: sequential plan+execute, in parallel with an independent review branch",
            toolReadonly = False,
            toolParameters = uTool2Schema
          },
      utToolExec = const (getUTool2 (planAgent, executeAgent, reviewAgent, models, runtime))
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
                  "description" .= ("user request to process" :: Text)
                ]
          ],
      "required" .= (["prompt"] :: [Text])
    ]

getUTool2 ::
  (Agent, Agent, Agent, ModelWithFallbacks, RuntimeArgs) ->
  Value ->
  IO ToolOutcome
getUTool2 (planAgent, executeAgent, reviewAgent, models, runtime) args = do
  let prompt = fromMaybe "unknown" $ parseMaybe parsePrompt args
      nodePlan = "wf-plan"
      nodeExecute = "wf-execute"
      nodeReview = "wf-review"
  pure $
    ToolCommand $
      CmdRunWorkflow $
        WPar
          [ WSeq
              [ WRunAgent
                  AgentNodeInput
                    { aniAgent = planAgent,
                      aniModels = models,
                      aniRt = runtime,
                      aniInput =
                        WInputText
                          ( "[Plan] Produce a short plan for the user request.\n\nRequest:\n"
                              <> prompt
                          ),
                      aniNodeId = nodePlan
                    },
                WRunAgent
                  AgentNodeInput
                    { aniAgent = executeAgent,
                      aniModels = models,
                      aniRt = runtime,
                      aniInput = WInputFromPrior nodePlan,
                      aniNodeId = nodeExecute
                    }
              ],
            WRunAgent
              AgentNodeInput
                { aniAgent = reviewAgent,
                  aniModels = models,
                  aniRt = runtime,
                  aniInput =
                    WInputText
                      ( "[Review] Critically review this request in 2-3 sentences:\n" <> prompt
                      ),
                  aniNodeId = nodeReview
                }
          ]
          MergeConcat

parsePrompt :: Value -> Parser Text
parsePrompt = withObject "args" (.: "prompt")
