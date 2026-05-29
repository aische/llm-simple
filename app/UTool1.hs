module UTool1 (uTool1) where

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
  ( AgentCommand (CmdPushSubagent),
    SubagentSpec (..),
    ToolOutcome (ToolCommand),
    TranscriptPolicy (..),
    UTool (..),
  )
import LLM.Agent.Types (Agent, RuntimeArgs)
import LLM.Core.Types
  ( ToolDef (..),
    Turn (UserTurn),
  )

-- | UTool that pushes a child agent frame (Generate5 stack machine).
uTool1 :: (Agent, ModelWithFallbacks, RuntimeArgs) -> UTool
uTool1 (a, m, r) =
  UTool
    { utToolDef =
        ToolDef
          { toolName = "subagent",
            toolDescription = "Delegate a task to a subagent with filesystem tools",
            toolReadonly = False,
            toolParameters = uTool1Schema
          },
      utToolExec = const (getUTool1 (a, m, r))
    }

uTool1Schema :: Value
uTool1Schema =
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

getUTool1 :: (Agent, ModelWithFallbacks, RuntimeArgs) -> Value -> IO ToolOutcome
getUTool1 (a, m, r) args = do
  let prompt = fromMaybe "unknown" $ parseMaybe parsePrompt args
  pure $
    ToolCommand $
      CmdPushSubagent $
        SubagentSpec
          { ssAgent = a,
            ssModels = m,
            ssRtOverrides = Just r,
            ssInitialTurns = [UserTurn prompt],
            ssMaxToolRounds = Nothing,
            ssTranscriptPolicy = TranscriptIsolated,
            ssSummaryPrompt = Nothing
          }

parsePrompt :: Value -> Parser Text
parsePrompt = withObject "args" (.: "prompt")
