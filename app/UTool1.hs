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
import LLM.Agent.Generate4 (AgentCommand (PushSubagent), SubagentSpec (..), ToolOutcome (ToolCommand), TranscriptPolicy (..), UTool (..))
import LLM.Agent.Types (Agent, RuntimeArgs)
import LLM.Core.Types
  ( ToolDef (..),
    Turn (UserTurn),
  )

uTool1 :: (Agent, ModelWithFallbacks, RuntimeArgs) -> UTool
uTool1 (a, m, r) =
  UTool
    { utToolDef =
        ToolDef
          { toolName = "subagent",
            toolDescription = "subagent with file system access",
            toolReadonly = False,
            toolParameters = uTool1Schema
          },
      utToolExecute = const (getUTool1 (a, m, r))
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

-- | Dummy implementation — in reality you'd call a weather API
getUTool1 :: (Agent, ModelWithFallbacks, RuntimeArgs) -> Value -> IO ToolOutcome
getUTool1 (a, m, r) args = do
  let prompt = fromMaybe "unknown" $ parseMaybe parsePrompt args
  -- error "Age database is currently unavailable"
  --   pure $ ToolReply "uTool1"
  pure $
    ToolCommand $
      PushSubagent $
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
