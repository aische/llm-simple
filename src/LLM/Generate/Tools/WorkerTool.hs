module LLM.Generate.Tools.WorkerTool where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (Turn (UserTurn), TypedTool (..))
import LLM.Generate.Types (GenerateErrorResult, GenerateResult (grText), RuntimeArgs, ToolContext (tcRuntimeArgs))

type GenerateText = RuntimeArgs -> [Turn] -> IO (Either GenerateErrorResult GenerateResult)

newtype WorkerToolArgs = WorkerToolArgs
  { _workerPrompt :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec WorkerToolArgs)

instance AC.HasCodec WorkerToolArgs where
  codec :: AC.JSONCodec WorkerToolArgs
  codec =
    AC.object "WorkerToolArgs" $
      WorkerToolArgs <$> AC.requiredField "prompt" "Prompt to send to the worker" AC..= _workerPrompt

workerToolTyped :: GenerateText -> Text -> Text -> TypedTool ToolContext WorkerToolArgs
workerToolTyped gen name description =
  TypedTool
    { ttoolName = name,
      ttoolDescription = description,
      ttoolReadonly = False,
      ttoolExecute = workerExecTyped gen
    }

workerExecTyped :: GenerateText -> ToolContext -> WorkerToolArgs -> IO Text
workerExecTyped gen ctx args = do
  result <- gen (tcRuntimeArgs ctx) [UserTurn (_workerPrompt args)]
  case result of
    Left e -> pure $ "Error: " <> T.pack (show e)
    Right r -> pure (grText r)
