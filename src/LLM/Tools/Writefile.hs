module LLM.Tools.Writefile (writefileToolTyped, WritefileToolArgs (..)) where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, sandboxWritePath)

data WritefileToolArgs = WritefileToolArgs
  { _wfPath :: Text,
    _wfContent :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec WritefileToolArgs)

instance AC.HasCodec WritefileToolArgs where
  codec :: AC.JSONCodec WritefileToolArgs
  codec =
    AC.object "write content to a file" $
      WritefileToolArgs
        <$> AC.requiredField "path" "Relative file path to write to" AC..= (._wfPath)
        <*> AC.requiredField "content" "The text content to write to the file" AC..= (._wfContent)

writefileToolTyped :: FsConfig -> TypedTool ctx WritefileToolArgs
writefileToolTyped cfg =
  TypedTool
    { ttoolName = "writefile",
      ttoolDescription =
        "Write content to a file at the given path (relative to the workspace). "
          <> "Creates the file if it doesn't exist, overwrites if it does. "
          <> "Automatically creates parent directories as needed.",
      ttoolReadonly = False,
      ttoolExecute = const (writefileExecTyped cfg)
    }

writefileExecTyped :: FsConfig -> WritefileToolArgs -> IO Text
writefileExecTyped cfg args = do
  let p = args._wfPath
      c = args._wfContent
  resolved <- sandboxWritePath cfg (T.unpack p)
  TIO.writeFile resolved c
  pure $ "Successfully wrote " <> T.pack (show (T.length c)) <> " characters to " <> p
