module LLM.Tools.Readfile (readfileToolTyped, ReadfileToolArgs (..)) where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, sandboxPath)

newtype ReadfileToolArgs = ReadfileToolArgs
  { _rfPath :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec ReadfileToolArgs)

instance AC.HasCodec ReadfileToolArgs where
  codec =
    AC.object "read a file" $
      ReadfileToolArgs <$> AC.requiredField "path" "Relative file path to read" AC..= _rfPath

readfileToolTyped :: FsConfig -> TypedTool ctx ReadfileToolArgs
readfileToolTyped cfg =
  TypedTool
    { ttoolName = "read_file",
      ttoolDescription =
        "Read the contents of a file at the given path (relative to the workspace). "
          <> "Returns the full file content as text.",
      ttoolReadonly = True,
      ttoolExecute = const (readfileExecTyped cfg)
    }

readfileExecTyped :: FsConfig -> ReadfileToolArgs -> IO Text
readfileExecTyped cfg args = do
  let p = _rfPath args
  resolved <- sandboxPath cfg (T.unpack p)
  TIO.readFile resolved
