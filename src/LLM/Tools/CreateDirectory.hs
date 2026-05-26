module LLM.Tools.CreateDirectory (createDirectoryToolTyped, CreateDirectoryToolArgs (..)) where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, sandboxPath)
import System.Directory (createDirectoryIfMissing)

data CreateDirectoryToolArgs = CreateDirectoryToolArgs
  { _cdPath :: Text,
    _cdParents :: Bool
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec CreateDirectoryToolArgs)

instance AC.HasCodec CreateDirectoryToolArgs where
  codec =
    AC.object "create a directory" $
      CreateDirectoryToolArgs
        <$> AC.requiredField "path" "Relative path of the directory to create" AC..= _cdPath
        <*> AC.optionalFieldWithDefault "parents" True "Create intermediate parent directories as needed" AC..= _cdParents

createDirectoryToolTyped :: FsConfig -> TypedTool ctx CreateDirectoryToolArgs
createDirectoryToolTyped cfg =
  TypedTool
    { ttoolName = "create_directory",
      ttoolDescription =
        "Create a directory at the given path (relative to the workspace). "
          <> "When 'parents' is true (the default), intermediate directories are created as needed "
          <> "and no error is raised if the directory already exists.",
      ttoolReadonly = False,
      ttoolExecute = const (createDirectoryExecTyped cfg)
    }

createDirectoryExecTyped :: FsConfig -> CreateDirectoryToolArgs -> IO Text
createDirectoryExecTyped cfg args = do
  let p = _cdPath args
      parents = _cdParents args
  resolved <- sandboxPath cfg (T.unpack p)
  createDirectoryIfMissing parents resolved
  pure $ "Successfully created directory " <> p
