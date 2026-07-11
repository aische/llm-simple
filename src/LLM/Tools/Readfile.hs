module LLM.Tools.Readfile (readfileToolTyped, ReadfileToolArgs (..)) where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, sandboxPath)
import LLM.Tools.FsLimits (maxReadBytes, readBoundedTextFile)

newtype ReadfileToolArgs = ReadfileToolArgs
  { _rfPath :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec ReadfileToolArgs)

instance AC.HasCodec ReadfileToolArgs where
  codec =
    AC.object "read a file" $
      ReadfileToolArgs <$> AC.requiredField "path" "Relative file path to read" AC..= (._rfPath)

readfileToolTyped :: FsConfig -> TypedTool ctx ReadfileToolArgs
readfileToolTyped cfg =
  TypedTool
    { ttoolName = "readfile",
      ttoolDescription =
        "Read the contents of a text file at the given path (relative to the workspace). "
          <> "Returns the full file content as text. Binary files are refused. "
          <> "Files larger than "
          <> T.pack (show maxReadBytes)
          <> " bytes are refused; use read_file_paginated instead.",
      ttoolReadonly = True,
      ttoolExecute = const (readfileExecTyped cfg)
    }

readfileExecTyped :: FsConfig -> ReadfileToolArgs -> IO Text
readfileExecTyped cfg args = do
  let p = args._rfPath
  resolved <- sandboxPath cfg (T.unpack p)
  result <- readBoundedTextFile resolved p maxReadBytes
  pure $ case result of
    Left err -> "Error: " <> err
    Right content -> content
