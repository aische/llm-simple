module LLM.Load.Types where

import Control.Exception (Exception)
import Data.Text (Text)

data LoadConfigError
  = LoadModelCatalogError Text
  | LoadModelConfigError Text
  | LoadSystemPromptError Text
  deriving (Show)

instance Exception LoadConfigError
