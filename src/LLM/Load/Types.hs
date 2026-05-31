module LLM.Load.Types where

import Data.Text (Text)

data LoadConfigError
  = LoadModelCatalogError Text
  | LoadModelConfigError Text
  | LoadSystemPromptError Text
  deriving (Show)
