module LLM.Load.Types where

import Control.Exception (Exception)
import Data.Text (Text)

-- | Errors raised while loading model catalogs or resolving configurations.
data LoadConfigError
  = -- | The catalog file is missing, contains invalid JSON, or has an
    -- invalid entry (a repeated name, a non-positive @maxTokens@, a
    -- negative limit or price).
    LoadModelCatalogError Text
  | -- | The provider catalog file is missing, contains invalid JSON, or
    -- has two entries with the same @providerName@.
    LoadProviderCatalogError Text
  | -- | A catalog entry references an unknown model or provider.
    LoadModelConfigError Text
  | -- | A @file:@ system prompt path could not be read.
    LoadSystemPromptError Text
  deriving (Show)

instance Exception LoadConfigError
