module LLM.Generate.Log
  ( formatTryingModel,
    formatModelFallback,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Types (LLMError, LLMGateway (gwName))
import LLM.Generate.ModelConfig (ModelConfig (mcGateway, mcModel))


formatTryingModel :: ModelConfig -> Text
formatTryingModel mc =
  "Trying model: "
    <> mcModel mc
    <> " via "
    <> gwName (mcGateway mc)

formatModelFallback :: ModelConfig -> LLMError -> Text
formatModelFallback mc err =
  "Falling back from "
    <> mcModel mc
    <> ": "
    <> T.pack (show err)
