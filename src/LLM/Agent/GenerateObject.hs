module LLM.Agent.GenerateObject where

import Data.Aeson (Value)
import LLM.Agent.ToolUtils (createGenRequestNoTools)
import LLM.Agent.Types
  ( Agent (..),
    RuntimeArgs (..),
  )
import LLM.Core.Types
  ( Turn (..),
  )
import LLM.Core.Usage (Usage (..))
import LLM.Generate.GenerateObject
import LLM.Generate.ModelConfig
  ( ModelWithFallbacks (..),
  )
import LLM.Generate.Types (GeneratableObject, GenerateErrorResult)

-- | Generate a typed Haskell value from the model via Autodocodec.
--
-- Tools are never sent: @grTools@ is always empty. 'agContextWindow' still
-- truncates messages, but does not inject @get_history@.
generateObject ::
  (GeneratableObject t) =>
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult (t, Usage))
generateObject a m r t = genObject (createGenRequestNoTools a r t) m

-- | Generate a JSON 'Value' from the model using a caller-supplied schema.
--
-- Same tool policy as 'generateObject': windowing may truncate, tools are not
-- advertised or executed.
generateObjectUntyped ::
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  Value ->
  IO (Either GenerateErrorResult (Value, Usage))
generateObjectUntyped a m r t = genObjectUntyped (createGenRequestNoTools a r t) m
