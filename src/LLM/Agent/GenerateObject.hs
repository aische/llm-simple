module LLM.Agent.GenerateObject where

import Data.Aeson (Value)
import Data.Map qualified as Map
import LLM.Agent.ToolUtils (createGenRequest)
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

generateObject ::
  (GeneratableObject t) =>
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult (t, Usage))
generateObject a m r t = genObject (createGenRequest a Map.empty r t) m

generateObjectUntyped ::
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  Value ->
  IO (Either GenerateErrorResult (Value, Usage))
generateObjectUntyped a m r t = genObjectUntyped (createGenRequest a Map.empty r t) m
