module LLM.Agent.GenerateObject where

import Data.Aeson (Value)
import LLM.Agent.Types
  ( Agent (..),
    GeneratableObject,
    RuntimeArgs (..),
    createGenRequest,
  )
import LLM.Core.Types
  ( Turn (..),
  )
import LLM.Core.Usage (Usage (..))
import LLM.Generate0.GenerateObject qualified as G0
import LLM.Generate0.ModelConfig
  ( ModelWithFallbacks (..),
  )
import LLM.Generate0.Types (GenerateErrorResult)

generateObject ::
  (GeneratableObject t) =>
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult (t, Usage))
generateObject a m r t = G0.generateObject (createGenRequest a r t) m

generateObjectUntyped ::
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  Value ->
  IO (Either GenerateErrorResult (Value, Usage))
generateObjectUntyped a m r t = G0.generateObjectUntyped (createGenRequest a r t) m
