module LLM.Generate.GenerateObject
  ( generateObject,
    generateObjectUntyped,
  )
where

import Autodocodec qualified as AC
import Autodocodec.Schema (jsonSchemaVia)
import Data.Aeson (Value)
import Data.Aeson qualified as AE
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import LLM.Core.Abort (isAbortedMaybe)
import LLM.Core.ProviderUtils (stripBoundsAndComments)
import LLM.Core.Types (LLMGateway (..), LLMObjectResult, Turn (..))
import LLM.Core.Usage (Usage (..), emptyUsage)
import LLM.Generate.Events (emitEvent)
import LLM.Generate.GenerateUtils
  ( callWithRetryTimeout,
    mkRequest,
    usageWithModelCost,
    withModelFallbacks,
  )
import LLM.Generate.ModelConfig (ModelConfig (mcGateway), ModelWithFallbacks)
import LLM.Generate.Types
  ( Agent,
    GeneratableObject,
    GenerateError (..),
    GenerateErrorResult (..),
    GenerateEventDetail (..),
    GenerateResult,
    RuntimeArgs (..),
  )

generateObject ::
  (GeneratableObject t) =>
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult (t, Usage))
generateObject = generateObjectInternal AC.codec

generateObjectInternal ::
  (GeneratableObject t) =>
  AC.JSONCodec t ->
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  IO (Either GenerateErrorResult (t, Usage))
generateObjectInternal codec agent models rt messages = do
  let jsonschema = stripBoundsAndComments $ AE.toJSON $ jsonSchemaVia codec
  res <- generateObjectUntyped agent models rt messages jsonschema
  case res of
    Left e -> pure (Left e)
    Right (v, u) -> do
      case AE.fromJSON v of
        AE.Error e ->
          pure $
            Left $
              GenerateErrorResult
                (GErrParseObjectError $ "Can't decode object returned from generateObjectUntyped" <> T.pack (show e))
                (rtGenerationId rt)
                []
                u
        AE.Success a -> pure $ Right (a, u)

generateObjectUntyped ::
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  Value ->
  IO (Either GenerateErrorResult (Value, Usage))
generateObjectUntyped agent models rt messages schema = do
  emitEvent rt GenerationStarted
  aborted <- isAbortedMaybe (rtAbortSignal rt)
  if aborted
    then do
      let errResult = GenerateErrorResult GErrAborted (rtGenerationId rt) [] emptyUsage
      emitEvent rt (GenerationFailed GErrAborted errResult)
      pure $ Left errResult
    else do
      result <- callObjectWithFallbacks agent models rt messages schema
      case result of
        Left err -> do
          let errResult = GenerateErrorResult err (rtGenerationId rt) [] emptyUsage
          emitEvent rt (GenerationFailed err errResult)
          pure $ Left errResult
        Right (value, usage) -> do
          pure $ Right (value, usage)

callObject ::
  Agent ->
  ModelConfig ->
  RuntimeArgs ->
  [Turn] ->
  Value ->
  IO LLMObjectResult
callObject agent mc rt turns schema =
  callWithRetryTimeout rt mc $
    gwGenerateObject
      (mcGateway mc)
      (rtLLMHooks rt)
      schema
      (mkRequest agent mc turns (rtReadonly rt))

callObjectWithFallbacks ::
  Agent ->
  ModelWithFallbacks ->
  RuntimeArgs ->
  [Turn] ->
  Value ->
  IO (GenerateResult (Value, Usage))
callObjectWithFallbacks agent models rt turns schema =
  withModelFallbacks rt models $ \mc -> do
    r <- callObject agent mc rt turns schema
    case r of
      Left err -> pure $ Left err
      Right (v, mbu) ->
        pure $ Right (v, usageWithModelCost mc (fromMaybe emptyUsage mbu))
