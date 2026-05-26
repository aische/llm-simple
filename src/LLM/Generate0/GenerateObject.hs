module LLM.Generate0.GenerateObject
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
import LLM.Core.Types (LLMGateway (..), LLMObjectResult)
import LLM.Core.Usage (Usage (..), emptyUsage)
import LLM.Generate0.GenerateUtils
  ( callWithRetryTimeout,
    mkRequest,
    usageWithModelCost,
    withModelFallbacks,
  )
import LLM.Generate0.ModelConfig (ModelConfig (mcGateway), ModelWithFallbacks)
import LLM.Generate0.Types
  ( GenRequest (grAbortSignal, grLLMHooks),
    GeneratableObject,
    GenerateError (..),
    GenerateErrorResult (..),
    GenerateResult,
  )

generateObject ::
  (GeneratableObject t) =>
  GenRequest ->
  ModelWithFallbacks ->
  IO (Either GenerateErrorResult (t, Usage))
generateObject = generateObjectInternal AC.codec

generateObjectInternal ::
  (GeneratableObject t) =>
  AC.JSONCodec t ->
  GenRequest ->
  ModelWithFallbacks ->
  IO (Either GenerateErrorResult (t, Usage))
generateObjectInternal codec gr models = do
  let jsonschema = stripBoundsAndComments $ AE.toJSON $ jsonSchemaVia codec
  res <- generateObjectUntyped gr models jsonschema
  case res of
    Left e -> pure (Left e)
    Right (v, u) -> do
      case AE.fromJSON v of
        AE.Error e ->
          pure $
            Left $
              GenerateErrorResult
                (GErrParseObjectError $ "Can't decode object returned from generateObjectUntyped" <> T.pack (show e))
                []
                u
        AE.Success a -> pure $ Right (a, u)

generateObjectUntyped ::
  GenRequest ->
  ModelWithFallbacks ->
  Value ->
  IO (Either GenerateErrorResult (Value, Usage))
generateObjectUntyped gr models schema = do
  aborted <- isAbortedMaybe (grAbortSignal gr)
  if aborted
    then do
      let errResult = GenerateErrorResult GErrAborted [] emptyUsage
      pure $ Left errResult
    else do
      result <- callObjectWithFallbacks gr models schema
      case result of
        Left err -> do
          let errResult = GenerateErrorResult err [] emptyUsage
          pure $ Left errResult
        Right (value, usage) -> do
          pure $ Right (value, usage)

callObject ::
  GenRequest ->
  ModelConfig ->
  Value ->
  IO LLMObjectResult
callObject gr mc schema =
  callWithRetryTimeout gr mc $
    gwGenerateObject
      (mcGateway mc)
      (grLLMHooks gr)
      schema
      (mkRequest gr mc)

callObjectWithFallbacks ::
  GenRequest ->
  ModelWithFallbacks ->
  Value ->
  IO (GenerateResult (Value, Usage))
callObjectWithFallbacks gr models schema =
  withModelFallbacks gr models $ \mc -> do
    r <- callObject gr mc schema
    case r of
      Left err -> pure $ Left err
      Right (v, mbu) ->
        pure $ Right (v, usageWithModelCost mc (fromMaybe emptyUsage mbu))
