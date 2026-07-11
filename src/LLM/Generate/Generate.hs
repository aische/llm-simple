module LLM.Generate.Generate
  ( usageWithModelCost,
    generateTextLLM,
    streamTextLLM,
    generateTextWithFallbacks,
    streamTextWithFallbacks,
  )
where

import Control.Monad (unless, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import LLM.Core.Types
  ( ChatResponse (..),
    LLMGateway (..),
    LLMTextResult,
    StreamEvent (..),
    Turn (..),
  )
import LLM.Core.Usage (emptyUsage)
import LLM.Generate.GenerateUtils
  ( callWithRetryTimeout,
    mkRequest,
    usageWithModelCost,
    withModelFallbacks,
  )
import LLM.Generate.ModelConfig
  ( ModelConfig (..),
    ModelWithFallbacks (..),
  )
import LLM.Generate.Types
  ( GenRequest (..),
    GenerateResult,
    RoundTextRole (..),
    StreamChunk (..),
  )

generateTextWithFallbacks ::
  GenRequest ->
  ModelWithFallbacks ->
  IO (GenerateResult ChatResponse)
generateTextWithFallbacks gr models =
  withModelFallbacks gr models $ \mc -> do
    r <- generateTextLLM gr mc
    case r of
      Left err -> pure $ Left err
      Right resp ->
        let usage = usageWithModelCost mc (fromMaybe emptyUsage resp.respUsage)
         in pure $ Right resp {respUsage = Just usage}

streamTextWithFallbacks ::
  (StreamChunk -> IO ()) ->
  GenRequest ->
  ModelWithFallbacks ->
  IO (GenerateResult ChatResponse)
streamTextWithFallbacks onChunk gr models =
  withModelFallbacks gr models $ \mc -> do
    r <- streamTextLLM onChunk gr mc
    case r of
      Left err -> pure $ Left err
      Right resp ->
        let usage = usageWithModelCost mc (fromMaybe emptyUsage resp.respUsage)
         in pure $ Right resp {respUsage = Just usage}

generateTextLLM :: GenRequest -> ModelConfig -> IO LLMTextResult
generateTextLLM gr mc =
  callWithRetryTimeout gr mc $
    let request = mkRequest gr mc
     in mc.mcGateway.gwGenerateText gr.grLLMHooks request

streamTextLLM :: (StreamChunk -> IO ()) -> GenRequest -> ModelConfig -> IO LLMTextResult
streamTextLLM onChunk gr mc =
  callWithRetryTimeout gr mc $
    let request = mkRequest gr mc
     in do
          ProviderStreamCallback {pscOnEvent, pscFinalize} <-
            mkProviderStreamCallback gr onChunk
          result <- mc.mcGateway.gwStreamText gr.grLLMHooks request pscOnEvent
          case result of
            Right _ -> pscFinalize >> pure result
            Left err -> pure (Left err)

data RoundPhase = Unclassified | Preamble | Answer
  deriving (Eq)

data ProviderStreamCallback = ProviderStreamCallback
  { pscOnEvent :: StreamEvent -> IO (),
    pscFinalize :: IO ()
  }

initialPhase :: [Turn] -> RoundPhase
initialPhase turns =
  case reverse turns of
    ToolTurn _ : _ -> Answer
    _ -> Unclassified

mkProviderStreamCallback ::
  GenRequest ->
  (StreamChunk -> IO ()) ->
  IO ProviderStreamCallback
mkProviderStreamCallback gr onChunk = do
  phaseRef <- newIORef (initialPhase gr.grMessages)
  pure
    ProviderStreamCallback
      { pscOnEvent = \case
          StreamReasoningDelta txt -> onChunk (ReasoningDelta txt)
          StreamDelta txt -> do
            phase <- readIORef phaseRef
            case phase of
              Answer -> onChunk (AnswerDelta txt)
              Preamble -> onChunk (PreambleDelta txt)
              Unclassified -> onChunk (TextDelta txt)
          StreamToolCall tc -> do
            phase <- readIORef phaseRef
            unless (phase == Preamble) $
              onChunk (RoundTextRoleCommitted PreambleRole)
            writeIORef phaseRef Preamble
            onChunk (StreamToolCallChunk tc),
        pscFinalize = do
          phase <- readIORef phaseRef
          when (phase == Unclassified) $
            onChunk (RoundTextRoleCommitted AnswerRole)
      }
