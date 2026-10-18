module LLM.Generate.Generate
  ( usageWithModelCost,
    generateTextLLM,
    streamTextLLM,
    generateTextWithFallbacks,
    streamTextWithFallbacks,
    generateTextWithFallbacksRotated,
    streamTextWithFallbacksRotated,
  )
where

import Control.Exception (Exception, handle, throwIO)
import Control.Monad (unless, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import LLM.Core.Abort (isAbortedMaybe)
import LLM.Core.Types
  ( ChatResponse (..),
    LLMError (Aborted),
    LLMGateway (..),
    LLMTextResult,
    StreamEvent (..),
    Turn (..),
  )
import LLM.Core.Usage (emptyUsage)
import LLM.Core.Utils (withFirstEventTimeout)
import LLM.Generate.GenerateUtils
  ( callWithRetry,
    callWithRetryTimeout,
    mkRequest,
    usageWithModelCost,
    withModelFallbacksRotated,
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

-- | Generate text with retries and model fallbacks.
--
-- Tries 'mwfModel' first, then each fallback in order. Returns a provider
-- 'ChatResponse' on success; does not execute tools.
generateTextWithFallbacks ::
  GenRequest ->
  ModelWithFallbacks ->
  IO (GenerateResult ChatResponse)
generateTextWithFallbacks gr models =
  fmap snd <$> generateTextWithFallbacksRotated gr models

-- | Like 'generateTextWithFallbacks', but also returns the chain rotated so
-- the model that answered comes first (see 'withModelFallbacksRotated').
generateTextWithFallbacksRotated ::
  GenRequest ->
  ModelWithFallbacks ->
  IO (GenerateResult (ModelWithFallbacks, ChatResponse))
generateTextWithFallbacksRotated gr models =
  withModelFallbacksRotated gr models $ \mc -> do
    r <- generateTextLLM gr mc
    case r of
      Left err -> pure $ Left err
      Right resp ->
        let usage = usageWithModelCost mc (fromMaybe emptyUsage resp.respUsage)
         in pure $ Right resp {respUsage = Just usage}

-- | Like 'generateTextWithFallbacks', but streams 'StreamChunk' values
-- through @onChunk@ as the provider responds.
streamTextWithFallbacks ::
  (StreamChunk -> IO ()) ->
  GenRequest ->
  ModelWithFallbacks ->
  IO (GenerateResult ChatResponse)
streamTextWithFallbacks onChunk gr models =
  fmap snd <$> streamTextWithFallbacksRotated onChunk gr models

-- | Like 'streamTextWithFallbacks', but also returns the rotated chain
-- (see 'withModelFallbacksRotated').
streamTextWithFallbacksRotated ::
  (StreamChunk -> IO ()) ->
  GenRequest ->
  ModelWithFallbacks ->
  IO (GenerateResult (ModelWithFallbacks, ChatResponse))
streamTextWithFallbacksRotated onChunk gr models = do
  emittedRef <- newIORef False
  withModelFallbacksRotated gr models $ \mc -> do
    r <- streamTextAttempts emittedRef onChunk gr mc
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

-- | Stream one model call with retries. 'mcRequestTimeout' bounds the time
-- until the first stream event of each attempt, not the whole stream.
--
-- If an attempt fails after streaming chunks, the next attempt first emits
-- 'StreamAttemptReset'.
streamTextLLM :: (StreamChunk -> IO ()) -> GenRequest -> ModelConfig -> IO LLMTextResult
streamTextLLM onChunk gr mc = do
  emittedRef <- newIORef False
  streamTextAttempts emittedRef onChunk gr mc

-- | 'streamTextLLM' with the \"chunks emitted\" flag supplied by the caller,
-- so it is shared across retries and model fallbacks of one model call.
streamTextAttempts ::
  IORef Bool ->
  (StreamChunk -> IO ()) ->
  GenRequest ->
  ModelConfig ->
  IO LLMTextResult
streamTextAttempts emittedRef onChunk gr mc =
  callWithRetry gr mc $
    let request = mkRequest gr mc
        onChunk' c = writeIORef emittedRef True >> onChunk c
     in do
          emitted <- readIORef emittedRef
          when emitted $ do
            onChunk StreamAttemptReset
            writeIORef emittedRef False
          ProviderStreamCallback {pscOnEvent, pscFinalize} <-
            mkProviderStreamCallback gr onChunk'
          -- Cut the stream off at the first event after the abort signal
          -- fired: the callback throws out of the provider's stream reader.
          let onEvent e = do
                aborted <- isAbortedMaybe gr.grAbortSignal
                if aborted then throwIO StreamAborted else pscOnEvent e
          result <-
            handle (\StreamAborted -> pure (Left Aborted)) $
              withFirstEventTimeout
                mc.mcRequestTimeout
                (mc.mcGateway.gwStreamText gr.grLLMHooks request)
                onEvent
          case result of
            Right _ -> pscFinalize >> pure result
            Left err -> pure (Left err)

-- | Thrown from the stream callback once the abort signal has fired.
data StreamAborted = StreamAborted
  deriving (Show)

instance Exception StreamAborted

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
