-- | Implementation of "LLM.Generate.Generate", plus the variants that skip
-- the turn check ('TurnCheck') for the agent loop.
module LLM.Generate.Internal.Generate
  ( usageWithModelCost,
    generateTextLLM,
    streamTextLLM,
    generateTextWithFallbacks,
    streamTextWithFallbacks,
    generateTextWithFallbacksRotated,
    streamTextWithFallbacksRotated,
    generateTextWithFallbacksRotatedWith,
    streamTextWithFallbacksRotatedWith,
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
import LLM.Generate.Internal.GenerateUtils
  ( callWithRetry,
    callWithRetryTimeout,
    mkRequest,
    usageWithModelCost,
    safeGenRequest,
    TurnCheck (..),
    withModelFallbacksRotatedWith,
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
-- Tries 'LLM.Generate.ModelConfig.mwfModel' first, then each fallback in order. Returns a provider
-- 'ChatResponse' on success; does not execute tools.
generateTextWithFallbacks ::
  GenRequest ->
  ModelWithFallbacks ->
  IO (GenerateResult ChatResponse)
generateTextWithFallbacks gr models =
  fmap snd <$> generateTextWithFallbacksRotated (safeGenRequest gr) models

-- | Like 'generateTextWithFallbacks', but also returns the chain rotated so
-- the model that answered comes first (see 'LLM.Generate.GenerateUtils.withModelFallbacksRotated').
generateTextWithFallbacksRotated ::
  GenRequest ->
  ModelWithFallbacks ->
  IO (GenerateResult (ModelWithFallbacks, ChatResponse))
generateTextWithFallbacksRotated = generateTextWithFallbacksRotatedWith CheckTurns

-- | 'generateTextWithFallbacksRotated' with the choice of 'TurnCheck'.
generateTextWithFallbacksRotatedWith ::
  TurnCheck ->
  GenRequest ->
  ModelWithFallbacks ->
  IO (GenerateResult (ModelWithFallbacks, ChatResponse))
generateTextWithFallbacksRotatedWith check gr0 models =
  withModelFallbacksRotatedWith check gr models $ \mc -> do
    r <- generateTextLLM gr mc
    case r of
      Left err -> pure $ Left err
      Right resp ->
        let usage = usageWithModelCost mc (fromMaybe emptyUsage resp.respUsage)
         in pure $ Right resp {respUsage = Just usage}
  where
    gr = safeGenRequest gr0

-- | Like 'generateTextWithFallbacks', but streams 'StreamChunk' values
-- through @onChunk@ as the provider responds.
streamTextWithFallbacks ::
  (StreamChunk -> IO ()) ->
  GenRequest ->
  ModelWithFallbacks ->
  IO (GenerateResult ChatResponse)
streamTextWithFallbacks onChunk gr models =
  fmap snd <$> streamTextWithFallbacksRotated onChunk (safeGenRequest gr) models

-- | Like 'streamTextWithFallbacks', but also returns the rotated chain
-- (see 'LLM.Generate.GenerateUtils.withModelFallbacksRotated').
streamTextWithFallbacksRotated ::
  (StreamChunk -> IO ()) ->
  GenRequest ->
  ModelWithFallbacks ->
  IO (GenerateResult (ModelWithFallbacks, ChatResponse))
streamTextWithFallbacksRotated = streamTextWithFallbacksRotatedWith CheckTurns

-- | 'streamTextWithFallbacksRotated' with the choice of 'TurnCheck'.
streamTextWithFallbacksRotatedWith ::
  TurnCheck ->
  (StreamChunk -> IO ()) ->
  GenRequest ->
  ModelWithFallbacks ->
  IO (GenerateResult (ModelWithFallbacks, ChatResponse))
streamTextWithFallbacksRotatedWith check onChunk gr0 models = do
  let gr = safeGenRequest gr0
  emittedRef <- newIORef False
  withModelFallbacksRotatedWith check gr models $ \mc -> do
    r <- streamTextAttempts emittedRef onChunk gr mc
    case r of
      Left err -> pure $ Left err
      Right resp ->
        let usage = usageWithModelCost mc (fromMaybe emptyUsage resp.respUsage)
         in pure $ Right resp {respUsage = Just usage}

generateTextLLM :: GenRequest -> ModelConfig -> IO LLMTextResult
generateTextLLM gr0 mc =
  callWithRetryTimeout gr mc $
    let request = mkRequest gr mc
     in mc.mcGateway.gwGenerateText gr.grLLMHooks request
  where
    gr = safeGenRequest gr0

-- | Stream one model call with retries. 'LLM.Generate.ModelConfig.mcRequestTimeout' bounds the time
-- until the first stream event of each attempt, not the whole stream.
--
-- If an attempt fails after streaming chunks, the next attempt first emits
-- 'StreamAttemptReset'.
streamTextLLM :: (StreamChunk -> IO ()) -> GenRequest -> ModelConfig -> IO LLMTextResult
streamTextLLM onChunk gr mc = do
  emittedRef <- newIORef False
  streamTextAttempts emittedRef onChunk (safeGenRequest gr) mc

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
          -- Deliver no event after the abort signal fired. ('callWithRetry'
          -- also interrupts the attempt, but asynchronously, so an event
          -- could otherwise slip through first.)
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
