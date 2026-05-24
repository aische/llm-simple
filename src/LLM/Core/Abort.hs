module LLM.Core.Abort
  ( AbortSignal,
    newAbortSignal,
    abort,
    isAborted,
    isAbortedMaybe,
  )
where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- | A cooperative cancellation signal.
-- Create one with 'newAbortSignal', pass it into 'ChatEnv', and call
-- 'abort' from any thread to request cancellation at the next checkpoint.
newtype AbortSignal = AbortSignal (IORef Bool)

-- | Create a fresh signal (not yet aborted).
newAbortSignal :: IO AbortSignal
newAbortSignal = AbortSignal <$> newIORef False

-- | Fire the signal. Idempotent — calling it more than once is harmless.
abort :: AbortSignal -> IO ()
abort (AbortSignal ref) = writeIORef ref True

-- | Check whether the signal has been fired.
isAborted :: AbortSignal -> IO Bool
isAborted (AbortSignal ref) = readIORef ref

isAbortedMaybe :: Maybe AbortSignal -> IO Bool
isAbortedMaybe Nothing = pure False
isAbortedMaybe (Just sig) = isAborted sig
