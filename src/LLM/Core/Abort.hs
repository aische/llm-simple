module LLM.Core.Abort
  ( AbortSignal,
    newAbortSignal,
    abort,
    isAborted,
    isAbortedMaybe,
    waitAborted,
  )
where

import Control.Concurrent.STM (TVar, atomically, check, newTVarIO, readTVar, readTVarIO, writeTVar)

-- | A cancellation signal.
-- Create one with 'newAbortSignal', pass it into 'RuntimeArgs' or 'GenRequest', and call
-- 'abort' from any thread. A model call or tool that is running when the signal
-- fires is interrupted.
newtype AbortSignal = AbortSignal (TVar Bool)

-- | Create a fresh signal (not yet aborted).
newAbortSignal :: IO AbortSignal
newAbortSignal = AbortSignal <$> newTVarIO False

-- | Fire the signal. Idempotent — calling it more than once is harmless.
abort :: AbortSignal -> IO ()
abort (AbortSignal var) = atomically (writeTVar var True)

-- | Check whether the signal has been fired.
isAborted :: AbortSignal -> IO Bool
isAborted (AbortSignal var) = readTVarIO var

isAbortedMaybe :: Maybe AbortSignal -> IO Bool
isAbortedMaybe Nothing = pure False
isAbortedMaybe (Just sig) = isAborted sig

-- | Block until the signal has been fired. Returns at once if it already has.
waitAborted :: AbortSignal -> IO ()
waitAborted (AbortSignal var) = atomically (readTVar var >>= check)
