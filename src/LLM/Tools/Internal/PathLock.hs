-- | An in-process lock per file path, so that the file tools' read, modify
-- and write steps on one file do not interleave.
--
-- It only covers threads of this process. Edits made by other processes at
-- the same time are not seen and can still be lost.
module LLM.Tools.Internal.PathLock
  ( withPathLock,
  )
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, retry, writeTVar)
import Control.Exception (bracket_)
import Data.Set (Set)
import Data.Set qualified as Set
import System.IO.Unsafe (unsafePerformIO)

-- | Paths currently held by some thread.
lockedPaths :: TVar (Set FilePath)
lockedPaths = unsafePerformIO (newTVarIO Set.empty)
{-# NOINLINE lockedPaths #-}

-- | Run an action while holding the lock for @path@, waiting until no
-- other thread holds it. Use the resolved (canonical) path as the key, so
-- two spellings of one file share a lock. Not reentrant: taking the lock
-- for a path this thread already holds blocks forever.
withPathLock :: FilePath -> IO a -> IO a
withPathLock path = bracket_ acquire release
  where
    acquire = atomically $ do
      held <- readTVar lockedPaths
      if Set.member path held
        then retry
        else writeTVar lockedPaths (Set.insert path held)
    release = atomically (modifyTVar' lockedPaths (Set.delete path))
