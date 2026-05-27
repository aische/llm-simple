module LLM.Agent.Events
  ( emitEvent,
    noEventObserver,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import LLM.Agent.Types
  ( EventObserver,
    GenerateEvent (..),
    GenerateEventDetail,
    RuntimeArgs (..),
  )

-- | Emit a lifecycle event, attaching 'rtGenerationId' from runtime.
-- Observer exceptions are swallowed so observers cannot abort the loop.
emitEvent :: RuntimeArgs -> GenerateEventDetail -> IO ()
emitEvent rt detail =
  void (try (rt.rtOnEvent (GenerateEvent rt.rtGenerationId detail)) :: IO (Either SomeException ()))

noEventObserver :: EventObserver
noEventObserver _ = pure ()
