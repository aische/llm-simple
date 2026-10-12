module LLM.Agent.Events
  ( emitEvent,
    noEventObserver,
  )
where

import Control.Monad (void)
import LLM.Agent.Types
  ( EventObserver,
    GenerateEvent (..),
    GenerateEventDetail,
    RuntimeArgs (..),
  )
import LLM.Core.Utils (trySync)

-- | Emit a lifecycle event, attaching 'rtGenerationId' from runtime.
-- Synchronous observer exceptions are swallowed so observers cannot abort
-- the loop; asynchronous ones (timeout, cancellation) propagate.
emitEvent :: RuntimeArgs -> GenerateEventDetail -> IO ()
emitEvent rt detail =
  void (trySync (rt.rtOnEvent (GenerateEvent rt.rtGenerationId detail)))

noEventObserver :: EventObserver
noEventObserver _ = pure ()
