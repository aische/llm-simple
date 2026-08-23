-- | Multi-round agent loop with tool execution.
--
-- 'generateText' and 'streamText' call the model, execute any returned tool
-- calls, append 'ToolTurn' results, and repeat until the model replies with
-- plain text or a limit is hit ('Agent.agMaxToolRounds', abort signal).
--
-- Tool implementations live in a separate 'ToolMap'; 'Agent.agTools' is the
-- allow-list of names exposed to the model. When 'Agent.agContextWindow' is
-- set, older turns are dropped from the provider request and a @get_history@
-- tool is injected automatically so the model can page through hidden history.
--
-- For structured output without tools, see 'generateObject' and
-- 'generateObjectUntyped'.
module LLM.Agent
  ( generateText,
    streamText,
    generateObject,
    generateObjectUntyped,
    Agent (..),
    RuntimeArgs (..),
    Tool (..),
    ToolContext (..),
    GenerateEvent (..),
    GenerateEventDetail (..),
    EventObserver,
    noEventObserver,
    toTool,
    createGenRequest,
    createGenRequestNoTools,
  )
where

import LLM.Agent.Events
import LLM.Agent.Generate
import LLM.Agent.GenerateObject
import LLM.Agent.ToolUtils
import LLM.Agent.Types