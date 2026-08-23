-- | Collection of LLM providers
module LLM.Providers
  ( -- * OpenAI
    openAIProvider,
    openAIGateway,

    -- * Gemini
    geminiProvider,
    geminiProviderWith,
    geminiGateway,
    geminiGatewayWith,

    -- * Claude
    claudeProvider,
    claudeProviderWith,
    claudeGateway,
    claudeGatewayWith,

    -- * Ollama
    ollamaProvider,
    ollamaGateway,

    -- * DeepSeek
    deepSeekProvider,
    deepSeekGateway,
  )
where

import LLM.Providers.Claude (claudeGateway, claudeGatewayWith, claudeProvider, claudeProviderWith)
import LLM.Providers.DeepSeek (deepSeekGateway, deepSeekProvider)
import LLM.Providers.Gemini (geminiGateway, geminiGatewayWith, geminiProvider, geminiProviderWith)
import LLM.Providers.Ollama (ollamaGateway, ollamaProvider)
import LLM.Providers.OpenAI (openAIGateway, openAIProvider)
