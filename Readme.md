# llm-simple

experimental library for talking to LLMs. _Work in progress, POC level. This Readme might be outdated already._

folders/sub-packages:

- Core: Basic types and functions for creating LLMGateways for different providers
- Providers:
    - ollama
    - claude
    - gemini
    - openai
    - deepseek
- Tools: collection of file system tools
- Load: loading model catalog from json file, initializing providers with API keys
- Generate: single-request generateTextWithFallbacks, streamTextWithFallbacks, genObject, genObjectUntyped (no tools), all with model fallback logic, timeout etc
- Agent: generateText, streamText (agent loops with tool execution).
- Workflow removed / moved to different repo
