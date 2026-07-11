module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import Control.Monad (unless, void, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Types (LLMGateway)
import LLM.Providers.Claude (claudeGateway)
import LLM.Providers.DeepSeek (deepSeekGateway)
import LLM.Providers.Gemini (geminiGateway)
import LLM.Providers.Ollama (ollamaGateway)
import LLM.Providers.OpenAI (openAIGateway)
import LLM.TestKit (recordConversation, recordedConversationPrompts)
import Options.Applicative
  ( Parser,
    ParserInfo,
    auto,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    optional,
    option,
    progDesc,
    strOption,
    switch,
    value,
    (<**>),
  )
import System.Environment (lookupEnv)
import System.Exit (die)

data Options = Options
  { optProvider :: ProviderName,
    optModel :: Maybe Text,
    optGeneratedOut :: FilePath,
    optStreamedOut :: FilePath,
    optGeneratedOnly :: Bool,
    optStreamedOnly :: Bool
  }

data ProviderName = Ollama | OpenAI | Claude | Gemini | DeepSeek
  deriving (Show, Read)

providerParser :: Parser ProviderName
providerParser =
  option auto $
    mconcat
      [ long "provider",
        metavar "PROVIDER",
        value DeepSeek,
        help "Provider to record (Ollama, OpenAI, Claude, Gemini, DeepSeek)"
      ]

optionsParser :: Parser Options
optionsParser =
  Options
    <$> providerParser
    <*> optional
      ( fmap T.pack $
          strOption $
            mconcat
              [ long "model",
                metavar "MODEL",
                help "Provider model name (defaults per provider when omitted)"
              ]
      )
    <*> strOption
      ( mconcat
          [ long "generated-out",
            metavar "PATH",
            help "Output path for non-streaming fixture"
          ]
      )
    <*> strOption
      ( mconcat
          [ long "streamed-out",
            metavar "PATH",
            help "Output path for streaming fixture"
          ]
      )
    <*> switch
      ( mconcat
          [ long "generated-only",
            help "Record only the non-streaming fixture"
          ]
      )
    <*> switch
      ( mconcat
          [ long "streamed-only",
            help "Record only the streaming fixture"
          ]
      )

main :: IO ()
main = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()
  opts <- execParser optionsInfo
  when (opts.optGeneratedOnly && opts.optStreamedOnly) $
    die "--generated-only and --streamed-only are mutually exclusive"
  gateway <- loadGateway opts.optProvider
  let modelName = fromMaybe (defaultModel opts.optProvider) opts.optModel
  putStrLn $
    unlines
      [ "Recording conversation fixtures",
        "  provider: " <> show opts.optProvider,
        "  model: " <> T.unpack modelName,
        "  prompts: " <> show (map T.unpack recordedConversationPrompts)
      ]
  unless opts.optStreamedOnly $
    void $ recordConversation False gateway modelName opts.optGeneratedOut
  unless opts.optGeneratedOnly $
    void $ recordConversation True gateway modelName opts.optStreamedOut
  putStrLn "Done."

optionsInfo :: ParserInfo Options
optionsInfo =
  info
    (optionsParser <**> helper)
    ( mconcat
        [ fullDesc,
          progDesc "Record live LLM conversation fixtures for replay tests.",
          header "record-conversation - capture provider request/response fixtures"
        ]
    )

loadGateway :: ProviderName -> IO LLMGateway
loadGateway = \case
  Ollama -> pure ollamaGateway
  OpenAI -> requireEnv "OPENAI_API_KEY" >>= pure . openAIGateway . T.pack
  Claude -> requireEnv "CLAUDE_API_KEY" >>= pure . claudeGateway . T.pack
  Gemini -> requireEnv "GEMINI_API_KEY" >>= pure . geminiGateway . T.pack
  DeepSeek -> requireEnv "DEEPSEEK_API_KEY" >>= pure . deepSeekGateway . T.pack

requireEnv :: String -> IO String
requireEnv name = do
  mVal <- lookupEnv name
  case mVal of
    Nothing -> die $ name <> " is not set"
    Just val | null val -> die $ name <> " is empty"
    Just val -> pure val

defaultModel :: ProviderName -> Text
defaultModel = \case
  Ollama -> "llama3.2:latest"
  OpenAI -> "gpt-4.1-2025-04-14"
  Claude -> "claude-haiku-4-5-20251001"
  Gemini -> "gemini-2.5-flash"
  DeepSeek -> "deepseek-v4-flash"
