module LLM.Generate.Logger
  ( Hooks (..),
    Logger,
    LogLevel (..),
    noHooks,
    withStderrLogger,
    withJsonDump,
    noLogger,
    stderrLogger,
    safeHooks,
    debugHooks,
    defaultDebugHooks,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (Value)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (stderr)

-- | Log verbosity levels, ordered from most to least verbose
data LogLevel = Debug | Info | Warn | Error
  deriving (Show, Eq, Ord)

-- | A logger callback. The library calls it; the consumer decides what to do.
type Logger = LogLevel -> Text -> IO ()

-- | No-op logger (default)
noLogger :: Logger
noLogger _ _ = pure ()

-- | Simple stderr logger that filters by minimum level
stderrLogger :: LogLevel -> Logger
stderrLogger minLevel level msg
  | level >= minLevel = TIO.hPutStrLn stderr $ "[" <> T.pack (show level) <> "] " <> msg
  | otherwise = pure ()

-- | Hooks for observing library behaviour. All callbacks are no-ops by default.
data Hooks = Hooks
  { -- | Log messages at various levels
    onLog :: Logger,
    -- | Called with (provider, request body JSON)
    onRequest :: Text -> Value -> IO (),
    -- | Called with (provider, response body JSON)
    onResponse :: Text -> Value -> IO (),
    onResponseError :: Text -> Text -> IO (),
    -- | Called with (tool call name, tool call arguments)
    onToolCall :: Text -> Value -> IO (),
    -- | Called with (tool call name, tool call result)
    onToolResult :: Text -> Text -> IO (),
    -- | Called with (tool call name, tool call error)
    onToolError :: Text -> Text -> IO ()
  }

-- | No-op hooks (default)
noHooks :: Hooks
noHooks =
  Hooks
    { onLog = noLogger,
      onRequest = \_ _ -> pure (),
      onResponse = \_ _ -> pure (),
      onResponseError = \_ _ -> pure (),
      onToolCall = \_ _ -> pure (),
      onToolResult = \_ _ -> pure (),
      onToolError = \_ _ -> pure ()
    }

-- | Add a stderr logger to existing hooks
withStderrLogger :: LogLevel -> Hooks -> Hooks
withStderrLogger minLvl h = h {onLog = stderrLogger minLvl}

-- | Add JSON request/response dumping to a directory.
-- Files are named @{provider}-{request|response}-{timestamp}.json@.
withJsonDump :: FilePath -> Hooks -> Hooks
withJsonDump dir h =
  h
    { onRequest = \provider body -> do
        onRequest h provider body
        dumpJson dir provider "request" body,
      onResponse = \provider body -> do
        onResponse h provider body
        dumpJson dir provider "response" body
    }

dumpJson :: FilePath -> Text -> Text -> Value -> IO ()
dumpJson dir provider label val = do
  createDirectoryIfMissing True dir
  ts <- getPOSIXTime
  let tsStr = show (round (ts * 1000) :: Integer)
      name = T.unpack provider <> "-" <> T.unpack label <> "-" <> tsStr <> ".json"
  BSL.writeFile (dir </> name) (encodePretty val)

-- | Wrap all hook callbacks so exceptions are silently caught.
-- Hooks are observability-only and should never abort control flow.
safeHooks :: Hooks -> Hooks
safeHooks h =
  Hooks
    { onLog = \level msg -> void (try (onLog h level msg) :: IO (Either SomeException ())),
      onRequest = \p v -> void (try (onRequest h p v) :: IO (Either SomeException ())),
      onResponse = \p v -> void (try (onResponse h p v) :: IO (Either SomeException ())),
      onResponseError = \p v -> void (try (onResponseError h p v) :: IO (Either SomeException ())),
      onToolCall = \p v -> void (try (onToolCall h p v) :: IO (Either SomeException ())),
      onToolResult = \p v -> void (try (onToolResult h p v) :: IO (Either SomeException ())),
      onToolError = \p v -> void (try (onToolError h p v) :: IO (Either SomeException ()))
    }

debugHooks :: FilePath -> Hooks
debugHooks dir = withJsonDump dir . withStderrLogger Debug $ noHooks

defaultDebugHooks :: Hooks
defaultDebugHooks = debugHooks "./dumps"
