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

import Control.Exception (bracket)
import Control.Monad (void)
import Data.Aeson (Value)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BSL
import Data.Char (isAlphaNum, isAscii)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import LLM.Core.Utils (trySync)
import System.Directory (createDirectoryIfMissing)
import System.IO (hClose, openBinaryTempFile, stderr)

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
--
-- Files are named @{provider}-{request|response}-{timestamp}-{unique}.json@,
-- where the timestamp is in milliseconds and @{unique}@ keeps dumps written
-- in the same millisecond apart. In the provider name, every character
-- other than an ASCII letter, digit, @-@ or @_@ becomes @_@, so the file
-- always lands in @dir@. Each file is created new, readable only by its
-- owner.
withJsonDump :: FilePath -> Hooks -> Hooks
withJsonDump dir h =
  h
    { onRequest = \provider body -> do
        h.onRequest provider body
        dumpJson dir provider "request" body,
      onResponse = \provider body -> do
        h.onResponse provider body
        dumpJson dir provider "response" body
    }

dumpJson :: FilePath -> Text -> Text -> Value -> IO ()
dumpJson dir provider label val = do
  createDirectoryIfMissing True dir
  ts <- getPOSIXTime
  let tsStr = show (round (ts * 1000) :: Integer)
      template = safeFileNamePart provider <> "-" <> safeFileNamePart label <> "-" <> tsStr <> "-.json"
  -- Creates the file exclusively under a fresh name (template plus a
  -- unique number), so concurrent dumps never overwrite each other.
  bracket (openBinaryTempFile dir template) (hClose . snd) $ \(_, h) ->
    BSL.hPut h (encodePretty val)

-- | Keep ASCII letters, digits, @-@ and @_@; replace everything else
-- (path separators, dots, spaces, ...) with @_@.
safeFileNamePart :: Text -> FilePath
safeFileNamePart t
  | T.null t = "_"
  | otherwise = map safeChar (T.unpack t)
  where
    safeChar c
      | isAscii c && (isAlphaNum c || c == '-' || c == '_') = c
      | otherwise = '_'

-- | Wrap all hook callbacks so synchronous exceptions are silently caught.
-- Hooks are observability-only and should never abort control flow.
-- Asynchronous exceptions (timeout, cancellation) still propagate.
safeHooks :: Hooks -> Hooks
safeHooks h =
  Hooks
    { onLog = \level msg -> void (trySync (h.onLog level msg)),
      onRequest = \p v -> void (trySync (h.onRequest p v)),
      onResponse = \p v -> void (trySync (h.onResponse p v)),
      onResponseError = \p v -> void (trySync (h.onResponseError p v)),
      onToolCall = \p v -> void (trySync (h.onToolCall p v)),
      onToolResult = \p v -> void (trySync (h.onToolResult p v)),
      onToolError = \p v -> void (trySync (h.onToolError p v))
    }

debugHooks :: FilePath -> Hooks
debugHooks dir = withJsonDump dir . withStderrLogger Debug $ noHooks

defaultDebugHooks :: Hooks
defaultDebugHooks = debugHooks "./dumps"
