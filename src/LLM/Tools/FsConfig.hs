module LLM.Tools.FsConfig
  ( FsConfig (..),
    SandboxViolation (..),
    mkFsConfig,
    sandboxPath,
    sandboxWritePath,
    isFileHidden,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Data.List (foldl', isPrefixOf)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesPathExist)
import System.FilePath (addTrailingPathSeparator, joinPath, normalise, splitDirectories, takeDirectory, (</>))

-- | Configuration for file-system tools.
-- 'fsBasePath' must be a canonical absolute path (use 'mkFsConfig').
newtype FsConfig = FsConfig
  { fsBasePath :: FilePath
  }
  deriving (Show)

data SandboxViolation = SandboxViolation
  { svAttempted :: FilePath,
    svBasePath :: FilePath
  }
  deriving (Show)

instance Exception SandboxViolation

-- | Create an 'FsConfig' by canonicalizing the given base directory.
-- The directory must already exist.
mkFsConfig :: FilePath -> IO FsConfig
mkFsConfig dir =
  FsConfig . addTrailingPathSeparator <$> canonicalizePath dir

-- | Resolve a (possibly relative) path against the sandbox base,
-- canonicalize it, and verify it stays within the sandbox.
-- If the path doesn't exist yet (e.g. for writes), falls back to
-- manual normalization to resolve @.@ and @..@ components.
-- Throws 'SandboxViolation' on escape attempts.
sandboxPath :: FsConfig -> FilePath -> IO FilePath
sandboxPath cfg relPath = do
  let base = fsBasePath cfg
      candidate = base </> relPath
  exists <- doesPathExist candidate
  canonical <-
    if exists
      then canonicalizePath candidate
      else pure (collapseDots (normalise candidate))
  unless (base `isPrefixOf` canonical || base `isPrefixOf` (canonical ++ "/")) $
    throwIO $
      SandboxViolation canonical base
  pure canonical

-- | Like 'sandboxPath', but also creates parent directories
-- inside the sandbox as needed (for write operations).
sandboxWritePath :: FsConfig -> FilePath -> IO FilePath
sandboxWritePath cfg relPath = do
  resolved <- sandboxPath cfg relPath
  createDirectoryIfMissing True (takeDirectory resolved)
  pure resolved

-- | Resolve @.@ and @..@ in a normalized absolute path
-- without touching the filesystem.
collapseDots :: FilePath -> FilePath
collapseDots = joinPath . reverse . foldl' step [] . splitDirectories
  where
    step acc "." = acc
    step (_ : rest) ".." = rest
    step acc ".." = acc -- at root, ignore
    step acc x = x : acc

isFileHidden :: [Char] -> Bool
isFileHidden path = case path of
  ('.' : _) -> True
  _ -> False
