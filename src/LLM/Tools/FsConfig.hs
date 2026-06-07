module LLM.Tools.FsConfig
  ( FsConfig (..),
    SandboxViolation (..),
    mkFsConfig,
    sandboxPath,
    sandboxWritePath,
    isFileHidden,
    isSymlink,
  )
where

import Control.Exception (Exception, IOException, throwIO, try)
import Control.Monad (unless)
import Data.List (foldl', isPrefixOf)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesPathExist, pathIsSymbolicLink)
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
-- If the path doesn't exist yet (e.g. for writes), the longest
-- existing ancestor is canonicalized (so symlinks in the parent
-- chain are resolved) and the missing tail is appended verbatim.
-- Throws 'SandboxViolation' on escape attempts.
sandboxPath :: FsConfig -> FilePath -> IO FilePath
sandboxPath cfg relPath = do
  let base = cfg.fsBasePath
      candidate = collapseDots (normalise (base </> relPath))
  canonical <- canonicalizeExisting candidate
  unless (base `isPrefixOf` canonical || base `isPrefixOf` (canonical ++ "/")) $
    throwIO $
      SandboxViolation canonical base
  pure canonical

-- | Canonicalize the longest existing prefix of a path, then append
-- any missing trailing components. This ensures that even when the
-- final target doesn't exist yet, symlinks in the parent chain are
-- resolved before the containment check.
canonicalizeExisting :: FilePath -> IO FilePath
canonicalizeExisting path = do
  exists <- doesPathExist path
  if exists
    then canonicalizePath path
    else do
      let parent = takeDirectory path
          name = drop (length parent) path
      if parent == path
        then pure path
        else do
          parentCanon <- canonicalizeExisting parent
          pure (parentCanon ++ name)

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

-- | Check whether a path is a symbolic link (without following it).
-- Returns 'False' if the path doesn't exist or can't be stat'd.
isSymlink :: FilePath -> IO Bool
isSymlink p = do
  r <- try (pathIsSymbolicLink p) :: IO (Either IOException Bool)
  case r of
    Right b -> pure b
    Left _ -> pure False
