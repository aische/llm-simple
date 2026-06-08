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
import Control.Monad (when)
import Data.List (foldl')
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesPathExist, pathIsSymbolicLink)
import System.FilePath (addTrailingPathSeparator, dropTrailingPathSeparator, equalFilePath, joinPath, normalise, splitDirectories, splitFileName, takeDirectory, (</>))

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
  let baseComps = splitDirectories (dropTrailingPathSeparator base)
      canonComps = splitDirectories canonical
  case stripPathPrefix baseComps canonComps of
    Nothing -> throwIO (SandboxViolation canonical base)
    Just tailComps -> do
      -- TOCTOU mitigation: the canonical path has symlinks resolved as of
      -- the moment of 'canonicalizeExisting'. Between that check and the
      -- tool's subsequent open, an attacker (or a concurrent process
      -- inside the sandbox) could replace a path component with a symlink
      -- pointing outside. Re-walk the live chain from 'base' to
      -- 'canonical' and refuse any symlink encountered: since the
      -- canonical chain should not contain symlinks by construction, any
      -- symlink found here is a fresh injection. Note: this narrows but
      -- does not fully close the TOCTOU window; a complete fix requires
      -- fd-relative ('openat') APIs not available in the portable
      -- 'directory' package.
      verifyNoSymlinksInChain base (joinPath baseComps) tailComps
      pure canonical

-- | Element-wise path-prefix comparison.
-- Returns the trailing components of @full@ after stripping @prefix@,
-- or 'Nothing' if @prefix@ is not a path-prefix of @full@.
-- Uses 'equalFilePath' so the comparison respects platform conventions
-- (case-insensitive on Windows, trailing-separator-insensitive, etc.).
-- This avoids the @\"\/sandbox\"@ vs @\"\/sandbox2\"@ string-prefix
-- foot-gun of 'Data.List.isPrefixOf'.
stripPathPrefix :: [FilePath] -> [FilePath] -> Maybe [FilePath]
stripPathPrefix [] ys = Just ys
stripPathPrefix _ [] = Nothing
stripPathPrefix (p : ps) (y : ys)
  | equalFilePath p y = stripPathPrefix ps ys
  | otherwise = Nothing

-- | Walk every path component of @target@ that lies beneath @base@ and
-- throw 'SandboxViolation' if any component is a symbolic link.
-- @baseJoined@ is the canonical base directory (without trailing
-- separator); @tailComps@ are the components below it that must be
-- traversed.
verifyNoSymlinksInChain :: FilePath -> FilePath -> [FilePath] -> IO ()
verifyNoSymlinksInChain base = walk
  where
    walk _ [] = pure ()
    walk acc (c : cs) = do
      let next = acc </> c
      exists <- doesPathExist next
      when exists $ do
        sym <- isSymlink next
        when sym $ throwIO (SandboxViolation next base)
      walk next cs

-- | Canonicalize the longest existing prefix of a path, then append
-- any missing trailing components. This ensures that even when the
-- final target doesn't exist yet, symlinks in the parent chain are
-- resolved before the containment check.
--
-- Uses 'splitFileName' to peel the trailing component instead of a
-- character-level @drop (length parent)@, which is brittle across
-- trailing-separator and platform-separator variations.
canonicalizeExisting :: FilePath -> IO FilePath
canonicalizeExisting path = do
  exists <- doesPathExist path
  if exists
    then canonicalizePath path
    else do
      let (parentRaw, name) = splitFileName path
          parent = dropTrailingPathSeparator parentRaw
      if null name || equalFilePath parent path
        then pure path
        else do
          parentCanon <- canonicalizeExisting parent
          pure (parentCanon </> name)

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
