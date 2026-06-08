module LLM.FsConfigSpec (spec) where

import Control.Exception (try)
import LLM.Tools.FsConfig
  ( SandboxViolation (..),
    mkFsConfig,
    sandboxPath,
    sandboxWritePath,
  )
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    createDirectoryLink,
    createFileLink,
    doesFileExist,
  )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
  ( Expectation,
    Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
    shouldSatisfy,
  )

-- | Run an IO action expected to throw 'SandboxViolation'.
shouldViolate :: IO a -> Expectation
shouldViolate act = do
  r <- try act
  case r of
    Left (SandboxViolation _ _) -> pure ()
    Right _ -> expectationFailure "expected SandboxViolation but action succeeded"

-- | Run an IO action with a fresh sandbox directory.
-- The directory itself is the sandbox root (canonical path).
withSandbox :: (FilePath -> IO a) -> IO a
withSandbox act = withSystemTempDirectory "llm-simple-fsconfig" $ \root -> do
  let sb = root </> "sandbox"
  createDirectory sb
  act sb

spec :: Spec
spec = describe "FsConfig sandbox" $ do
  describe "sandboxPath: relative paths" $ do
    it "resolves a plain relative path inside the sandbox" $ withSandbox $ \sb -> do
      cfg <- mkFsConfig sb
      writeFile (sb </> "a.txt") "hello"
      resolved <- sandboxPath cfg "a.txt"
      resolved `shouldBe` (sb </> "a.txt")

    it "rejects '..' that escapes the sandbox" $ withSandbox $ \sb -> do
      cfg <- mkFsConfig sb
      shouldViolate (sandboxPath cfg "../outside.txt")

    it "rejects deep '..' that escapes via existing parents" $ withSandbox $ \sb -> do
      cfg <- mkFsConfig sb
      createDirectory (sb </> "sub")
      shouldViolate (sandboxPath cfg "sub/../../escape.txt")

    it "allows '..' that stays inside the sandbox" $ withSandbox $ \sb -> do
      cfg <- mkFsConfig sb
      createDirectory (sb </> "sub")
      resolved <- sandboxPath cfg "sub/../sub"
      resolved `shouldBe` (sb </> "sub")

  describe "sandboxPath: absolute paths" $ do
    it "accepts an absolute path inside the sandbox" $ withSandbox $ \sb -> do
      cfg <- mkFsConfig sb
      writeFile (sb </> "f.txt") "x"
      resolved <- sandboxPath cfg (sb </> "f.txt")
      resolved `shouldBe` (sb </> "f.txt")

    it "rejects an absolute path outside the sandbox" $ withSandbox $ \sb -> do
      cfg <- mkFsConfig sb
      shouldViolate (sandboxPath cfg "/etc/passwd")

    it "rejects a similarly-named sibling directory (no string-prefix foot-gun)" $
      withSystemTempDirectory "llm-simple-fsconfig" $ \root -> do
        let sb = root </> "sandbox"
            sibling = root </> "sandbox2"
        createDirectory sb
        createDirectory sibling
        writeFile (sibling </> "secret.txt") "x"
        cfg <- mkFsConfig sb
        shouldViolate (sandboxPath cfg (sibling </> "secret.txt"))

  describe "sandboxPath: symlinks" $ do
    it "rejects a symlink in the path chain pointing outside the sandbox" $
      withSystemTempDirectory "llm-simple-fsconfig" $ \root -> do
        let sb = root </> "sandbox"
            outside = root </> "outside"
        createDirectory sb
        createDirectory outside
        writeFile (outside </> "secret.txt") "secret"
        createDirectoryLink outside (sb </> "link")
        cfg <- mkFsConfig sb
        shouldViolate (sandboxPath cfg "link/secret.txt")

    it "rejects a final-component symlink pointing outside" $
      withSystemTempDirectory "llm-simple-fsconfig" $ \root -> do
        let sb = root </> "sandbox"
            outside = root </> "outside"
        createDirectory sb
        createDirectory outside
        writeFile (outside </> "target.txt") "x"
        createFileLink (outside </> "target.txt") (sb </> "alias")
        cfg <- mkFsConfig sb
        shouldViolate (sandboxPath cfg "alias")

    it "rejects writes through a symlinked parent pointing outside" $
      withSystemTempDirectory "llm-simple-fsconfig" $ \root -> do
        let sb = root </> "sandbox"
            outside = root </> "outside"
        createDirectory sb
        createDirectory outside
        createDirectoryLink outside (sb </> "link")
        cfg <- mkFsConfig sb
        shouldViolate (sandboxWritePath cfg "link/new.txt")
        doesFileExist (outside </> "new.txt") >>= (`shouldBe` False)

    it "allows a symlink pointing to a path inside the sandbox" $
      withSandbox $ \sb -> do
        createDirectory (sb </> "real")
        writeFile (sb </> "real" </> "f.txt") "hi"
        createDirectoryLink (sb </> "real") (sb </> "link")
        cfg <- mkFsConfig sb
        -- The link is resolved by 'canonicalizeExisting', the canonical
        -- target lives under the sandbox, and the live-chain walk goes
        -- through the canonical (symlink-free) path. So the access is
        -- permitted and the resolved path points at the real file.
        resolved <- sandboxPath cfg "link/f.txt"
        resolved `shouldBe` (sb </> "real" </> "f.txt")

  describe "sandboxWritePath: non-existent targets" $ do
    it "resolves a write path under a not-yet-existing nested dir" $ withSandbox $ \sb -> do
      cfg <- mkFsConfig sb
      resolved <- sandboxWritePath cfg "a/b/c/new.txt"
      resolved `shouldBe` (sb </> "a" </> "b" </> "c" </> "new.txt")

    it "rejects a write path that would escape via '..' in the tail" $ withSandbox $ \sb -> do
      cfg <- mkFsConfig sb
      createDirectoryIfMissing True (sb </> "a")
      shouldViolate (sandboxWritePath cfg "a/../../evil.txt")

    it "resolved path always starts with the sandbox base" $ withSandbox $ \sb -> do
      cfg <- mkFsConfig sb
      resolved <- sandboxPath cfg "./x/./y"
      resolved `shouldSatisfy` \p -> take (length sb) p == sb
