{-# LANGUAGE CPP #-}

module LLM.FsLimitsSpec (spec) where

import Control.Exception (ErrorCall (..), throwIO, try)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (mkFsConfig)
import LLM.Tools.FsLimits
  ( atomicWriteWith,
    maxPaginatedFileBytes,
    maxReadBytes,
    readBoundedTextFile,
    writeUtf8File,
  )
import LLM.Tools.ReadFilePaginated
  ( ReadFilePaginatedArgs (..),
    readFilePaginatedToolTyped,
  )
import System.Directory (createDirectory, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
#if !defined(mingw32_HOST_OS)
import System.Posix.Files (fileMode, getFileStatus, setFileMode)
import System.Posix.Types (FileMode)
#endif
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
    shouldContain,
    shouldReturn,
    shouldSatisfy,
  )

spec :: Spec
spec = describe "FsLimits" $ do
  describe "readBoundedTextFile" $ do
    it "reads a file within the cap" $ withTempFile $ \dir -> do
      let path = dir </> "small.txt"
      writeFile path "hello"
      result <- readBoundedTextFile path "test.txt" maxReadBytes
      result `shouldBe` Right "hello"

    it "rejects a file larger than the cap" $ withTempFile $ \dir -> do
      let path = dir </> "big.txt"
      BS.writeFile path (BS.replicate (maxReadBytes + 1) 0x61)
      result <- readBoundedTextFile path "big.txt" maxReadBytes
      result `shouldSatisfy` \case
        Left err -> "exceeds the" `T.isInfixOf` err
        Right _ -> False

  describe "atomicWriteWith" $ do
    it "leaves the original unchanged and no temp file when the action throws" $ withTempFile $ \dir -> do
      let path = dir </> "f.txt"
      BS.writeFile path "original"
      r <- try $ atomicWriteWith path $ \h -> do
        BS.hPut h "partial"
        throwIO (ErrorCall "boom")
      r `shouldBe` Left (ErrorCall "boom")
      BS.readFile path `shouldReturn` "original"
      listDirectory dir `shouldReturn` ["f.txt"]

    it "replaces the content and leaves no temp file" $ withTempFile $ \dir -> do
      let path = dir </> "f.txt"
      BS.writeFile path "original"
      writeUtf8File path "new"
      BS.readFile path `shouldReturn` "new"
      listDirectory dir `shouldReturn` ["f.txt"]

#if !defined(mingw32_HOST_OS)
    it "keeps the exact mode of an existing 0644 file" $ withTempFile $ \dir ->
      modeAfterWrite dir 0o644 `shouldReturn` 0o644

    it "keeps the exact mode of an existing 0755 file" $ withTempFile $ \dir ->
      modeAfterWrite dir 0o755 `shouldReturn` 0o755

    it "gives a new file the default mode, not 0600" $ withTempFile $ \dir -> do
      -- A file created the ordinary way gets 0666 minus the umask.
      BS.writeFile (dir </> "plain.txt") ""
      expected <- modeOf (dir </> "plain.txt")
      writeUtf8File (dir </> "new.txt") "x"
      modeOf (dir </> "new.txt") `shouldReturn` expected
#endif

  describe "read_file_paginated" $ do
    it "rejects source files larger than maxPaginatedFileBytes before reading" $
      withSandbox $ \sb -> do
        cfg <- mkFsConfig sb
        let path = sb </> "huge.txt"
        BS.writeFile path (BS.replicate (fromIntegral maxPaginatedFileBytes + 1) 0x61)
        let TypedTool {ttoolExecute = exec} = readFilePaginatedToolTyped cfg
        result <-
          exec
            ()
            ReadFilePaginatedArgs
              { _rfpPath = "huge.txt",
                _rfpOffset = 1,
                _rfpLimit = 10
              }
        T.unpack result `shouldContain` "exceeds the"

withTempFile :: (FilePath -> IO a) -> IO a
withTempFile = withSystemTempDirectory "llm-simple-fslimits"

withSandbox :: (FilePath -> IO a) -> IO a
withSandbox act = withSystemTempDirectory "llm-simple-fslimits" $ \root -> do
  let sb = root </> "sandbox"
  createDirectory sb
  act sb

#if !defined(mingw32_HOST_OS)
modeOf :: FilePath -> IO FileMode
modeOf p = (`mod` 0o1000) . fileMode <$> getFileStatus p

modeAfterWrite :: FilePath -> FileMode -> IO FileMode
modeAfterWrite dir mode = do
  let path = dir </> "f.txt"
  BS.writeFile path "old"
  setFileMode path mode
  writeUtf8File path "new"
  modeOf path
#endif
