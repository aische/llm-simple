module LLM.FsLimitsSpec (spec) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (mkFsConfig)
import LLM.Tools.FsLimits
  ( maxPaginatedFileBytes,
    maxReadBytes,
    readBoundedTextFile,
  )
import LLM.Tools.ReadFilePaginated
  ( ReadFilePaginatedArgs (..),
    readFilePaginatedToolTyped,
  )
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
    shouldContain,
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
