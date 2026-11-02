module LLM.LoggerSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (object, (.=))
import Data.List (isPrefixOf, sort)
import LLM.Generate.Logger (Hooks (..), noHooks, withJsonDump)
import System.Directory (createDirectory, doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "Logger" $ do
  describe "withJsonDump" $ do
    it "keeps a provider name with path separators inside the dump directory" $
      withSystemTempDirectory "llm-simple-dump" $ \root -> do
        let dir = root </> "dumps"
        createDirectory dir
        let hooks = withJsonDump dir noHooks
        hooks.onRequest "../leak" (object ["a" .= (1 :: Int)])
        hooks.onRequest "a/b" (object ["a" .= (1 :: Int)])
        sort <$> listDirectory root >>= (`shouldBe` ["dumps"])
        files <- sort <$> listDirectory dir
        length files `shouldBe` 2
        files `shouldSatisfy` all (\f -> "___leak-request-" `isPrefixOf` f || "a_b-request-" `isPrefixOf` f)
        doesDirectoryExist (dir </> "a") >>= (`shouldBe` False)

    it "writes one file per dump, even within the same millisecond" $
      withSystemTempDirectory "llm-simple-dump" $ \dir -> do
        let hooks = withJsonDump dir noHooks
        forM_ [1 .. 20 :: Int] $ \i -> hooks.onResponse "openai" (object ["i" .= i])
        files <- listDirectory dir
        length files `shouldBe` 20
