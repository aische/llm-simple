module LLM.FsToolsSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, bracket_, evaluate, try)
import Control.Monad (forM, forM_)
import Data.Aeson (Value, object, (.=))
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import GHC.IO.Encoding (getLocaleEncoding, setLocaleEncoding)
import Heptapod (generate)
import LLM.Agent.ToolUtils (executeTool)
import LLM.Agent.Types (RuntimeArgs (..), Tool (..), ToolContext (..))
import LLM.Core.Types (ToolCall (..), ToolResult (..))
import LLM.Generate.GenerateUtils (llmHooks)
import LLM.Generate.Logger (noHooks)
import LLM.Load.FsTools (fsTools)
import LLM.Tools.FsConfig (SandboxViolation (..))
import LLM.Tools.FsLimits (maxOutputBytes, maxTreeEntries, maxVisitedEntries)
import System.Directory
  ( createDirectory,
    createDirectoryLink,
    createFileLink,
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    pathIsSymbolicLink,
  )
import System.FilePath (takeDirectory, (</>))
import System.IO (TextEncoding, char8)
import System.IO.Error (ioeGetFileName)
import GHC.Clock (getMonotonicTime)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
  ( Spec,
    aroundAll,
    describe,
    expectationFailure,
    it,
    shouldBe,
    shouldContain,
    shouldNotContain,
    shouldReturn,
    shouldSatisfy,
  )

spec :: Spec
spec = describe "FsTools" $ do
  describe "fsTools registration" $ do
    it "registers the expected tool names" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      Map.keys tm
        `shouldBe`
          [ "copy_file",
            "create_directory",
            "directory_tree",
            "file_info",
            "find_files",
            "grep",
            "move_file",
            "multi_replace_in_file",
            "read_file_paginated",
            "readdir",
            "remove_directory",
            "remove_file",
            "replace_in_file",
            "writefile"
          ]

  describe "writefile / read_file_paginated" $ do
    it "writes and reads back text" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      _ <- runTool tm "writefile" (object ["path" .= ("notes.txt" :: Text), "content" .= ("line1\nline2\n" :: Text)])
      result <- runTool tm "read_file_paginated" (object ["path" .= ("notes.txt" :: Text), "offset" .= (1 :: Int), "limit" .= (10 :: Int)])
      T.unpack result `shouldContain` "line1"
      T.unpack result `shouldContain` "line2"

  describe "replace_in_file" $ do
    it "replaces a unique occurrence" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      runTool_ tm "writefile" (object ["path" .= ("code.hs" :: Text), "content" .= ("foo = 1\n" :: Text)])
      result <-
        runTool
          tm
          "replace_in_file"
          (object ["path" .= ("code.hs" :: Text), "old" .= ("foo = 1" :: Text), "new" .= ("foo = 2" :: Text)])
      T.unpack result `shouldContain` "Successfully replaced"
      readBack <- runTool tm "read_file_paginated" (object ["path" .= ("code.hs" :: Text)])
      T.unpack readBack `shouldContain` "foo = 2"

    it "lands every edit when many run on one file at once" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      -- A large file makes each edit slow enough that the threads would
      -- interleave between read and write without the per-path lock.
      let n = 40 :: Int
          tok i = "<tok " <> T.pack (show i) <> ">"
          done i = "<done " <> T.pack (show i) <> ">"
          padding = T.replicate 95000 "0123456789"
      runTool_ tm "writefile" (object ["path" .= ("big.txt" :: Text), "content" .= (padding <> T.concat (map tok [1 .. n]))])
      dones <- forM [1 .. n] $ \i -> do
        mv <- newEmptyMVar
        _ <- forkIO $ do
          r <- try (runTool tm "replace_in_file" (object ["path" .= ("big.txt" :: Text), "old" .= tok i, "new" .= done i]))
          putMVar mv (r :: Either IOException Text)
        pure mv
      results <- mapM takeMVar dones
      forM_ results $ \r -> r `shouldSatisfy` either (const False) ("Successfully" `T.isPrefixOf`)
      final <- BS.readFile (sb </> "big.txt")
      forM_ [1 .. n] $ \i -> encodeUtf8 (done i) `shouldSatisfy` (`BS.isInfixOf` final)

  describe "grep" $ do
    it "finds a literal substring" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      runTool_ tm "writefile" (object ["path" .= ("src/main.hs" :: Text), "content" .= ("main = putStrLn \"hello\"\n" :: Text)])
      result <-
        runTool
          tm
          "grep"
          ( object
              [ "pattern" .= ("putStrLn" :: Text),
                "path" .= ("." :: Text),
                "extensions" .= (["hs"] :: [Text])
              ]
          )
      T.unpack result `shouldContain` "main.hs"

    it "shows paths relative to the search root, never host paths" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      runTool_ tm "writefile" (object ["path" .= ("src/app/main.hs" :: Text), "content" .= ("needle\n" :: Text)])
      fromRoot <- runTool tm "grep" (object ["pattern" .= ("needle" :: Text)])
      T.lines fromRoot `shouldContain` [T.pack ("src" </> "app" </> "main.hs") <> ":1: needle"]
      fromSub <- runTool tm "grep" (object ["pattern" .= ("needle" :: Text), "path" .= ("src" :: Text)])
      T.lines fromSub `shouldContain` [T.pack ("app" </> "main.hs") <> ":1: needle"]
      oneFile <- runTool tm "grep" (object ["pattern" .= ("needle" :: Text), "path" .= ("src/app/main.hs" :: Text)])
      T.lines oneFile `shouldContain` ["main.hs:1: needle"]
      T.unpack (fromRoot <> fromSub <> oneFile) `shouldNotContain` sb

  describe "find_files" $ do
    it "matches a glob pattern" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      runTool_ tm "writefile" (object ["path" .= ("src/A.hs" :: Text), "content" .= ("x" :: Text)])
      runTool_ tm "writefile" (object ["path" .= ("src/B.txt" :: Text), "content" .= ("y" :: Text)])
      result <- runTool tm "find_files" (object ["pattern" .= ("**/*.hs" :: Text), "path" .= ("." :: Text)])
      T.unpack result `shouldContain` "src/A.hs"
      T.unpack result `shouldNotContain` "B.txt"

  describe "readdir" $ do
    it "lists directory entries" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      runTool_ tm "writefile" (object ["path" .= ("visible.txt" :: Text), "content" .= ("x" :: Text)])
      result <- runTool tm "readdir" (object ["path" .= ("." :: Text)])
      T.unpack result `shouldContain` "visible.txt"

  describe "directory_tree" $ do
    it "renders a tree under the workspace" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      runTool_ tm "create_directory" (object ["path" .= ("pkg" :: Text)])
      runTool_ tm "writefile" (object ["path" .= ("pkg/mod.hs" :: Text), "content" .= ("x" :: Text)])
      result <- runTool tm "directory_tree" (object ["path" .= ("." :: Text)])
      T.unpack result `shouldContain` "pkg"
      T.unpack result `shouldContain` "mod.hs"

  describe "file_info" $ do
    it "reports file metadata" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      runTool_ tm "writefile" (object ["path" .= ("data.txt" :: Text), "content" .= ("abc\n" :: Text)])
      result <- runTool tm "file_info" (object ["path" .= ("data.txt" :: Text)])
      T.unpack result `shouldContain` "kind: text"

  describe "copy_file / move_file" $ do
    it "copies then moves a file" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      runTool_ tm "writefile" (object ["path" .= ("orig.txt" :: Text), "content" .= ("payload" :: Text)])
      runTool_ tm "copy_file" (object ["source" .= ("orig.txt" :: Text), "destination" .= ("copy.txt" :: Text)])
      runTool_ tm "move_file" (object ["source" .= ("copy.txt" :: Text), "destination" .= ("moved.txt" :: Text)])
      doesFileExist (sb </> "moved.txt") `shouldReturn` True
      doesFileExist (sb </> "copy.txt") `shouldReturn` False

  describe "multi_replace_in_file" $ do
    it "applies ordered replacements" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      runTool_ tm "writefile" (object ["path" .= ("multi.txt" :: Text), "content" .= ("aaa bbb\n" :: Text)])
      result <-
        runTool
          tm
          "multi_replace_in_file"
          ( object
              [ "path" .= ("multi.txt" :: Text),
                "replacements"
                  .= [ object ["old" .= ("aaa" :: Text), "new" .= ("AAA" :: Text)],
                       object ["old" .= ("bbb" :: Text), "new" .= ("BBB" :: Text)]
                     ]
              ]
          )
      T.unpack result `shouldContain` "Successfully applied 2"
      readBack <- runTool tm "read_file_paginated" (object ["path" .= ("multi.txt" :: Text)])
      T.unpack readBack `shouldContain` "AAA BBB"

  describe "create_directory" $ do
    it "creates nested directories" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      result <- runTool tm "create_directory" (object ["path" .= ("a/b/c" :: Text), "parents" .= True])
      T.unpack result `shouldContain` "Successfully created"
      doesDirectoryExist (sb </> "a" </> "b" </> "c") `shouldReturn` True

  describe "remove_file / remove_directory" $ do
    it "removes files and directories" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      runTool_ tm "create_directory" (object ["path" .= ("tmp" :: Text)])
      runTool_ tm "writefile" (object ["path" .= ("tmp/x.txt" :: Text), "content" .= ("z" :: Text)])
      runTool_ tm "remove_file" (object ["path" .= ("tmp/x.txt" :: Text)])
      doesFileExist (sb </> "tmp" </> "x.txt") `shouldReturn` False
      runTool_ tm "remove_directory" (object ["path" .= ("tmp" :: Text)])
      doesDirectoryExist (sb </> "tmp") `shouldReturn` False

  describe "tool errors" $ do
    it "name the file relative to the workspace in the raised IOException" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      createDirectory (sb </> "sub")
      r <- try (runTool tm "remove_file" (object ["path" .= ("sub/missing.txt" :: Text)]))
      case r of
        Left (e :: IOException) -> ioeGetFileName e `shouldBe` Just ("sub" </> "missing.txt")
        Right out -> expectationFailure ("expected an IOException, got " <> T.unpack out)

    it "do not show the workspace's host path to the model" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      ctx <- mkToolContext
      let call = ToolCall {tcId = "1", tcName = "remove_file", tcArguments = object ["path" .= ("missing.txt" :: Text)], tcProviderMeta = Nothing}
      res <- executeTool noHooks ctx (Map.elems tm) call
      let out = T.unpack res.trContent
      out `shouldContain` "missing.txt"
      out `shouldContain` "does not exist"
      out `shouldNotContain` "llm-simple-fstools"

  describe "symbolic links" $ do
    it "remove_file removes the link and leaves its target intact" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      writeFile (sb </> "real.txt") "keep"
      createFileLink "real.txt" (sb </> "link")
      result <- runTool tm "remove_file" (object ["path" .= ("link" :: Text)])
      T.unpack result `shouldContain` "symbolic link"
      doesPathExist (sb </> "link") `shouldReturn` False
      readFile (sb </> "real.txt") `shouldReturn` "keep"

    it "remove_file removes a link that points outside the workspace" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      let outside = takeDirectory sb </> "outside.txt"
      writeFile outside "keep"
      createFileLink outside (sb </> "link")
      runTool_ tm "remove_file" (object ["path" .= ("link" :: Text)])
      doesPathExist (sb </> "link") `shouldReturn` False
      readFile outside `shouldReturn` "keep"

    it "remove_directory removes the link and leaves the linked directory intact" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      createDirectory (sb </> "real")
      writeFile (sb </> "real" </> "x.txt") "keep"
      createDirectoryLink "real" (sb </> "link")
      result <- runTool tm "remove_directory" (object ["path" .= ("link" :: Text)])
      T.unpack result `shouldContain` "symbolic link"
      doesPathExist (sb </> "link") `shouldReturn` False
      readFile (sb </> "real" </> "x.txt") `shouldReturn` "keep"

    it "move_file refuses a link source" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      writeFile (sb </> "real.txt") "keep"
      createFileLink "real.txt" (sb </> "link")
      result <- runTool tm "move_file" (object ["source" .= ("link" :: Text), "destination" .= ("moved.txt" :: Text)])
      T.unpack result `shouldContain` "symbolic link"
      pathIsSymbolicLink (sb </> "link") `shouldReturn` True
      readFile (sb </> "real.txt") `shouldReturn` "keep"
      doesPathExist (sb </> "moved.txt") `shouldReturn` False

    it "writefile refuses a dangling link pointing outside the workspace" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      -- The link target does not exist yet, so the link looks absent to
      -- every check that stats through it; the write must still not
      -- create it. The agent loop turns the violation into a model-facing
      -- message, so here it surfaces as the exception itself.
      let outside = takeDirectory sb </> "outside.txt"
      createFileLink outside (sb </> "dangling")
      r <- try (runTool tm "writefile" (object ["path" .= ("dangling" :: Text), "content" .= ("pwned" :: Text)]))
      case r of
        Left (SandboxViolation _ _) -> pure ()
        Right _ -> expectationFailure "expected SandboxViolation but writefile succeeded"
      doesPathExist outside `shouldReturn` False

    it "copy_file refuses a link source" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      writeFile (sb </> "real.txt") "keep"
      createFileLink "real.txt" (sb </> "link")
      result <- runTool tm "copy_file" (object ["source" .= ("link" :: Text), "destination" .= ("copy.txt" :: Text)])
      T.unpack result `shouldContain` "symbolic link"
      doesPathExist (sb </> "copy.txt") `shouldReturn` False

  describe "text encoding" $ do
    -- "café foo\n" in Latin-1: 0xE9 is not valid UTF-8.
    let latin1 = BS.pack [0x63, 0x61, 0x66, 0xE9, 0x20, 0x66, 0x6F, 0x6F, 0x0A]
    it "replace_in_file refuses a non-UTF-8 file and leaves its bytes unchanged" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      BS.writeFile (sb </> "latin1.txt") latin1
      result <- runTool tm "replace_in_file" (object ["path" .= ("latin1.txt" :: Text), "old" .= ("foo" :: Text), "new" .= ("bar" :: Text)])
      T.unpack result `shouldContain` "UTF-8"
      BS.readFile (sb </> "latin1.txt") `shouldReturn` latin1

    it "multi_replace_in_file refuses a non-UTF-8 file and leaves its bytes unchanged" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      BS.writeFile (sb </> "latin1.txt") latin1
      result <-
        runTool
          tm
          "multi_replace_in_file"
          ( object
              [ "path" .= ("latin1.txt" :: Text),
                "replacements" .= [object ["old" .= ("foo" :: Text), "new" .= ("bar" :: Text)]]
              ]
          )
      T.unpack result `shouldContain` "UTF-8"
      BS.readFile (sb </> "latin1.txt") `shouldReturn` latin1

    it "writefile writes UTF-8 regardless of the locale" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      withLocaleEncoding char8 $
        runTool_ tm "writefile" (object ["path" .= ("u.txt" :: Text), "content" .= ("äöü" :: Text)])
      BS.readFile (sb </> "u.txt") `shouldReturn` encodeUtf8 "äöü"

    it "replace_in_file writes UTF-8 regardless of the locale" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      BS.writeFile (sb </> "u.txt") (encodeUtf8 "x = 1\n")
      withLocaleEncoding char8 $
        runTool_ tm "replace_in_file" (object ["path" .= ("u.txt" :: Text), "old" .= ("1" :: Text), "new" .= ("äöü" :: Text)])
      BS.readFile (sb </> "u.txt") `shouldReturn` encodeUtf8 "x = äöü\n"

    it "grep finds matches in a non-UTF-8 file" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      BS.writeFile (sb </> "latin1.txt") latin1
      result <- runTool tm "grep" (object ["pattern" .= ("foo" :: Text), "path" .= ("." :: Text)])
      T.unpack result `shouldContain` "latin1.txt"

    it "read_file_paginated reads a non-UTF-8 file leniently" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      BS.writeFile (sb </> "latin1.txt") latin1
      result <- runTool tm "read_file_paginated" (object ["path" .= ("latin1.txt" :: Text)])
      T.unpack result `shouldContain` "caf\xFFFD foo"

  describe "file tool limits" $ do
    it "grep refuses an explicit file over the size cap" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      BS.writeFile (sb </> "big.txt") (BS.replicate (1024 * 1024) 0x61 <> "needle\n")
      result <- runTool tm "grep" (object ["pattern" .= ("needle" :: Text), "path" .= ("big.txt" :: Text)])
      T.unpack result `shouldContain` "Error"
      T.unpack result `shouldContain` "exceeds"

    it "find_files matches '*' and '?' within a segment" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      mapM_ (\f -> BS.writeFile (sb </> f) "x") ["abc.hs", "a.hs", "abxc.hs", "ab.hs", "b.hs"]
      result <- runTool tm "find_files" (object ["pattern" .= ("a*b?c.hs" :: Text)])
      T.lines result `shouldContain` ["abxc.hs"]
      T.unpack result `shouldNotContain` "abc.hs"
      result2 <- runTool tm "find_files" (object ["pattern" .= ("*a*.hs" :: Text)])
      filter (not . T.isPrefixOf "===") (T.lines result2) `shouldBe` ["a.hs", "ab.hs", "abc.hs", "abxc.hs"]

    it "find_files does not backtrack exponentially" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      -- The old backtracking matcher took ~5 s on this; its loop does not
      -- allocate, so 'timeout' could not interrupt it. Measure instead.
      BS.writeFile (sb </> replicate 40 'a') "x"
      t0 <- getMonotonicTime
      result <- runTool tm "find_files" (object ["pattern" .= ("*a*a*a*a*a*a*a*a*b" :: Text)])
      _ <- evaluate (T.length result)
      t1 <- getMonotonicTime
      T.unpack result `shouldContain` "0 match(es)"
      (t1 - t0) `shouldSatisfy` (< 1)

  describe "work bounds" $ aroundAll withBigTree $ do
    it "find_files stops at the entry budget when nothing matches" $ \sb -> do
      tm <- fsTools sb
      result <- runTool tm "find_files" (object ["pattern" .= ("**/*.nomatch" :: Text)])
      T.unpack result `shouldContain` "0 match(es)"
      T.unpack result `shouldContain` ("stopped after visiting " <> show maxVisitedEntries <> " entries")

    it "grep stops at the entry budget when nothing matches" $ \sb -> do
      tm <- fsTools sb
      result <- runTool tm "grep" (object ["pattern" .= ("needle" :: Text)])
      T.unpack result `shouldContain` "0 match(es)"
      T.unpack result `shouldContain` ("stopped after visiting " <> show maxVisitedEntries <> " entries")

    it "directory_tree stops at the entry budget" $ \sb -> do
      tm <- fsTools sb
      result <- runTool tm "directory_tree" (object ["path" .= ("." :: Text)])
      T.unpack result `shouldContain` ("stopped after visiting " <> show maxVisitedEntries <> " entries")

    it "readdir lists at most maxTreeEntries entries and says how many there are" $ \sb -> do
      tm <- fsTools sb
      result <- runTool tm "readdir" (object ["path" .= ("." :: Text)])
      length (T.lines result) `shouldBe` maxTreeEntries + 1
      last (T.lines result) `shouldBe` "[showing 2000 of 20001 entries; use find_files to search the rest]"

  describe "work bounds (small trees)" $ do
    it "grep stops at maxResults and reports a lower bound on files" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      mapM_ (\i -> BS.writeFile (sb </> ("f" <> show i <> ".txt")) "needle\n") [1 .. 5 :: Int]
      result <- runTool tm "grep" (object ["pattern" .= ("needle" :: Text), "maxResults" .= (2 :: Int)])
      T.unpack result `shouldContain` "2 match(es) across 2+ file(s)"
      T.unpack result `shouldContain` "stopped at maxResults"

    it "grep reports an exact file count when it searched everything" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      mapM_ (\i -> BS.writeFile (sb </> ("f" <> show i <> ".txt")) "needle\n") [1 .. 3 :: Int]
      result <- runTool tm "grep" (object ["pattern" .= ("needle" :: Text)])
      T.unpack result `shouldContain` "3 match(es) across 3 file(s) ==="

    it "find_files cuts output at maxOutputBytes" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      -- 1500 names of ~100 bytes is ~150 KB of output, over the cap.
      let name i = replicate 90 'x' <> show (i :: Int) <> ".txt"
      mapM_ (\i -> BS.writeFile (sb </> name i) "") [1000 .. 2499]
      result <- runTool tm "find_files" (object ["pattern" .= ("*.txt" :: Text), "maxResults" .= (2000 :: Int)])
      BS.length (encodeUtf8 result) `shouldSatisfy` (< maxOutputBytes + 1000)
      T.unpack result `shouldContain` "output truncated at"

    it "read_file_paginated cuts a line longer than a page and marks it" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      BS.writeFile (sb </> "long.txt") ("short\n" <> BS.replicate (maxOutputBytes * 3) 0x61 <> "\nlast\n")
      result <- runTool tm "read_file_paginated" (object ["path" .= ("long.txt" :: Text), "offset" .= (2 :: Int)])
      BS.length (encodeUtf8 result) `shouldSatisfy` (< maxOutputBytes + 1000)
      T.unpack result `shouldContain` ("[line cut: " <> show (maxOutputBytes * 3) <> " bytes")
      T.unpack result `shouldContain` "next offset: 3"

    it "read_file_paginated ends a page before the byte cap" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      let line = BS.replicate (maxOutputBytes `div` 2 + 10) 0x62
      BS.writeFile (sb </> "two.txt") (line <> "\n" <> line <> "\n")
      result <- runTool tm "read_file_paginated" (object ["path" .= ("two.txt" :: Text)])
      T.unpack result `shouldContain` "lines 1-1"
      T.unpack result `shouldContain` "next offset: 2"
      T.unpack result `shouldNotContain` "line cut"

    it "read_file_paginated does not split a UTF-8 character when cutting" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      -- "ä" is two bytes; an odd prefix byte puts the cap inside one.
      BS.writeFile (sb </> "u.txt") ("x" <> encodeUtf8 (T.replicate maxOutputBytes "ä"))
      result <- runTool tm "read_file_paginated" (object ["path" .= ("u.txt" :: Text)])
      T.unpack result `shouldNotContain` "\xFFFD"

    it "read_file_paginated skips lines before the offset" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      BS.writeFile (sb </> "n.txt") (BS.concat [encodeUtf8 (T.pack ("l" <> show i <> "\n")) | i <- [1 .. 10 :: Int]])
      result <- runTool tm "read_file_paginated" (object ["path" .= ("n.txt" :: Text), "offset" .= (9 :: Int)])
      filter (not . T.isPrefixOf "===") (T.lines result) `shouldBe` ["     9| l9", "    10| l10"]
      T.unpack result `shouldContain` "end of file"

  describe "workspace root guard" $ do
    let rootPaths = [".", "", "./", "sub/.."] :: [Text]
    it "remove_directory refuses the workspace root" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      runTool_ tm "create_directory" (object ["path" .= ("sub" :: Text)])
      runTool_ tm "writefile" (object ["path" .= ("keep.txt" :: Text), "content" .= ("k" :: Text)])
      mapM_
        ( \p -> do
            result <- runTool tm "remove_directory" (object ["path" .= p])
            T.unpack result `shouldContain` "Error"
            T.unpack result `shouldContain` "workspace root"
        )
        rootPaths
      doesFileExist (sb </> "keep.txt") `shouldReturn` True
      doesDirectoryExist (sb </> "sub") `shouldReturn` True

    it "remove_file refuses the workspace root" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      runTool_ tm "create_directory" (object ["path" .= ("sub" :: Text)])
      mapM_
        ( \p -> do
            result <- runTool tm "remove_file" (object ["path" .= p])
            T.unpack result `shouldContain` "workspace root"
        )
        rootPaths
      doesDirectoryExist sb `shouldReturn` True

    it "move_file refuses the workspace root as source" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      runTool_ tm "create_directory" (object ["path" .= ("sub" :: Text)])
      mapM_
        ( \p -> do
            result <- runTool tm "move_file" (object ["source" .= p, "destination" .= ("sub/moved" :: Text)])
            T.unpack result `shouldContain` "workspace root"
        )
        rootPaths
      doesDirectoryExist (sb </> "sub") `shouldReturn` True

    it "remove_file reports a clear error for a directory" $ withSandbox $ \sb -> do
      tm <- fsTools sb
      runTool_ tm "create_directory" (object ["path" .= ("sub" :: Text)])
      result <- runTool tm "remove_file" (object ["path" .= ("sub" :: Text)])
      T.unpack result `shouldContain` "is a directory"
      doesDirectoryExist (sb </> "sub") `shouldReturn` True

withLocaleEncoding :: TextEncoding -> IO a -> IO a
withLocaleEncoding enc act = do
  old <- getLocaleEncoding
  bracket_ (setLocaleEncoding enc) (setLocaleEncoding old) act

-- | A sandbox with one more entry than 'maxVisitedEntries', none matching.
withBigTree :: (FilePath -> IO ()) -> IO ()
withBigTree act = withSandbox $ \sb -> do
  mapM_ (\i -> BS.writeFile (sb </> ("e" <> show i)) "") [0 .. maxVisitedEntries]
  act sb

withSandbox :: (FilePath -> IO a) -> IO a
withSandbox act = withSystemTempDirectory "llm-simple-fstools" $ \root -> do
  let sb = root </> "sandbox"
  createDirectory sb
  act sb

runTool :: Map.Map Text (Tool Text) -> Text -> Value -> IO Text
runTool tm name args = do
  ctx <- mkToolContext
  case Map.lookup name tm of
    Nothing -> error ("tool not found: " <> T.unpack name)
    Just tool -> tool.toolExecute ctx args

runTool_ :: Map.Map Text (Tool Text) -> Text -> Value -> IO ()
runTool_ tm name args = runTool tm name args >> pure ()

mkToolContext :: IO ToolContext
mkToolContext = do
  genId <- generate
  pure
    ToolContext
      { tcConversation = [],
        tcUsage = mempty,
        tcWindowOffset = 0,
        tcRuntimeArgs =
          RuntimeArgs
            { rtGenerationId = genId,
              rtAbortSignal = Nothing,
              rtLLMHooks = llmHooks noHooks,
              rtHooks = noHooks,
              rtOnEvent = \_ -> pure (),
              rtReadonly = False
            }
      }
