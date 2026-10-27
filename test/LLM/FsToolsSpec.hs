module LLM.FsToolsSpec (spec) where

import Control.Exception (bracket_, evaluate, try)
import Data.Aeson (Value, object, (.=))
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import GHC.IO.Encoding (getLocaleEncoding, setLocaleEncoding)
import Heptapod (generate)
import LLM.Agent.Types (RuntimeArgs (..), Tool (..), ToolContext (..))
import LLM.Generate.GenerateUtils (llmHooks)
import LLM.Generate.Logger (noHooks)
import LLM.Load.FsTools (fsTools)
import LLM.Tools.FsConfig (SandboxViolation (..))
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
import GHC.Clock (getMonotonicTime)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
  ( Spec,
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
