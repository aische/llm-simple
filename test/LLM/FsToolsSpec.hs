module LLM.FsToolsSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Heptapod (generate)
import LLM.Agent.Types (RuntimeArgs (..), Tool (..), ToolContext (..))
import LLM.Generate.GenerateUtils (llmHooks)
import LLM.Generate.Logger (noHooks)
import LLM.Load.FsTools (fsTools)
import System.Directory
  ( createDirectory,
    doesDirectoryExist,
    doesFileExist,
  )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
    shouldContain,
    shouldNotContain,
    shouldReturn,
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
