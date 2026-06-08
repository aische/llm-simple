module LLM.Workflow.Tools.FsTools where

import Control.Monad.IO.Class (MonadIO)
import Data.Map qualified as Map
import LLM.Tools.CopyFile (copyFileToolTyped)
import LLM.Tools.CreateDirectory (createDirectoryToolTyped)
import LLM.Tools.DirectoryTree (directoryTreeToolTyped)
import LLM.Tools.FileInfo (fileInfoToolTyped)
import LLM.Tools.FindFiles (findFilesToolTyped)
import LLM.Tools.FsConfig (mkFsConfig)
import LLM.Tools.Grep (grepToolTyped)
import LLM.Tools.MoveFile (moveFileToolTyped)
import LLM.Tools.MultiReplaceInFile (multiReplaceInFileToolTyped)
import LLM.Tools.ReadFilePaginated (readFilePaginatedToolTyped)
import LLM.Tools.Readdir (readdirToolTyped)
-- import LLM.Tools.Readfile (readfileToolTyped)
import LLM.Tools.RemoveDirectory (removeDirectoryToolTyped)
import LLM.Tools.RemoveFile (removeFileToolTyped)
import LLM.Tools.ReplaceInFile (replaceInFileToolTyped)
import LLM.Tools.Writefile (writefileToolTyped)
import LLM.Workflow.ToolUtils (toTool)
import LLM.Workflow.Types (ToolMap)

fsTools :: (MonadIO m) => FilePath -> IO (ToolMap m)
fsTools filePath = do
  cfg <- mkFsConfig filePath
  pure $
    Map.fromList
      [ ("copy_file", toTool $ copyFileToolTyped cfg),
        ("create_directory", toTool $ createDirectoryToolTyped cfg),
        ("directory_tree", toTool $ directoryTreeToolTyped cfg),
        ("file_info", toTool $ fileInfoToolTyped cfg),
        ("find_files", toTool $ findFilesToolTyped cfg),
        ("grep", toTool $ grepToolTyped cfg),
        ("move_file", toTool $ moveFileToolTyped cfg),
        ("multi_replace_in_file", toTool $ multiReplaceInFileToolTyped cfg),
        ("readdir", toTool $ readdirToolTyped cfg),
        -- ("readfile", toTool $ readfileToolTyped cfg),
        ("read_file_paginated", toTool $ readFilePaginatedToolTyped cfg),
        ("remove_directory", toTool $ removeDirectoryToolTyped cfg),
        ("remove_file", toTool $ removeFileToolTyped cfg),
        ("replace_in_file", toTool $ replaceInFileToolTyped cfg),
        ("writefile", toTool $ writefileToolTyped cfg)
      ]
