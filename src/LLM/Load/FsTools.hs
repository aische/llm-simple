module LLM.Load.FsTools where

import Data.Map qualified as Map
-- import LLM.Tools.Readfile (readfileToolTyped)

import Data.Text (Text)
import LLM.Agent.ToolUtils (embedTextTool, toTool)
import LLM.Agent.Types (ToolMap)
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
import LLM.Tools.RemoveDirectory (removeDirectoryToolTyped)
import LLM.Tools.RemoveFile (removeFileToolTyped)
import LLM.Tools.ReplaceInFile (replaceInFileToolTyped)
import LLM.Tools.Writefile (writefileToolTyped)

-- | Build the bundled filesystem 'ToolMap' rooted at @filePath@.
--
-- All paths are resolved inside a canonical sandbox ('LLM.Tools.FsConfig').
-- Mutating tools refuse paths that escape the workspace; reads are bounded
-- (1 MiB for edits, 10 MiB for paginated reads). Include desired names in
-- 'Agent.agTools' to expose them to the model.
fsTools :: FilePath -> IO (ToolMap Text)
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

-- | Like 'fsTools', but embed each tool's 'Text' result via @embed@.
--
-- Use when your 'ToolMap' carries a custom result type (e.g. structured
-- tool outcomes) instead of plain 'Text'.
fsTools' :: (Text -> result) -> FilePath -> IO (ToolMap result)
fsTools' embed filePath = do
  m <- fsTools filePath
  pure $ Map.map (embedTextTool embed) m
