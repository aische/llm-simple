-- | Collection of tools for LLM operations
module LLM.Tools
  ( -- * File system tools
    copyFileToolTyped,
    createDirectoryToolTyped,
    directoryTreeToolTyped,
    fileInfoToolTyped,
    findFilesToolTyped,
    grepToolTyped,
    moveFileToolTyped,
    multiReplaceInFileToolTyped,
    readdirToolTyped,
    readfileToolTyped,
    removeDirectoryToolTyped,
    removeFileToolTyped,
    replaceInFileToolTyped,
    writefileToolTyped,

    -- * FsConfig
    mkFsConfig,
    FsConfig (..),
  )
where

import LLM.Tools.CopyFile (copyFileToolTyped)
import LLM.Tools.CreateDirectory (createDirectoryToolTyped)
import LLM.Tools.DirectoryTree (directoryTreeToolTyped)
import LLM.Tools.FileInfo (fileInfoToolTyped)
import LLM.Tools.FindFiles (findFilesToolTyped)
import LLM.Tools.FsConfig (FsConfig (..), mkFsConfig)
import LLM.Tools.Grep (grepToolTyped)
import LLM.Tools.MoveFile (moveFileToolTyped)
import LLM.Tools.MultiReplaceInFile (multiReplaceInFileToolTyped)
import LLM.Tools.Readdir (readdirToolTyped)
import LLM.Tools.Readfile (readfileToolTyped)
import LLM.Tools.RemoveDirectory (removeDirectoryToolTyped)
import LLM.Tools.RemoveFile (removeFileToolTyped)
import LLM.Tools.ReplaceInFile (replaceInFileToolTyped)
import LLM.Tools.Writefile (writefileToolTyped)
