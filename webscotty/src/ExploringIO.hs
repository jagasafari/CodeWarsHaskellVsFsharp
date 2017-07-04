module ExploringIO where

import System.Directory
import System.FilePath

filterDirContent = 
    filter (not . (`elem` [".", ".."]))

currentDirContents = 
    getCurrentDirectory >>= getDirectoryContents

runWorkflow = 
    workflow >>= \(x, y) -> [x, ":", y, "\n"] where
    createTmpDirLabel = "create tmp dir"
    foundTmpDirLabel = "create tmp dir"
    done = ""
    workflow = 
            [("server dir content",done)
            ,(foundTmpDirLabel,done)
            ,(createTmpDirLabel,done)
            ,(foundTmpDirLabel,done)
            ,("remove tmp dir",done)
            ,(foundTmpDirLabel,done)
            ,(createTmpDirLabel,done)
            ,("createFile",done)
            ,("random numbers append",done)
            ,("display file content",done)
            ,("remove file",done)]

