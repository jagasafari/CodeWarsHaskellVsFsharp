module ExploringIO where

import Control.Monad
import Control.Monad.IO.Class
import System.Directory
import System.FilePath

doesFileExistCurrentDir searchedContent = do 
    currDir <- liftIO getCurrentDirectory
    liftIO $ doesFileExist $ joinPath [currDir, searchedContent]
 
currentDirContentFiltered = 
    liftM filterDirContent currentDirContent
    where
        filterDirContent = filter (not . (`elem` [".", ".."]))
        currentDirContent = getCurrentDirectory >>= getDirectoryContents 

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

