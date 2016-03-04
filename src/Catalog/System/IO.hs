-- all IO operations lifted to commands

module Catalog.System.IO
where

import           Catalog.Cmd.Types
-- import Data.Prim

import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as C
import qualified System.Directory as D
import qualified System.Posix as X

-- ----------------------------------------

type FileStatus = X.FileStatus

fileExist :: FilePath -> Cmd Bool
fileExist = io . D.doesFileExist

dirExist :: FilePath -> Cmd Bool
dirExist = io . D.doesDirectoryExist

getFileStatus :: FilePath -> Cmd FileStatus
getFileStatus = io . X.getFileStatus

writeFileLB :: FilePath -> LB.ByteString -> Cmd ()
writeFileLB f = io . LB.writeFile f

readFileLB :: FilePath -> Cmd LB.ByteString
readFileLB = io . LB.readFile

removeFile :: FilePath -> Cmd ()
removeFile = io . D.removeFile

getWorkingDirectory :: Cmd FilePath
getWorkingDirectory = io X.getWorkingDirectory

readDir :: FilePath -> Cmd [FilePath]
readDir p = io $ do
  s  <- X.openDirStream p
  xs <- readDirEntries s
  X.closeDirStream s
  return xs
  where
    readDirEntries s = do
      e1 <- X.readDirStream s
      if null e1
        then return []
        else do
          es <- readDirEntries s
          return (e1 : es)

putStrLnLB :: LB.ByteString -> Cmd ()
putStrLnLB = io . LB.putStrLn

putStrLn' :: String -> Cmd ()
putStrLn' = io . putStrLn

atThisMoment :: Cmd UTCTime
atThisMoment = io C.getCurrentTime

-- ----------------------------------------
