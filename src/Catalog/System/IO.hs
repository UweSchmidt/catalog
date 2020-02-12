{-# LANGUAGE DeriveFunctor #-}
-- all IO operations lifted to commands

module Catalog.System.IO
  ( SysPath
  , FileStatus
  , fileExist
  , fileNotEmpty
  , dirExist
  , getFileStatus
  , getModiTime
  , getModiTime'
  , setModiTime
  , writeFileLB
  , writeFileT
  , writeFileLT
  , readFileLB
  , readFileT
  , readFileT'
  , removeFile
  , renameFile
  , linkFile
  , createDir
  , removeDir
  , getWorkingDirectory
  , readDir
  , putStrLnLB
  , putStrLn'
  , atThisMoment
  , formatTimeIso8601
  , nowAsIso8601
  )
where

import           Catalog.Cmd.Types

import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Prim.Prelude
import           Data.Prim.SysPath
import           Data.Prim.TimeStamp
import qualified Data.Text.IO      as T
import qualified Data.Text.Lazy.IO as LT
import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock   as C
import qualified Data.Time.Format  as C
import qualified System.Directory  as D
import qualified System.Posix      as X

-- ----------------------------------------

type FileStatus = X.FileStatus

fileExist :: Config r => SysPath -> Action r s Bool
fileExist sp = io . D.doesFileExist $ sp ^. isoFilePath

-- fileExist :: FilePath -> Cmd Bool
-- fileExist = io . D.doesFileExist

dirExist :: Config r => SysPath -> Action r s Bool
dirExist sp = io . D.doesDirectoryExist $ sp ^. isoFilePath

-- check whether a file is there and not empty

fileNotEmpty :: Config r => SysPath -> Action r s Bool
fileNotEmpty sp = do
  ex <- fileExist sp
  if not ex
    then do st <- getFileStatus sp
            return $ X.fileSize st == 0
    else return True

getFileStatus :: Config r => SysPath -> Action r s FileStatus
getFileStatus sp = io . X.getFileStatus $ sp ^. isoFilePath

getModiTime :: Config r => SysPath -> Action r s TimeStamp
getModiTime f = fsTimeStamp <$> getFileStatus f

getModiTime' :: Config r => SysPath -> Action r s TimeStamp
getModiTime' f = do
  ex <- fileExist f
  if ex
    then getModiTime f
    else return mempty

setModiTime :: Config r => TimeStamp -> SysPath -> Action r s ()
setModiTime ts sp =
  io $ X.setFileTimes (sp ^. isoFilePath) ep ep
  where
    ep = ts ^. isoEpochTime

writeFileLB :: Config r => SysPath -> LB.ByteString -> Action r s ()
writeFileLB sp = io . LB.writeFile (sp ^. isoFilePath)

readFileLB :: Config r => SysPath -> Action r s LB.ByteString
readFileLB sp = io . LB.readFile $ sp ^. isoFilePath

readFileT :: Config r => SysPath -> Action r s Text
readFileT sp = io . T.readFile $ sp ^. isoFilePath

readFileT' :: Config r => SysPath -> Action r s Text
readFileT' fp = do
  ex <- fileExist fp
  if ex
    then readFileT fp
    else return mempty

writeFileT :: Config r => SysPath -> Text -> Action r s ()
writeFileT sp = io . T.writeFile (sp ^. isoFilePath)

writeFileLT :: Config r => SysPath -> LazyText -> Action r s ()
writeFileLT sp = io . LT.writeFile (sp ^. isoFilePath)

removeFile :: Config r => SysPath -> Action r s ()
removeFile sp = io . D.removeFile $ sp ^. isoFilePath

renameFile :: Config r => SysPath -> SysPath -> Action r s ()
renameFile old new = io $ X.rename (old ^. isoFilePath) (new ^. isoFilePath)

-- try to make a hard link, if that fails copy file

linkFile :: Config r => SysPath -> SysPath -> Action r s ()
linkFile oldf newf =
  (io $ X.createLink old new)
  `catchError`
  (\ _e -> io $ D.copyFile old new)
  where
    old = oldf ^. isoFilePath
    new = newf ^. isoFilePath

createDir :: Config r => SysPath -> Action r s ()
createDir sp = io . D.createDirectoryIfMissing True $ sp ^. isoFilePath

removeDir :: Config r => SysPath -> Action r s ()
removeDir sp = io . D.removeDirectoryRecursive $ sp ^. isoFilePath

getWorkingDirectory :: Config r => Action r s FilePath
getWorkingDirectory = io X.getWorkingDirectory

readDir :: Config r => SysPath -> Action r s [FilePath]
readDir sp = io $ do
  s  <- X.openDirStream (sp ^. isoFilePath)
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

putStrLnLB :: Config r => LB.ByteString -> Action r s ()
putStrLnLB = io . LB.putStrLn

putStrLn' :: Config r => String -> Action r s ()
putStrLn' = io . putStrLn

-- ----------------------------------------

atThisMoment :: Config r => Action r s UTCTime
atThisMoment = io C.getCurrentTime

nowAsIso8601 :: Config r => Action r s String
nowAsIso8601 = formatTimeIso8601 <$> atThisMoment

formatTimeIso8601 :: UTCTime -> String
formatTimeIso8601 =
  C.formatTime C.defaultTimeLocale (C.iso8601DateFormat (Just "%H:%M:%S"))

-- ----------------------------------------
