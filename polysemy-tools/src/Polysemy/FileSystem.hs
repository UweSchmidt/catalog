{-# LANGUAGE
    DataKinds,
    FlexibleContexts,
    GADTs,
    OverloadedStrings,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TemplateHaskell,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Polysemy.FileSystem
  ( -- Effect
    FileSystem (..)

    -- * Actions
  , fileExist
  , dirExist
  , fsStat
  , fsDirStat
  , fsFileStat
  , setModiTime
  , writeFileBS
  , writeFileLB
  , writeFileT
  , writeFileLT
  , readFileBS
  , readFileLB
  , readFileT
  , readDir
  , removeFile
  , removeDir
  , createDir
  , renameFile
  , linkFile
  , getWorkDir

  , fileNotEmpty
  , getModiTime
  , readFileT'

  , -- * Interpretations
    basicFileSystem

    -- * reexports
  , embedExc
  , ioExcToText

    -- * aux types and functions
  , TextPath
  , FileStatus
  , EpochTime
  , IOException
  )
where

import Polysemy
import Polysemy.Error
import Polysemy.EmbedExc

import Data.ByteString
       ( ByteString )

import Data.Text
       ( Text )

import System.Posix
       ( FileStatus
       , EpochTime
       )

import qualified Control.Exception          as EX
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.IO          as LT
import qualified System.Directory           as D
import qualified System.Posix               as X
import qualified System.IO.Error            as EX

------------------------------------------------------------------------------

type TextPath = Text

data FileSystem m a where
  DirExist     :: TextPath  ->                  FileSystem m Bool
  FileExist    :: TextPath  ->                  FileSystem m Bool
  FsStat       :: TextPath  ->                  FileSystem m FileStatus
  FsDirStat    :: TextPath  ->                  FileSystem m FileStatus
  FsFileStat   :: TextPath  ->                  FileSystem m FileStatus
  SetModiTime  :: EpochTime -> TextPath      -> FileSystem m ()
  WriteFileBS  :: TextPath  -> ByteString    -> FileSystem m ()
  WriteFileLB  :: TextPath  -> LB.ByteString -> FileSystem m ()
  WriteFileT   :: TextPath  -> Text          -> FileSystem m ()
  WriteFileLT  :: TextPath  -> LT.Text       -> FileSystem m ()
  ReadFileBS   :: TextPath  ->                  FileSystem m ByteString
  ReadFileLB   :: TextPath  ->                  FileSystem m LB.ByteString
  ReadFileT    :: TextPath  ->                  FileSystem m Text
  ReadDir      :: TextPath ->                   FileSystem m [TextPath]
  RemoveFile   :: TextPath  ->                  FileSystem m ()
  RemoveDir    :: TextPath  ->                  FileSystem m ()
  CreateDir    :: TextPath  ->                  FileSystem m ()
  RenameFile   :: TextPath  -> TextPath ->      FileSystem m ()
  LinkFile     :: TextPath  -> TextPath ->      FileSystem m ()
  GetWorkDir   ::                               FileSystem m TextPath

makeSem ''FileSystem

------------------------------------------------------------------------------
--
-- | basic filesystem access
--

basicFileSystem :: ( Member (Embed IO) r
                   , Member (Error exc) r
                   )
                => (IOException -> exc)
                -> InterpreterFor FileSystem r
basicFileSystem ef = do
  interpret $
    \ c -> case c of
      DirExist p -> embedExc ef $
        D.doesDirectoryExist (T.unpack p)

      FileExist p -> embedExc ef $
        D.doesFileExist (T.unpack p)

      FsStat p -> embedExc ef $
        X.getFileStatus (T.unpack p)

      FsDirStat p -> embedExc ef $ do
        fsStat' D.doesDirectoryExist "directory" (T.unpack p)

      FsFileStat p -> embedExc ef $
        fsStat' D.doesFileExist "regular file" (T.unpack p)

      SetModiTime ep p -> embedExc ef $
        X.setFileTimes (T.unpack p) ep ep

      WriteFileBS p bytes -> embedExc ef $
        BS.writeFile (T.unpack p) bytes

      WriteFileLB p bytes -> embedExc ef $
        LB.writeFile (T.unpack p) bytes

      WriteFileT p txt -> embedExc ef $
        T.writeFile (T.unpack p) txt

      WriteFileLT p txt -> embedExc ef $
        LT.writeFile (T.unpack p) txt

      ReadFileBS p -> embedExc ef $
        BS.readFile (T.unpack p)

      ReadFileLB p -> embedExc ef $
        LB.readFile (T.unpack p)

      ReadFileT p -> embedExc ef $
        T.readFile (T.unpack p)

      ReadDir p -> embedExc ef $
        map T.pack <$> readDir' (T.unpack p)

      RemoveFile p -> embedExc ef $
        D.removeFile (T.unpack p)

      RemoveDir p -> embedExc ef $
        D.removeDirectoryRecursive (T.unpack p)

      CreateDir p -> embedExc ef $
        D.createDirectoryIfMissing True (T.unpack p)

      RenameFile op np -> embedExc ef $
        X.rename (T.unpack op) (T.unpack np)

      LinkFile op np -> embedExc ef $ do
        let ofp = T.unpack op
            nfp = T.unpack np

        X.createLink ofp nfp
           `EX.catchIOError`
           ( const $ D.copyFile ofp nfp)

      GetWorkDir -> embedExc ef $
        T.pack <$> X.getWorkingDirectory

--------------------
--
-- derived operations

fileNotEmpty :: Member FileSystem r
             => TextPath -> Sem r Bool
fileNotEmpty p = do
  ex <- fileExist p
  if ex
    then do st <- fsStat p
            return $ X.fileSize st == 0
    else return True

getModiTime :: Member FileSystem r
            => TextPath -> Sem r EpochTime
getModiTime p = do
  ex <- fileExist p
  if ex
    then do X.modificationTime <$> fsStat p
    else return $ fromIntegral (0::Int)

readFileT' :: Member FileSystem r
           => TextPath -> Sem r Text
readFileT' p = do
  ex <- fileExist p
  if ex
    then readFileT p
    else return mempty

--------------------

fsStat' :: (String -> IO Bool) -> String -> String -> IO FileStatus
fsStat' exist msg fp = do
  ex <- exist fp
  if ex
    then X.getFileStatus fp
    else EX.throw $ EX.userError $ msg ++ " does not exist: " ++ fp

readDir' :: String -> IO [String]
readDir' fp =
  EX.bracket
    (X.openDirStream fp)
    (X.closeDirStream)
    readDirEntries
  where
    readDirEntries s = do
      e1 <- X.readDirStream s
      if null e1
        then return []
        else do
          es <- readDirEntries s
          return (e1 : es)

------------------------------------------------------------------------------
