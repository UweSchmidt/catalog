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
  , writeFileLB
  , readFileLB

  , fileNotEmpty
  , getModiTime

  , -- * Interpretations
    basicFileSystem

    -- * aux types and functions
  , TextPath
  , FileStatus
  , EpochTime
  , IOException
  , ioExcToText
  )
where

import Polysemy
import Polysemy.Error

import Control.Exception
       ( IOException )

import Control.Monad

import Data.Text
       ( Text )

import System.IO
       ( hFlush
       , stdout
       )

import System.Posix
       ( FileStatus
       , EpochTime
       )

import qualified Control.Exception          as EX
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Data.Text.Lazy.IO          as LT
import qualified Data.Time.Clock            as C
import qualified Data.Time.Format           as C
import qualified System.Directory           as D
import qualified System.Posix               as X
import qualified System.IO.Error            as EX

------------------------------------------------------------------------------

type TextPath = Text

data FileSystem m a where
  DirExist     :: TextPath -> FileSystem m Bool
  FileExist    :: TextPath -> FileSystem m Bool
  FsStat       :: TextPath -> FileSystem m FileStatus
  FsDirStat    :: TextPath -> FileSystem m FileStatus
  FsFileStat   :: TextPath -> FileSystem m FileStatus
  SetModiTime  :: EpochTime -> TextPath      -> FileSystem m ()
  WriteFileLB  :: TextPath  -> LB.ByteString -> FileSystem m ()
  ReadFileLB   :: TextPath  ->                  FileSystem m LB.ByteString

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

      WriteFileLB p bytes -> embedExc ef $
        LB.writeFile (T.unpack p) bytes

      ReadFileLB p -> embedExc ef $
        LB.readFile (T.unpack p)

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

--------------------

fsStat' :: (String -> IO Bool) -> String -> String -> IO FileStatus
fsStat' exist msg fp = do
  ex <- exist fp
  if ex
    then X.getFileStatus fp
    else EX.throw $ EX.userError $ msg ++ " does not exist: " ++ fp

--------------------
--
-- | perform an IO cmd, catch all IOException
-- and map them to Error exc

embedExc :: forall exc r a
          . ( Member (Embed IO) r
            , Member (Error exc) r
            )
         => (IOException -> exc)
         -> IO a
         -> Sem r a
embedExc ef iocmd = do
  r <- embed $ EX.try iocmd
  case r of
    Left  e -> throw @exc (ef e)
    Right a -> pure a

{-# INLINE embedExc #-}

ioExcToText :: IOException -> Text
ioExcToText = T.pack . show

{-# INLINE ioExcToText  #-}

------------------------------------------------------------------------------
