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

module Server.Commands
  ( -- * Effects
    SCommand (..)

    -- * Actions
  , saveBlogSource
  , changeWriteProtected
  , sortCollection
  , removeFromCollection
  , copyToCollection
  , moveToCollection
  , setCollectionImg
  , setCollectionBlog
  , newCollection
  , renameCollection
  , setMetaData
  , setMetaData1
  , setRating
  , setRating1
  , snapshot
  , syncCollection
  , syncExif
  , newSubCollections
  , updateCheckSum
  , updateTimeStamp
  , theEntry
  , isWriteable
  , isRemovable
  , isSortable
  , isCollection
  , theBlogContents
  , theBlogSource
  , theMetaData
  , theRating
  , theRatings
  , checkImgPart
  , staticFile
  , jpgImgCopy
  , htmlPage

  -- reexport of types used in SCommand
  , ImgNodeP
  , MetaData
  , Name
  , Path
  , Rating
  , ReqType(..)
  , Text
  )
where

import Polysemy

import Data.Prim
import Data.ImgTree     ( ImgNodeP )
import Data.MetaData    ( MetaData
                        , Rating
                        )

-- ----------------------------------------
--
-- the Cmd API as GATD

data SCommand m a where

  -- catalog modifying commands
  SaveBlogSource       ::  Int  -> Text     -> Path -> SCommand m ()
  ChangeWriteProtected :: [Int] -> Bool     -> Path -> SCommand m ()
  SortCollection       ::         [Int]     -> Path -> SCommand m ()
  RemoveFromCollection ::         [Int]     -> Path -> SCommand m ()
  CopyToCollection     :: [Int] -> Path     -> Path -> SCommand m ()
  MoveToCollection     :: [Int] -> Path     -> Path -> SCommand m ()
  SetCollectionImg     :: Path  -> Int      -> Path -> SCommand m ()
  SetCollectionBlog    :: Path  -> Int      -> Path -> SCommand m ()
  NewCollection        ::          Name     -> Path -> SCommand m ()
  RenameCollection     ::          Name     -> Path -> SCommand m ()
  SetMetaData          :: [Int] -> MetaData -> Path -> SCommand m ()
  SetMetaData1         ::  Int  -> MetaData -> Path -> SCommand m ()
  SetRating            :: [Int] -> Rating   -> Path -> SCommand m ()
  SetRating1           ::  Int  -> Rating   -> Path -> SCommand m ()
  Snapshot             ::          Text     -> Path -> SCommand m ()
  SyncCollection       ::                      Path -> SCommand m ()
  SyncExif             ::                      Path -> SCommand m ()
  NewSubCollections    ::                      Path -> SCommand m ()
  UpdateCheckSum       :: CheckSum  -> Name -> Path -> SCommand m ()
  UpdateTimeStamp      :: TimeStamp -> Name -> Path -> SCommand m ()

  -- catalog reading commands
  TheEntry             ::                      Path -> SCommand m ImgNodeP
  IsWriteable          ::                      Path -> SCommand m Bool
  IsRemovable          ::                      Path -> SCommand m Bool
  IsSortable           ::                      Path -> SCommand m Bool
  IsCollection         ::                      Path -> SCommand m Bool
  TheBlogContents      ::          Int      -> Path -> SCommand m Text
  TheBlogSource        ::          Int      -> Path -> SCommand m Text
  TheMetaData          ::          Int      -> Path -> SCommand m MetaData
  TheRating            ::          Int      -> Path -> SCommand m Rating
  TheRatings           ::                      Path -> SCommand m [Rating]
  CheckImgPart         :: Bool ->  Name     -> Path -> SCommand m CheckSumRes

  -- catalog get commands
  StaticFile           ::          FilePath -> Text -> SCommand m LazyByteString
  JpgImgCopy           ::    ReqType -> Geo -> Path -> SCommand m LazyByteString
  HtmlPage             ::    ReqType -> Geo -> Path -> SCommand m LazyByteString

makeSem ''SCommand

-- ----------------------------------------
