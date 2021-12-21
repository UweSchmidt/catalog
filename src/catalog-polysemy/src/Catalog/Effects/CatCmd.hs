{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------

module Catalog.Effects.CatCmd
  ( -- * Effect
    CatCmd(..)          -- the catalog API for server and clients

    -- * Actions

    -- catalog modifying commands
  , saveBlogSource
  , changeWriteProtected
  , sortCollection
  , sortCollByDate
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

  -- catalog reading commands
  , theEntry
  , isWriteable
  , isRemovable
  , isSortable
  , isCollection
  , theBlogContents
  , theBlogSource
  , theMetaDataText
  , theRating
  , theRatings
  , theMediaPath
  , checkImgPart

  -- catalog get commands
  , staticFile
  , jpgImgCopy
  , htmlPage

  -- undo history commands
  , newUndoEntry
  , applyUndo
  , dropUndoEntries
  , listUndoEntries

  -- reexport of types used in CatCmd
  , HistoryID
  , ImgNodeP
  , MetaDataText
  , Name
  , Path
  , Rating
  , ReqType(..)
  , Text
  , TextPath
  )
where

import Polysemy
       ( makeSem )
import Polysemy.FileSystem
       ( TextPath )

import Data.Prim
       ( Text
       , CheckSum
       , CheckSumRes
       , Geo
       , Name
       , Path
       , LazyByteString
       , ReqType(..)
       , TimeStamp
       )
import Data.ImgTree
       ( ImgNodeP )

import Data.MetaData
       ( MetaDataText
       , Rating
       )
import Data.History
       ( HistoryID )

-- ----------------------------------------
--
-- the Cmd API as GATD

data CatCmd m a where
  -- catalog modifying commands
  SaveBlogSource       ::  Int  -> Text         -> Path -> CatCmd m ()
  ChangeWriteProtected :: [Int] -> Bool         -> Path -> CatCmd m ()
  SortCollection       ::         [Int]         -> Path -> CatCmd m ()
  SortCollByDate       ::         [Int]         -> Path -> CatCmd m ()
  RemoveFromCollection ::         [Int]         -> Path -> CatCmd m ()
  CopyToCollection     :: [Int] -> Path         -> Path -> CatCmd m ()
  MoveToCollection     :: [Int] -> Path         -> Path -> CatCmd m ()
  SetCollectionImg     :: Path  -> Int          -> Path -> CatCmd m ()
  SetCollectionBlog    :: Path  -> Int          -> Path -> CatCmd m ()
  NewCollection        ::          Name         -> Path -> CatCmd m ()
  RenameCollection     ::          Name         -> Path -> CatCmd m ()
  SetMetaData          :: [Int] -> MetaDataText -> Path -> CatCmd m ()
  SetMetaData1         ::  Int  -> MetaDataText -> Path -> CatCmd m ()
  SetRating            :: [Int] -> Rating       -> Path -> CatCmd m ()
  SetRating1           ::  Int  -> Rating       -> Path -> CatCmd m ()
  Snapshot             ::          Text         -> Path -> CatCmd m ()
  SyncCollection       ::                          Path -> CatCmd m ()
  SyncExif             :: Bool  -> Bool         -> Path -> CatCmd m ()
  NewSubCollections    ::                          Path -> CatCmd m ()
  UpdateCheckSum       :: CheckSum  -> Name     -> Path -> CatCmd m ()
  UpdateTimeStamp      :: TimeStamp -> Name     -> Path -> CatCmd m ()

  -- catalog reading commands
  TheEntry             ::                          Path -> CatCmd m ImgNodeP
  IsWriteable          ::                          Path -> CatCmd m Bool
  IsRemovable          ::                          Path -> CatCmd m Bool
  IsSortable           ::                          Path -> CatCmd m Bool
  IsCollection         ::                          Path -> CatCmd m Bool
  TheBlogContents      ::          Int          -> Path -> CatCmd m Text
  TheBlogSource        ::          Int          -> Path -> CatCmd m Text
  TheMetaDataText      ::          Int          -> Path -> CatCmd m MetaDataText
  TheRating            ::          Int          -> Path -> CatCmd m Rating
  TheRatings           ::                          Path -> CatCmd m [Rating]
  TheMediaPath         ::                          Path -> CatCmd m [Path]
  CheckImgPart         :: Bool ->  Name         -> Path -> CatCmd m CheckSumRes

  -- catalog get commands
  StaticFile           ::                      TextPath -> CatCmd m LazyByteString
  JpgImgCopy           ::    ReqType -> Geo     -> Path -> CatCmd m LazyByteString
  HtmlPage             ::    ReqType -> Geo     -> Path -> CatCmd m LazyByteString

  -- Undo history
  NewUndoEntry         ::                          Text -> CatCmd m HistoryID
  ApplyUndo            ::                     HistoryID -> CatCmd m ()
  DropUndoEntries      ::                     HistoryID -> CatCmd m ()
  ListUndoEntries      ::                                  CatCmd m [(HistoryID, Text)]

makeSem ''CatCmd

-- ----------------------------------------
