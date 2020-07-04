{-# LANGUAGE
    ConstraintKinds,
    DataKinds,
    FlexibleContexts,
    GADTs,
    OverloadedStrings,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)

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
  , theMetaData
  , theRating
  , theRatings
  , checkImgPart

  -- catalog get commands
  , staticFile
  , jpgImgCopy
  , htmlPage

  -- undo history commands
  , newUndoEntry
  , applyUndo
  , dropUndoEntries

  -- reexport of types used in CatCmd
  , HistoryID
  , ImgNodeP
  , MetaData
  , Name
  , Path
  , Rating
  , ReqType(..)
  , Text
  , TextPath
  )
where

import Polysemy
import Polysemy.FileSystem ( TextPath )

import Data.Prim
import Data.ImgTree        ( ImgNodeP )
import Data.MetaData       ( MetaData
                           , Rating
                           )
import Data.History        ( HistoryID )

-- ----------------------------------------
--
-- the Cmd API as GATD

data CatCmd m a where
  -- catalog modifying commands
  SaveBlogSource       ::  Int  -> Text     -> Path -> CatCmd m ()
  ChangeWriteProtected :: [Int] -> Bool     -> Path -> CatCmd m ()
  SortCollection       ::         [Int]     -> Path -> CatCmd m ()
  RemoveFromCollection ::         [Int]     -> Path -> CatCmd m ()
  CopyToCollection     :: [Int] -> Path     -> Path -> CatCmd m ()
  MoveToCollection     :: [Int] -> Path     -> Path -> CatCmd m ()
  SetCollectionImg     :: Path  -> Int      -> Path -> CatCmd m ()
  SetCollectionBlog    :: Path  -> Int      -> Path -> CatCmd m ()
  NewCollection        ::          Name     -> Path -> CatCmd m ()
  RenameCollection     ::          Name     -> Path -> CatCmd m ()
  SetMetaData          :: [Int] -> MetaData -> Path -> CatCmd m ()
  SetMetaData1         ::  Int  -> MetaData -> Path -> CatCmd m ()
  SetRating            :: [Int] -> Rating   -> Path -> CatCmd m ()
  SetRating1           ::  Int  -> Rating   -> Path -> CatCmd m ()
  Snapshot             ::          Text     -> Path -> CatCmd m ()
  SyncCollection       ::                      Path -> CatCmd m ()
  SyncExif             ::                      Path -> CatCmd m ()
  NewSubCollections    ::                      Path -> CatCmd m ()
  UpdateCheckSum       :: CheckSum  -> Name -> Path -> CatCmd m ()
  UpdateTimeStamp      :: TimeStamp -> Name -> Path -> CatCmd m ()

  -- catalog reading commands
  TheEntry             ::                      Path -> CatCmd m ImgNodeP
  IsWriteable          ::                      Path -> CatCmd m Bool
  IsRemovable          ::                      Path -> CatCmd m Bool
  IsSortable           ::                      Path -> CatCmd m Bool
  IsCollection         ::                      Path -> CatCmd m Bool
  TheBlogContents      ::          Int      -> Path -> CatCmd m Text
  TheBlogSource        ::          Int      -> Path -> CatCmd m Text
  TheMetaData          ::          Int      -> Path -> CatCmd m MetaData
  TheRating            ::          Int      -> Path -> CatCmd m Rating
  TheRatings           ::                      Path -> CatCmd m [Rating]
  CheckImgPart         :: Bool ->  Name     -> Path -> CatCmd m CheckSumRes

  -- catalog get commands
  StaticFile           ::          TextPath -> Text -> CatCmd m LazyByteString
  JpgImgCopy           ::    ReqType -> Geo -> Path -> CatCmd m LazyByteString
  HtmlPage             ::    ReqType -> Geo -> Path -> CatCmd m LazyByteString

  -- Undo history
  NewUndoEntry         ::                              CatCmd m HistoryID
  ApplyUndo            ::                 HistoryID -> CatCmd m ()
  DropUndoEntries      ::                 HistoryID -> CatCmd m ()

makeSem ''CatCmd

-- ----------------------------------------
