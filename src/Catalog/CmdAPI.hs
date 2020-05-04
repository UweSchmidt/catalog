{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Catalog.CmdAPI
  ( Cmd'(..)          -- the catalog API for server

  -- reexport of types used in Cmd'
  , ImgNodeP
  , MetaData
  , Name
  , Path
  , Rating
  , ReqType(..)
  , Text
  )
where

import Data.Prim
import Data.ImgTree     ( ImgNodeP )
import Data.MetaData    ( MetaData
                        , Rating
                        )
import Catalog.CheckSum ( CheckSumRes )

-- ----------------------------------------
--
-- the Cmd API as GATD

data Cmd' a where
  -- catalog modifying commands
  SaveBlogSource       ::  Int  -> Text     -> Path -> Cmd' ()
  ChangeWriteProtected :: [Int] -> Bool     -> Path -> Cmd' ()
  SortCollection       ::         [Int]     -> Path -> Cmd' ()
  RemoveFromCollection ::         [Int]     -> Path -> Cmd' ()
  CopyToCollection     :: [Int] -> Path     -> Path -> Cmd' ()
  MoveToCollection     :: [Int] -> Path     -> Path -> Cmd' ()
  SetCollectionImg     :: Path  -> Int      -> Path -> Cmd' ()
  SetCollectionBlog    :: Path  -> Int      -> Path -> Cmd' ()
  NewCollection        ::          Name     -> Path -> Cmd' ()
  RenameCollection     ::          Name     -> Path -> Cmd' ()
  SetMetaData          :: [Int] -> MetaData -> Path -> Cmd' ()
  SetMetaData1         ::  Int  -> MetaData -> Path -> Cmd' ()
  SetRating            :: [Int] -> Rating   -> Path -> Cmd' ()
  SetRating1           ::  Int  -> Rating   -> Path -> Cmd' ()
  Snapshot             ::          Text     -> Path -> Cmd' ()
  SyncCollection       ::                      Path -> Cmd' ()
  SyncExif             ::                      Path -> Cmd' ()
  NewSubCollections    ::                      Path -> Cmd' ()
  UpdateCheckSum       :: CheckSum  -> Name -> Path -> Cmd' ()
  UpdateTimeStamp      :: TimeStamp -> Name -> Path -> Cmd' ()

  -- catalog reading commands
  TheCollection        ::                      Path -> Cmd' ImgNodeP
  IsWriteable          ::                      Path -> Cmd' Bool
  IsRemovable          ::                      Path -> Cmd' Bool
  IsSortable           ::                      Path -> Cmd' Bool
  IsCollection         ::                      Path -> Cmd' Bool
  TheBlogContents      ::          Int      -> Path -> Cmd' Text
  TheBlogSource        ::          Int      -> Path -> Cmd' Text
  TheMetaData          ::          Int      -> Path -> Cmd' MetaData
  TheRating            ::          Int      -> Path -> Cmd' Rating
  TheRatings           ::                      Path -> Cmd' [Rating]
  CheckImgPart         :: Bool ->  Name     -> Path -> Cmd' CheckSumRes

  -- catalog get commands
  StaticFile           ::          FilePath -> Text -> Cmd' LazyByteString
  JpgImgCopy           ::    ReqType -> Geo -> Path -> Cmd' LazyByteString
  HtmlPage             ::    ReqType -> Geo -> Path -> Cmd' LazyByteString


-- ----------------------------------------
