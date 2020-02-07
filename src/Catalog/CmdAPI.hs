{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Catalog.CmdAPI
  ( Cmd'(..)
  , module Data.Prim
  , module Data.ImgTree
  , module Data.MetaData
  )
where

import Data.Prim     ( Name
                     , Path
                     , Text
                     )
import Data.ImgTree  ( ImgNodeP )
import Data.MetaData ( MetaData
                     , Rating
                     )

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


{- client side: commands will be turned into requests

reqCmd :: Cmd a -> (forall r. ToJSON r => Req r)
reqCmd c = undefined

data Method = Post | Get
data Req r = Req
           { method :: Method
           , url    :: Text
           , body   :: Maybe r
           }
-- -}

-- ----------------------------------------
