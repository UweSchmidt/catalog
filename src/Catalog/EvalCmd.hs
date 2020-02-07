{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Catalog.EvalCmd
  ( evalCmd
  , module Catalog.CmdAPI
  )
where

import Data.Prim    ( ObjId )
import Data.ImgTree ( ImgNode )

import Catalog.Cmd
import Catalog.CmdAPI
import Catalog.JsonCommands

-- ----------------------------------------

evalCmd :: Cmd' a -> Cmd a

-- eval modifying commands

evalCmd (SaveBlogSource pos t p) =
  path2node p >>= modify'saveblogsource pos t

evalCmd (ChangeWriteProtected ixs ro p) =
  path2node p >>= modify'changeWriteProtected ixs ro

evalCmd (SortCollection ixs p) =
  getIdNode' p >>= uncurry (modify'sort ixs)

evalCmd (RemoveFromCollection ixs p) =
  getIdNode' p >>= uncurry (modify'removeFromCollection ixs)

evalCmd (CopyToCollection ixs dst p) =
  path2node p >>= modify'copyToCollection ixs dst

evalCmd (MoveToCollection ixs dst p) =
  getIdNode' p >>= uncurry (modify'moveToCollection ixs dst)

evalCmd (SetCollectionImg sPath pos p) =
  path2id p >>= modify'colimg sPath pos

evalCmd (SetCollectionBlog sPath pos p) =
  path2id p >>= modify'colblog sPath pos

evalCmd (NewCollection nm p) =
  path2id p >>= modify'newcol nm

evalCmd (RenameCollection nm p) =
  path2id p >>= modify'renamecol nm

evalCmd (SetMetaData ixs md p) =
  path2node p >>= modify'setMetaData ixs md

evalCmd (SetMetaData1 pos md p) =
  path2node p >>= modify'setMetaData1 pos md

evalCmd (SetRating ixs r p) =
  path2node p >>= modify'setRating ixs r

evalCmd (SetRating1 pos r p) =
  path2node p >>= modify'setRating1 pos r

evalCmd (Snapshot t _p) =
  modify'snapshot t

evalCmd (SyncCollection p) =
  path2id p >>= modify'syncCol

evalCmd (SyncExif p) =
  path2id p >>= modify'syncExif

evalCmd (NewSubCollections p) =
  path2id p >>= modify'newSubCols


-- eval reading commands

evalCmd (TheCollection p) =
  path2node p >>= read'collection

evalCmd (IsWriteable p) =
  path2node p >>= read'isWriteable

evalCmd (IsRemovable p) =
  path2node p >>= read'isRemovable

evalCmd (IsSortable p) =
  path2node p >>= read'isSortable

evalCmd (IsCollection p) =
  read'isCollection p

evalCmd (TheBlogContents pos p) =
  path2node p >>= read'blogcontents pos

evalCmd (TheBlogSource pos p) =
  path2node p >>= read'blogsource pos

evalCmd (TheMetaData pos p) =
  path2node p >>= read'metadata pos

evalCmd (TheRating pos p) =
  path2node p >>= read'rating pos

evalCmd (TheRatings p) =
  path2node p >>= read'ratings

-- --------------------

path2id :: Path -> Cmd ObjId
path2id p = fst <$> getIdNode' p

path2node :: Path -> Cmd ImgNode
path2node p = snd <$> getIdNode' p

-- ----------------------------------------
