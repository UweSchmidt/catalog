{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Catalog.EvalCmd
  ( evalCmd
  , module Catalog.CmdAPI
  )
where

import Data.Prim
import Data.ImgTree ( ImgNode )

import Catalog.Cmd
import Catalog.CmdAPI
import Catalog.JsonCommands

import Catalog.Workflow        ( Req'
                               , emptyReq'
                               , processReqImg
                               , processReqPage
                               , rType
                               , rGeo
                               , rPathPos
                               )

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
  getIdNode' p >>= uncurry (modify'setMetaData1 pos md)

evalCmd (SetRating ixs r p) =
  path2node p >>= modify'setRating ixs r

evalCmd (SetRating1 pos r p) =
  getIdNode' p >>= uncurry (modify'setRating1 pos r)

evalCmd (Snapshot t _p) =
  modify'snapshot t

evalCmd (SyncCollection p) =
  path2id p >>= modify'syncCol

evalCmd (SyncExif p) =
  path2id p >>= modify'syncExif

evalCmd (NewSubCollections p) =
  path2id p >>= modify'newSubCols

evalCmd (UpdateCheckSum cs n p) =
  path2id p >>= modify'updateCheckSum cs n

evalCmd (UpdateTimeStamp ts n p) =
  path2id p >>= modify'updateTimeStamp ts n


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
  getIdNode' p >>= uncurry (read'blogcontents pos)

evalCmd (TheBlogSource pos p) =
  getIdNode' p >>= uncurry (read'blogsource pos)

evalCmd (TheMetaData pos p) =
  getIdNode' p >>= uncurry (read'metadata pos)

evalCmd (TheRating pos p) =
  getIdNode' p >>= uncurry (read'rating pos)

evalCmd (TheRatings p) =
  path2node p >>= read'ratings

evalCmd (CheckImgPart onlyUpdate nm p) =
  path2node p >>= read'checkImgPart onlyUpdate p nm

-- eval get commands

evalCmd (StaticFile dp bn) = do
  sp <- view ( envMountPath .
               isoFilePath .
               to (++ (dp ++ "/" ++ bn ^. isoString)) .
               from isoFilePath
             )
  readStaticFile sp

evalCmd (JpgImgCopy rt geo path)
  | Just ppos <- path2colPath ".jpg" path =
      process ppos
  | otherwise =
      abort $ "illegal doc path " ++ show path
  where
    process ppos =
       processReqImg (mkReq rt geo ppos)
       >>= toSysPath
       >>= readFileLB

evalCmd (HtmlPage rt geo path)
  | Just ppos <- path2colPath ".html" path =
      process ppos
  | otherwise =
      abort $ "illegal doc path " ++ show path
  where
    process ppos =
       processReqPage (mkReq rt geo ppos)

-- ----------------------------------------
--
-- helper functions

mkReq :: ReqType -> Geo -> PathPos -> Req' ()
mkReq rt' geo' ppos' =
  emptyReq' & rType    .~ rt'
            & rGeo     .~ geo'
            & rPathPos .~ ppos'

-- parser for object path
--
-- remove extension
-- parse optional collection index
--
-- example: path2colPath ".jpg" "collections/2018/may/pic-0007.jpg"
--          -> Just ("/collections/2018/may", Just 7)

path2colPath :: String -> Path -> Maybe PathPos
path2colPath ext p =
  (^. isoPathPos) <$> checkAndRemExt ext p

readStaticFile :: SysPath -> Cmd LazyByteString
readStaticFile sp = do
  ex <- fileExist sp
  if ex
    then readFileLB sp
    else abort $ "document not found: " ++ sp ^. isoFilePath

-- --------------------

path2id :: Path -> Cmd ObjId
path2id p = fst <$> getIdNode' p

path2node :: Path -> Cmd ImgNode
path2node p = snd <$> getIdNode' p

-- ----------------------------------------
