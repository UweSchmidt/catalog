{-# LANGUAGE OverloadedStrings #-}

module Catalog.JsonCommands
  ( modify'changeWriteProtected
  , modify'colblog
  , modify'colimg
  , modify'copyToCollection
  , modify'moveToCollection
  , modify'newSubCols
  , modify'newcol
  , modify'removeFromCollection
  , modify'renamecol
  , modify'saveblogsource
  , modify'setMetaData
  , modify'setMetaData1
  , modify'setRating
  , modify'setRating1
  , modify'snapshot
  , modify'sort
  , modify'syncCol
  , modify'syncExif
  , modify'updateCheckSum
  , modify'updateTimeStamp
  , read'blogcontents
  , read'blogsource
  , read'collection
  , read'isCollection
  , read'isRemovable
  , read'isSortable
  , read'isWriteable
  , read'metadata
  , read'rating
  , read'ratings
  , read'checkImgPart
  )
where

import Data.Prim
import Data.ImgNode
import Data.ImgTree
import Data.MetaData

import Catalog.Cmd
import Catalog.Html.Basic ( getColBlogSource
                          , putColBlogSource
                          , getColBlogCont
                          )
import Catalog.CheckSum   ( checkImgPart
                          , updateCheckSum
                          , updateTimeStamp
                          )
import Catalog.Sync       ( syncDirP
                          , syncNewDirs
                          )
import Catalog.System.ExifTool
                          ( forceSyncAllMetaData )

import qualified Data.Sequence as Seq

-- ----------------------------------------
--
-- commands for modifying the catalog

-- --------------------

modify'saveblogsource :: Int -> Text -> ImgNode -> Cmd ()
modify'saveblogsource pos t n = putBlogCont t pos n
  where
    putBlogCont :: Text -> Int -> ImgNode -> Cmd ()
    putBlogCont val =
      processColEntryAt
        (putColBlogSource val)   -- change blog file
        putColBlog               -- change blog entry of collection
        where
          putColBlog i = do
            n'  <- getImgVal i   -- the blog file of the collection
            br  <- maybe
                   (abortP i ("modify'saveblogsource: "
                              ++ "no blog entry set in collection: "))
                   return
                   (n' ^? theColBlog . traverse)
            putColBlogSource val br

-- --------------------
--
-- change the write protection for a list of collection entries

modify'changeWriteProtected :: [Int] -> Bool -> ImgNode -> Cmd ()
modify'changeWriteProtected ixs ro n =
  traverse_ mark $ toPosList ixs
  where
    cs = n ^. theColEntries

    cf | ro        = addNoWriteAccess
       | otherwise = subNoWriteAccess

    mark pos = maybe (return ()) adj $ cs ^? ix pos
      where
        adj = colEntry'
              (\ _ -> return ())            -- ignore ImgRef's
              (adjustMetaData cf)

-- --------------------
--
-- sort a collection by sequence of positions
-- result is the new collection

modify'sort :: [Int] -> ObjId -> ImgNode -> Cmd ()
modify'sort ixs i n
  | null ixs =
      return ()
  | otherwise = do
      unless (isSortableCol n) $
        abortP i "modify'sort: collection not sortable"
      adjustColEntries (reorderCol ixs) i

reorderCol :: [Int] -> Seq a -> Seq a
reorderCol ixs cs =
  fmap snd . Seq.sortBy (cmp mx `on` fst) $ Seq.zip ixs' cs
  where
    ixs' :: Seq (Int, Int)
    ixs' = isoSeqList # zip ixs [0..]

    mx :: (Int, Int)
    mx = maximum ixs'

cmp :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Ordering
cmp (mi, mx) (i, x) (j, y)
      | i == -1 && j == -1 =
        compare x y
      | i == -1 && j >= 0 =
        compare x mx
      | i >= 0  && j == -1 =
        compare mx y
      | i == mi && j >= 0 =
        LT
      | i >= 0  && j == mi =
        GT
      | i >= 0  && j >= 0 =
        compare i j
      | otherwise =
        EQ

-- --------------------
--
-- remove all marked images and sub-collection from a collection

modify'removeFromCollection :: [Int] -> ObjId -> ImgNode -> Cmd ()
modify'removeFromCollection ixs i n = do
  checkRemoveable "modify'removeToCollection" ixs i n
  removeFromCol ixs i n

removeFromCol :: [Int] -> ObjId -> ImgNode -> Cmd ()
removeFromCol ixs oid n =
  traverse_ rmv $ toPosDescList ixs  -- remove list entries from the right
  where
    cs = n ^. theColEntries

    rmv :: Int -> Cmd ()
    rmv pos = maybe (return ()) rm $ cs ^? ix pos
      where
        rm = colEntry'
             (\ _ -> adjustColEntries (Seq.deleteAt pos) oid)
             rmRec

checkMoveable :: [Int] -> ImgNode -> Cmd Bool
checkMoveable ixs n =
  and <$> mapM check ixs
  where
    cs = n ^. theColEntries

    check :: Int -> Cmd Bool
    check pos = maybe (return True) ck $ cs ^? ix pos
      where
        ck :: ColEntry -> Cmd Bool
        ck = colEntry'
             (const $ return True)
             ckCol

        ckCol :: ObjId -> Cmd Bool
        ckCol i = isRemovableCol <$> getImgVal i

-- --------------------
--
-- copy marked images and collections to another collection
--
-- the int list doesn't contain indexes but
-- ints which determine the order of copying
--
-- a -1 indicates "don't copy this image"
-- an int i >= 0 indicates "copy this as the i-th image"

modify'copyToCollection :: [Int] -> Path -> ImgNode -> Cmd ()
modify'copyToCollection ixs dPath n = do
  di <- checkWriteableCol "modify'copyToCollection" dPath
  copyToCol ixs di n

copyToCol :: [Int] -> ObjId -> ImgNode -> Cmd ()
copyToCol xs di n =
  traverse_ cpy $ toPosList xs
  where
    cs = n ^. theColEntries

    cpy pos = maybe (return ()) cp $ cs ^? ix pos
      where
        cp ce = colEntry'
                (\ _ -> adjustColEntries (Seq.|> ce) di)
                copyColToCol
                ce
          where
            copyColToCol si = do
              dp <- objid2path di
              sp <- objid2path si
              copyCollection sp dp
              -- remove the access restrictions in copied collection,
              -- in a copied collection there aren't any access restrictions
              --
              -- the path of the copied collection
              let tp = dp `snocPath` (sp ^. viewBase . _2)
              modifyMetaDataRec clearAccess tp

modifyMetaDataRec :: (MetaData -> MetaData) -> Path -> Cmd ()
modifyMetaDataRec mf path = do
  i <- fst <$> getIdNode "modifyMetaDataRec: entry not found" path
  foldCollections colA i
  where
    colA go i md im be cs = do
      adjustMetaData mf i
      foldColColEntries go i md im be cs

checkWriteableCol :: String -> Path -> Cmd ObjId
checkWriteableCol jsonCall dPath = do
  (di, dn) <- getIdNode' dPath
  unless (isWriteableCol dn) $
    abortP di (jsonCall ++ ": not a writeable collection")
  return di

checkRemoveable :: String -> [Int] -> ObjId -> ImgNode -> Cmd ()
checkRemoveable jsonCall ixs i n = do
  -- check whether source collection is writeable (contents may change)
  unless (isWriteableCol n) $
    abortP i (jsonCall ++ ": source collection is write protected")

  -- check whether entries (collections are removable)
  unlessM (checkMoveable ixs n) $
    abortP i (jsonCall ++ ": delete-protected entry found in")

-- --------------------
--
-- move marked images and collections in a source col
-- to a dest col
-- this is implemented as a sequence of copy and remove

modify'moveToCollection :: [Int] -> Path -> ObjId -> ImgNode -> Cmd ()
modify'moveToCollection ixs dPath i n = do
  checkRemoveable "modify'moveToCollection" ixs i n
  di <- checkWriteableCol "modify'moveToCollection" dPath

  copyToCol     ixs di n
  removeFromCol ixs  i n

-- --------------------
--
-- set or unset the collection image
-- i must reference a collection, not an image
-- pos must be an index to an ImgRef for a .jpg image

modify'colimg :: Path -> Int -> ObjId -> Cmd ()
modify'colimg = modifyCol adjustColImg

-- set or unset the collection blog text
-- i must reference a collection, not an image
-- pos must be an index to an ImgRef for a .md text

modify'colblog :: Path -> Int -> ObjId -> Cmd ()
modify'colblog = modifyCol adjustColBlog


modifyCol :: ((Maybe ImgRef -> Maybe ImgRef) -> ObjId -> Cmd ())
          -> Path -> Int -> ObjId -> Cmd ()
modifyCol adjust sPath pos i
  | pos < 0 =
      adjust (const Nothing) i
  | otherwise = do
      scn <- snd <$> getIdNode' sPath
      processColImgEntryAt
        (\ ir -> adjust (const $ Just ir) i)
        pos scn

-- --------------------
--
-- create a new collection with name nm in
-- collection i

modify'newcol :: Name -> ObjId -> Cmd ()
modify'newcol nm i = do
  path  <- objid2path i
  _newi <- mkCollection (path `snocPath` nm)
  return ()

-- --------------------
--
-- rename a sub-collection in a given collection

modify'renamecol :: Name -> ObjId -> Cmd ()
modify'renamecol newName i = do
  iParent <- getImgParent i

  -- duplicate collection in parent collection
  dupColRec i iParent newName

  -- find position of objid i in parent collection
  ps <- flip findFstColEntry iParent $
        \ ce -> return (i == ce ^. theColObjId)
  let pos = maybe (-1) fst ps

  -- remove i in parent collection
  rmRec i

  -- move duplicated col from the end of the col entry list to the pos of i
  adjustColEntries (adj pos) iParent
    where
      adj pos cs = Seq.insertAt pos cn . Seq.deleteAt ixm $ cs
        where
          len = Seq.length cs
          ixm = len - 1
          cn  = Seq.index cs ixm

-- --------------------
--
-- set meta data fields for a list of selected collection entries

modify'setMetaData :: [Int] -> MetaData -> ImgNode -> Cmd ()
modify'setMetaData ixs md n =
  traverse_ setm $ toPosList ixs
  where
    cs = n ^. theColEntries

    setm pos = maybe (return ()) sm $ cs ^? ix pos
      where
        sm = colEntry'
             (adjustMetaData (md <>) . _iref)
             (adjustMetaData (md <>)        )

-- set meta data fields for a collection or a single collection entry

modify'setMetaData1 :: Int -> MetaData -> ObjId -> ImgNode -> Cmd ()
modify'setMetaData1 pos md oid n
  | pos < 0   = adjustMetaData (md <>) oid      -- update coll  metadata
  | otherwise = modify'setMetaData ixs md n     -- update entry metadata
  where
    ixs = replicate pos (0-1) ++ [1]

-- set the rating field for a list of selected collection entries

modify'setRating :: [Int] -> Rating -> ImgNode -> Cmd ()
modify'setRating ixs r =
  modify'setMetaData ixs (mkRating r)

-- set the rating field for a collection or a single collection entry

modify'setRating1 :: Int -> Rating -> ObjId -> ImgNode -> Cmd ()
modify'setRating1 pos r oid n
  | pos < 0   = modify'setMetaData1 pos md oid n
  | otherwise = modify'setRating ixs r n
  where
    md  = mkRating r
    ixs = replicate pos (0-1) ++ [1]

-- --------------------
--

-- save a snapshot of the current image store
-- on client side, the 1. arg must be a path to an existing node
-- simply take p'archive ("/archive"), the root node

modify'snapshot :: Text -> Cmd ()
modify'snapshot t = snapshotImgStore (t ^. isoString)

-- --------------------
--
-- sync a subcollection of /archive/photo with filesystem

modify'syncCol :: ObjId -> Cmd ()
modify'syncCol i = do
  ts <- now
  syncCol' (syncDirP ts) i

syncCol' :: (Path -> Cmd ()) -> ObjId -> Cmd ()
syncCol' sync i = do
  path <- objid2path i
  unless (isPathPrefix p'photos path) $
    abortP i ("syncCol': collection does not have path prefix "
              ++ quotePath p'photos)
  let path'dir = substPathPrefix p'photos p'arch'photos path
  verbose $ "syncCol': directory " ++ quotePath path'dir
  sync path'dir

-- sync a subcollection of /archive/photo with filesystem

modify'syncExif :: ObjId -> Cmd ()
modify'syncExif = forceSyncAllMetaData

-- import new subcollection of a collection in /archive/photo

modify'newSubCols :: ObjId -> Cmd ()
modify'newSubCols = syncCol' syncNewDirs

-- set checksum of an image part

modify'updateCheckSum :: CheckSum -> Name -> ObjId -> Cmd ()
modify'updateCheckSum cs n i = updateCheckSum i n cs

-- set timestamp of an image part

modify'updateTimeStamp :: TimeStamp -> Name -> ObjId -> Cmd ()
modify'updateTimeStamp ts n i = updateTimeStamp i n ts

-- ----------------------------------------
--
-- command for quering the catalog

-- read a whole collection

read'collection :: ImgNode -> Cmd ImgNodeP
read'collection n = mapObjId2Path n

-- access restrictions on a collection

read'isWriteable :: ImgNode -> Cmd Bool
read'isWriteable = return . isWriteableCol

read'isRemovable :: ImgNode -> Cmd Bool
read'isRemovable = return . isRemovableCol

read'isSortable :: ImgNode -> Cmd Bool
read'isSortable  = return . isSortableCol

-- --------------------
--
-- existence check of a collection

read'isCollection :: Path -> Cmd Bool
read'isCollection p =
  maybe False (isCOL . snd) <$> lookupByPath p

-- --------------------
--
-- get the contents of a blog entry, already converted to HTML

read'blogcontents :: Int -> ObjId -> ImgNode -> Cmd Text
read'blogcontents pos _i n
  | pos < 0   = maybe
                (return mempty)             -- return nothing, when not there
                getColBlogCont              -- else generate the HTML
                (n ^? theColBlog . traverse)

  | otherwise = processColEntryAt
                getColBlogCont                        -- ImgEnt: entry is a blog text
                (\ i' -> do n' <- getImgVal i'        -- theColBlog
                            read'blogcontents (-1) i' n'
                )
                pos n

-- get the contents of a blog entry

read'blogsource :: Int -> ObjId -> ImgNode -> Cmd Text
read'blogsource pos i n
  | pos < 0   = do br <- maybe
                         (abortP i "getBlogCont: no blog entry set in collection")
                         return
                         (n ^? theColBlog . traverse)
                   getColBlogSource br

  | otherwise = processColEntryAt
                getColBlogSource
                (\ i' -> do n' <- getImgVal i'
                            read'blogsource (-1) i' n'
                )
                pos
                n

-- --------------------
--
-- get the meta data of a collection entry

read'metadata :: Int -> ObjId -> ImgNode -> Cmd MetaData
read'metadata pos i n
  | pos < 0   = getMetaData i
  | otherwise = processColEntryAt
                (\ (ImgRef i' _) -> getMetaData i')
                (\ i'            -> getMetaData i')
                pos
                n

-- get the rating field of a collection entry

read'rating :: Int -> ObjId -> ImgNode -> Cmd Rating
read'rating pos i n =
  getRating <$> read'metadata pos i n

-- get the rating field of all entries in a collection

read'ratings :: ImgNode -> Cmd [Rating]
read'ratings n =
  traverse f (n ^. theColEntries . isoSeqList)
  where
    f = colEntry' (getR . _iref) getR
      where
        getR i' = getRating <$> getMetaData i'

read'checkImgPart :: Bool -> Path -> Name -> ImgNode -> Cmd CheckSumRes
read'checkImgPart p nm n = checkImgPart p nm n

-- --------------------

abortP :: ObjId -> String -> Cmd a
abortP i msg = do
  p <- objid2path i
  abort (msg ++ ": " ++ show p)

-- --------------------
--
-- a little helper for index calc

toPosList :: [Int] -> [Int]
toPosList xs =
  map snd
  . sortBy (compare `on` fst)
  . filter ((>= 0) . fst)
  . zip xs
  $ [0..]

toPosDescList :: [Int] -> [Int]
toPosDescList xs =
  reverse
  . map snd
  . filter ((>= 0) . fst)
  . zip xs
  $ [0..]

-- ----------------------------------------
