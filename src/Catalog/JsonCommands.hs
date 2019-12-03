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

-- --------------------

modify'saveblogsource :: Int -> Text -> ImgNode -> Cmd ()
modify'saveblogsource pos t n = putBlogCont t pos n
  where
    putBlogCont :: Text -> Int -> ImgNode -> Cmd ()
    putBlogCont val =
      processColEntryAt
        (putColBlogSource val)
        (\ i    -> do
            n'            <- getImgVal i -- theColBlog
            br  <- maybe
                             ( do p <- objid2path i
                                  abort ("modify'saveblogsource: "
                                         ++ "no blog entry set in collection: "
                                         ++ p ^. isoString)
                             )
                             return
                             (n' ^? theColBlog . traverse)
            putColBlogSource val br
        )

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

modify'sort :: [Int] -> ObjId -> Cmd ()
modify'sort ixs i
  | null ixs =
      return ()
  | otherwise =
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
  -- check whether collection is readonly
  unless (isWriteable $ n ^. theColMetaData) $ do
    path <- objid2path i
    abort ("removeFrCol: collection is write protected: " ++ show path)
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
  di <- checkWriteableCol dPath
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

checkWriteableCol :: Path -> Cmd ObjId
checkWriteableCol dPath = do
  (di, dn) <- getIdNode' dPath
  unless (isCOL dn) $
    abort ("jsonCall: not a collection: " ++ show dPath)
  unless (isWriteable $ dn ^. theColMetaData) $
    abort ("jsonCall: collection is write protected: " ++ show dPath)
  return di

-- --------------------
--
-- move marked images and collections in a source col
-- to a dest col
-- this is implemented as a sequence of copy and remove

modify'moveToCollection :: [Int] -> Path -> ObjId -> ImgNode -> Cmd ()
modify'moveToCollection ixs dPath i n = do
  di <- checkWriteableCol dPath
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

-- set meta data fields for a single collection entry

modify'setMetaData1 :: Int -> MetaData -> ImgNode -> Cmd ()
modify'setMetaData1 i' =
  modify'setMetaData ixs
  where
    ixs = replicate i' (0-1) ++ [1]

-- set the rating field for a list of selected collection entries

modify'setRating :: [Int] -> Rating -> ImgNode -> Cmd ()
modify'setRating ixs r =
  modify'setMetaData ixs (mkRating r)

-- set the rating field for a single collection entry

modify'setRating1 :: Int -> Rating -> ImgNode -> Cmd ()
modify'setRating1 i' =
  modify'setRating ixs
  where
    ixs = replicate i' (0-1) ++ [1]

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
    abort ("syncCol': collection does not have path prefix "
           ++ quotePath p'photos ++ ": "
           ++ quotePath path)
  let path'dir = substPathPrefix p'photos p'arch'photos path
  verbose $ "syncCol': directory " ++ quotePath path'dir
  sync path'dir

-- sync a subcollection of /archive/photo with filesystem

modify'syncExif :: ObjId -> Cmd ()
modify'syncExif = forceSyncAllMetaData

-- import new subcollection of a collection in /archive/photo

modify'newSubCols :: ObjId -> Cmd ()
modify'newSubCols = syncCol' syncNewDirs

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

read'blogcontents :: Int -> ImgNode -> Cmd Text
read'blogcontents =
  processColEntryAt
    getColBlogCont                        -- ImgEnt: entry is a blog text
    (\ i    -> do                         -- ColRef: lookup the col blog ref
        n <- getImgVal i                  -- theColBlog
        maybe (return mempty)             -- return nothing, when not there
              getColBlogCont              -- else generate the HTML
              (n ^? theColBlog . traverse)
    )

-- get the contents of a blog entry

read'blogsource :: Int -> ImgNode -> Cmd Text
read'blogsource =
  processColEntryAt
    getColBlogSource
    (\ i -> do
        n  <- getImgVal i
        br <- maybe
              ( do p <- objid2path i
                   abort ("getBlogCont: no blog entry set in collection: "
                           ++ p ^. isoString)
              )
              return
              (n ^? theColBlog . traverse)
        getColBlogSource br
    )

-- --------------------
--
-- get the meta data of a collection entry

read'metadata :: Int -> ImgNode -> Cmd MetaData
read'metadata =
  processColEntryAt
    (\ (ImgRef i _) -> getMetaData i)
    (\ i            -> getMetaData i)

-- get the rating field of a collection entry

read'rating :: Int -> ImgNode -> Cmd Rating
read'rating pos n =
  getRating <$> read'metadata pos n

-- get the rating field of all entries in a collection

read'ratings :: ImgNode -> Cmd [Rating]
read'ratings n =
  traverse f (n ^. theColEntries . isoSeqList)
  where
    f = colEntry' (getR . _iref) getR
      where
        getR i' = getRating <$> getMetaData i'

-- ----------------------------------------
