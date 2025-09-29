------------------------------------------------------------------------------

module Catalog.Effects.CatCmd.Interpreter
where

-- catalog-polysemy
import Catalog.Effects
       ( Sem
       , InterpreterFor
       , Eff'ALL
       , Eff'ISE
       , Eff'ISEJL
       , EffCatEnv
       , EffError
       , EffFileSys
       , EffLogging
       , fileExist
       , readFileLB
       , writeFileLB
       , log'trc
       , interpret
       , ask
       , get
       , put
       , throw
       )
import Catalog.Effects.CatCmd
       ( CatCmd(..) )

import Catalog.GenCheckSum
       ( Eff'CheckSum )

import Catalog.CatalogIO
       ( Eff'CatIO )

import Catalog.CatEnv
       ( CatEnv
       , catNoSync
       )

import Catalog.GenCollections
       ( modifyMetaDataRec )

import Catalog.History
       ( addToUndoList
       , getFromUndoList
       , getWholeUndoList
       , dropFromUndoList
       )
import Catalog.Html
       ( Eff'Html )

import Catalog.ImgTree.Access
       ( getIdNode'
       , getId
       , getImgVal
       , getImgParent
       , getMetaData
       , getImgMetaData
       , findFstColEntry
       , lookupByPath
       , mapObjId2Path
       , objid2path
       , colEntryAt
       , processColEntryAt
       , processColImgEntryAt
       )
import Catalog.ImgTree.Modify
       ( adjustColEntries
       , adjustColImg
       , adjustColBlog
       , adjustMetaData
       , adjustPartMetaData
       , mkCollection
       )
import Catalog.Invariant
       ( checkImgStore )

import Catalog.Journal
       ( journal )

import Catalog.MetaData.Sync
       ( Eff'MDSync )

import Catalog.SyncWithFileSys
       ( Eff'Sync )

import Catalog.TextPath
       ( toFileSysPath )

import Catalog.TimeStamp
       ( whatTimeIsIt )

import Catalog.GenPages
       ( Req'
       , emptyReq'
       , processReqMediaPath
       , processReqImg
       , processReqPage
       , processReqJson
       , processReqJpg
       , rType
       , rGeo
       , rPathPos
       )

import qualified Catalog.CatalogIO       as IO
import qualified Catalog.CopyRemove      as CR
import qualified Catalog.GenCheckSum     as CS
import qualified Catalog.Html            as HT
import qualified Catalog.MetaData.Sync   as MS
import qualified Catalog.SyncWithFileSys as SC

-- catalog
import Data.Prim
       ( p'arch'photos
       , p'collections
       , p'photos
       , checkAndRemExt
       , isPathPrefix
       , msgPath
       , snocPath
       , substPathPrefix
       , viewBase
       , isoPathPos
       , CheckSum
       , CheckSumRes
       , Geo
       , Name
       , Path
       , ObjId
       , PathPos
       , ReqType(..)
       , TimeStamp
       )
import Data.Prim.Prelude
import Data.ImgNode
       ( colEntryM'
       , isCOL
       , isRemovableCol
       , isSortableCol
       , isUserCol
       , isWriteableCol
       , theColBlog
       , theColEntries
       , theColEntry
       , theColMeta
       , theColObjId
       , ImgRef
       , ImgRef'(ImgRef)
       )
import Data.Journal
       ( Journal'(..) )

import Data.MetaData
       ( MetaData
       , MetaDataT
       , MetaDataText
       , Rating
       , editMetaData
       , isoMetaDataMDT
       , splitMDT
       , colMDT
       , noshowMDT
       , showMDT
       , lookupCreate
       , lookupRating
       , mkRating
       , clearAccess
       , addNoWriteAccess
       , subNoWriteAccess
       )
import Data.ImageStore
       ( ImgStore )

import Data.ImgTree
       ( ColEntryM
       , ImgNode
       , ImgNodeP
       )
import Data.TextPath
       ( path2imgPath )

-- libraries
import qualified Data.Sequence   as Seq

------------------------------------------------------------------------
--
-- the catalog command interpreter

evalCatCmd :: Eff'ALL r => InterpreterFor CatCmd r
evalCatCmd =
  interpret $
  \ case
    SaveBlogSource pos t p ->
      path2node p >>= modify'saveblogsource pos t

    ChangeWriteProtected ixs ro p ->
      path2node p >>= modify'changeWriteProtected ixs ro

    SortCollection ixs p ->
      getIdNode' p >>= uncurry (modify'sort ixs)

    SortCollByDate ixs p ->
      getIdNode' p >>= uncurry (modify'sortByDate ixs)

    RemoveFromCollection ixs p ->
      getIdNode' p >>= uncurry (modify'removeFromCollection ixs)

    CopyToCollection ixs dst p ->
      path2node p >>= modify'copyToCollection ixs dst

    MoveToCollection ixs dst p ->
      getIdNode' p >>= uncurry (modify'moveToCollection ixs dst)

    SetCollectionImg sPath pos p ->
      path2id p >>= modify'colimg sPath pos

    SetCollectionBlog sPath pos p ->
      path2id p >>= modify'colblog sPath pos

    NewCollection nm p ->
      path2id p >>= modify'newcol nm

    RenameCollection nm p ->
      path2id p >>= modify'renamecol nm

    SetMetaData ixs md p ->
      getIdNode' p >>= uncurry (modify'setMetaData ixs md)

    SetMetaData1 pos md p ->
      getIdNode' p >>= uncurry (modify'setMetaData1 pos md)

    SetRating ixs r p ->
      path2node p >>= modify'setRating ixs r

    SetRating1 pos r p ->
      getIdNode' p >>= uncurry (modify'setRating1 pos r)

    Snapshot t _p ->
      modify'snapshot t

    SyncCollection p ->
      throwNoSync "sync collection" >>
      path2id p >>= modify'syncCol

    SyncExif recursive force p ->
      throwNoSync "sync EXIF metadata" >>
      path2id p >>= modify'syncExif recursive force

    NewSubCollections p ->
      throwNoSync "import new collection" >>
      path2id p >>= modify'newSubCols

    UpdateCheckSum cs n p ->
      throwNoSync "update checksum" >>
      path2id p >>= modify'updateCheckSum cs n

    UpdateTimeStamp ts n p ->
      path2id p >>= modify'updateTimeStamp ts n


    -- eval reading commands

    TheEntry p ->
      path2node p >>= read'collection

    IsWriteable p ->
      path2node p >>= read'isWriteable

    IsRemovable p ->
      path2node p >>= read'isRemovable

    IsSortable p ->
      path2node p >>= read'isSortable

    IsCollection p ->
      read'isCollection p

    TheBlogContents pos p ->
      getIdNode' p >>= uncurry (read'blogcontents pos)

    TheBlogSource pos p ->
      getIdNode' p >>= uncurry (read'blogsource pos)

    TheMetaDataText pos p ->
      getIdNode' p >>= uncurry (read'metadata pos)

    TheRating pos p ->
      getIdNode' p >>= uncurry (read'rating pos)

    TheRatings p ->
      path2node p >>= read'ratings

    TheMediaPath path
      | Just ppos <- path2colPath "" path -> do
          processReqMediaPath (mkReq RRef mempty ppos)

      | otherwise ->
          throw @Text $ msgPath path "illegal doc path "

    CheckImgPart onlyUpdate nm p ->
      path2node p >>= read'checkImgPart onlyUpdate p nm

    -- eval get commands

    StaticFile tp -> do
      readStaticFile (isoText # tp)

    JpgImgCopy rt geo path
      | Just ppos <- path2colPath ".jpg" path -> do
          p' <- processReqImg (mkReq rt geo ppos)
          fp <- toFileSysPath p'
          readFileLB fp

      | Just (imP, imN) <- path2imgPath (path ^. isoText) -> do
          p' <- processReqJpg (mkReq rt geo (imP, Nothing)) imN
          fp <- toFileSysPath p'
          readFileLB fp

      | otherwise ->
          throw @Text $ msgPath path "illegal doc path "

    HtmlPage rt geo path
      | Just ppos <- path2colPath ".html" path -> do
          processReqPage (mkReq rt geo ppos)

      | otherwise ->
          throw @Text $ msgPath path "illegal HTML doc path "

    JsonPage geo path
      | Just ppos <- path2colPath ".json" path -> do
          processReqJson (mkReq RJson geo ppos)

      | otherwise ->
          throw @Text $ msgPath path "illegal JSON doc path "

    -- eval undo commands

    NewUndoEntry cmt -> do
      hid <- get @ImgStore >>= addToUndoList cmt
      journal (NewUndo hid)
      return hid

    ApplyUndo hid -> do
      oldState <- getFromUndoList hid
      case oldState of
        Just s  -> do put @ImgStore s
                      journal (DoUndo hid)
        Nothing -> return ()

    DropUndoEntries hid -> do
      dropFromUndoList hid
      journal (DropUndo hid)

    ListUndoEntries -> do
      res <- getWholeUndoList
      log'trc ("listUnodEntries: " <> show res ^. isoText)
      return res

{-# INLINE evalCatCmd #-}

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
  (^. isoPathPos) <$> ( checkAndRemExt ext p
                        >>=
                        guarded (isPathPrefix p'collections)
                      )

readStaticFile :: (EffError r, EffLogging r, EffFileSys r, EffCatEnv r)
               => Path -> Sem r LazyByteString
readStaticFile srcPath = do
  tp <- toFileSysPath srcPath
  log'trc $ "readStaticFile: " <> tp
  ex <- fileExist tp
  if ex
    then readFileLB tp
    else
    do
      log'trc $ "readStaticFile: file not found: " <> tp
      throw @Text $ "document not found: " <> tp

writeStaticFile :: (EffError r, EffLogging r, EffFileSys r, EffCatEnv r)
                => Path -> LazyByteString -> Sem r ()
writeStaticFile dstPath lbs = do
  tp <- toFileSysPath dstPath
  log'trc $ "writeStaticFile: write to " <> tp
  writeFileLB tp lbs
  log'trc "writeStaticFile: finished "

-- --------------------

path2id :: Eff'ISE r => Path -> Sem r ObjId
path2id p = fst <$> getIdNode' p

path2node :: Eff'ISE r => Path -> Sem r ImgNode
path2node p = snd <$> getIdNode' p

{-# INLINE path2id #-}
{-# INLINE path2node #-}

------------------------------------------------------------------------
--
-- the "real" command interpretation functions
--
-- ----------------------------------------
--
-- commands for modifying the catalog

-- --------------------

modify'saveblogsource :: Eff'Html r => Int -> Text -> ImgNode -> Sem r ()
modify'saveblogsource pos t = putBlogCont t pos
  where
    putBlogCont :: Eff'Html r => Text -> Int -> ImgNode -> Sem r ()
    putBlogCont val =
      processColEntryAt
        (HT.putColBlogSource val)   -- change blog file
        putColBlog                  -- change blog entry of collection
        where
          putColBlog i = do
            n'  <- getImgVal i      -- the blog file of the collection
            br  <- maybe
                   (throwP i ("modify'saveblogsource: "
                              <> "no blog entry set in collection: "))
                   return
                   (n' ^? theColBlog . traverse)
            HT.putColBlogSource val br

-- --------------------
--
-- change the write protection for a list of collection entries

modify'changeWriteProtected :: Eff'ISEJL r => [Int] -> Bool -> ImgNode -> Sem r ()
modify'changeWriteProtected ixs ro n =
  traverse_ mark $ toPosList ixs
  where
    cs = n ^. theColEntries

    cf | ro        = addNoWriteAccess
       | otherwise = subNoWriteAccess

    mark pos = maybe (return ()) adj $ cs ^? ix pos
      where
        adj = colEntryM'
              (\ _ -> return ())            -- ignore ImgRef's
              adjMD

        adjMD i = do
          b <- isUserCol <$> getImgVal i
          unless b $
            throwP i
            "modify'changeWriteProtected: not allowed for system collections"
          adjustMetaData cf i

-- --------------------
--
-- sort a collection by sequence of positions
-- result is the new collection

modify'sort :: Eff'ISEJL r => [Int] -> ObjId -> ImgNode -> Sem r ()
modify'sort ixs i n
  | null ixs =
      return ()

  | otherwise = do
      unless (isSortableCol n) $
        throwP i "modify'sort: collection not sortable"

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
    cmp (mi, mp) (i1, p1) (i2, p2)
      | i1 == -1 && i2 == -1 = compare p1 p2
      | i1 == -1 && i2 >=  0 = compare p1 mp
      | i1 >= 0  && i2 == -1 = compare mp p2
      | i1 == mi && i2 >=  0 = LT
      | i1 >= 0  && i2 == mi = GT
      | i1 >= 0  && i2 >=  0 = compare i1 i2
      | otherwise          = EQ

-- sort entries of a collection by create date (makes sense only for image entries)
--
-- if nothing is marked, all entries are sorted,
-- else all entries marked, except the last one, are ordered by create date (if there)
-- and moved behind the last marked entries

modify'sortByDate :: Eff'ISEJL r => [Int] -> ObjId -> ImgNode -> Sem r ()
modify'sortByDate ixs0 i n
  | null ixs0 =
      return ()

  | otherwise = do
      unless (isSortableCol n) $
        throwP i "modify'sortByDate: collection not sortable"

      ds <- getCreateDates n
      adjustColEntries (reorderPartByDate ixMax $ Seq.zip ixPos ds) i
        where
          sortWholeCollection = all (-1 ==) ixs0

          ixs
            | sortWholeCollection = map (const 1) ixs0    -- mark all with mark cnt 1
            | otherwise           =               ixs0

          ixPos :: Seq (Int, Int)
          ixPos = isoSeqList # zip ixs [0..]

          ixMax :: (Int, Int)
          ixMax
            | sortWholeCollection = (0, 0)                -- no last mark occurs in list of marked pos
            | otherwise           = maximum ixPos         -- --> in cmpd: only 6. case (create date compare) occurs

getCreateDates :: Eff'ISE r => ImgNode -> Sem r (Seq Text)
getCreateDates n' =
  traverse
  (fmap (lookupCreate id) . colEntryM' getImgMetaData getMetaData)
  (n' ^. theColEntries)


reorderPartByDate :: (Int, Int) -> Seq ((Int, Int), Text) -> Seq a -> Seq a
reorderPartByDate (mi, mp) ixs cs =
  fmap snd . Seq.sortBy (cmpd `on` fst) $ Seq.zip ixs cs
  where
    cmpd :: ((Int, Int), Text) -> ((Int, Int), Text) -> Ordering
    cmpd ((i1, p1), t1) ((i2, p2), t2)
      | i1 == -1 && i2 == -1 = compare p1 p2    -- both  unmarked: compare positions
      | i1 == -1 && i2 >=  0 = compare p1 mp    -- first unmarked: snd   less or greater than position of last marked
      | i1 >=  0 && i2 == -1 = compare mp p2    -- snd   unmarked: first less or greater than position of last marked
      | i1 == mi && i2 >=  0 = LT               -- first is marked last: snd   is greater
      | i1 >=  0 && i2 == mi = GT               -- snd   is marked last: first is greater
      | i1 >=  0 && i2 >=  0 = compare t1 t2    -- both marked: compare create date, if missing (col entries), compare positions
                               <>
                               compare p1 p2
      | otherwise            = EQ

-- --------------------
--
-- remove all marked images and sub-collection from a collection

modify'removeFromCollection :: Eff'ISEJL r => [Int] -> ObjId -> ImgNode -> Sem r ()
modify'removeFromCollection ixs i n = do
  checkRemoveable "modify'removeToCollection" ixs i n
  removeFromCol ixs i n

removeFromCol :: Eff'ISEJL r => [Int] -> ObjId -> ImgNode -> Sem r ()
removeFromCol ixs oid n =
  traverse_ rmv $ toPosDescList ixs  -- remove list entries from the right
  where
    cs = n ^. theColEntries

    rmv :: Eff'ISEJL r => Int -> Sem r ()
    rmv pos = maybe (return ()) rm $ cs ^? ix pos
      where
        rm = colEntryM'
             (\ _ -> adjustColEntries (Seq.deleteAt pos) oid)
             CR.rmRec

checkMoveable :: Eff'ISE r => [Int] -> ImgNode -> Sem r Bool
checkMoveable ixs n =
  and <$> traverse check ixs
  where
    cs = n ^. theColEntries

    check :: Eff'ISE r => Int -> Sem r Bool
    check pos = maybe (return True) ck $ cs ^? ix pos
      where
        ck :: Eff'ISE r => ColEntryM -> Sem r Bool
        ck = colEntryM'
             (const $ return True)
             ckCol

        ckCol :: Eff'ISE r => ObjId -> Sem r Bool
        ckCol i = isRem <$> getImgVal i
          where
            --      (r ->) is an Applicative
            isRem = (&&) <$> isRemovableCol <*> isUserCol

        -- not write protected and not a system collection

-- --------------------
--
-- copy marked images and collections to another collection
--
-- the int list doesn't contain indexes but
-- ints which determine the order of copying
--
-- a -1 indicates "don't copy this image"
-- an int i >= 0 indicates "copy this as the i-th image"

modify'copyToCollection :: Eff'ISEJL r => [Int] -> Path -> ImgNode -> Sem r ()
modify'copyToCollection ixs dPath n = do
  di <- checkWriteableCol "modify'copyToCollection" dPath
  copyToCol ixs di n

copyToCol :: Eff'ISEJL r => [Int] -> ObjId -> ImgNode -> Sem r ()
copyToCol xs di n =
  traverse_ cpy $ toPosList xs
  where
    cs = n ^. theColEntries

    cpy pos = maybe (return ()) cp $ cs ^? ix pos
      where
        cp ce = colEntryM'
                (\ _ -> adjustColEntries (Seq.|> ce) di)
                copyColToCol
                ce
          where
            copyColToCol si = do
              dp <- objid2path di
              sp <- objid2path si
              CR.copyCollection sp dp
              -- remove the access restrictions in copied collection,
              -- in a copied collection there aren't any access restrictions
              --
              -- the path of the copied collection
              let tp = dp `snocPath` (sp ^. viewBase . _2)
              getId tp >>= modifyMetaDataRec clearAccess

checkWriteableCol :: Eff'ISE r => Text -> Path -> Sem r ObjId
checkWriteableCol jsonCall dPath = do
  (di, dn) <- getIdNode' dPath
  unless (isWriteableCol dn) $
    throwP di (jsonCall <> ": not a writeable collection")
  return di

checkRemoveable :: Eff'ISE r => Text -> [Int] -> ObjId -> ImgNode -> Sem r ()
checkRemoveable jsonCall ixs i n = do
  -- check whether source collection is writeable (contents may change)
  unless (isWriteableCol n) $
    throwP i (jsonCall <> ": this collection is write protected")

  -- check whether entries (collections are removable)
  unlessM (checkMoveable ixs n) $
    throwP i (jsonCall <> ": an entry may not be moved or deleted")

-- --------------------
--
-- move marked images and collections in a source col
-- to a dest col
-- this is implemented as a sequence of copy and remove

modify'moveToCollection :: Eff'ISEJL r
                        => [Int] -> Path -> ObjId -> ImgNode -> Sem r ()
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

modify'colimg :: Eff'ISEJL r => Path -> Int -> ObjId -> Sem r ()
modify'colimg = modifyCol adjustColImg

-- set or unset the collection blog text
-- i must reference a collection, not an image
-- pos must be an index to an ImgRef for a .md text

modify'colblog :: Eff'ISEJL r => Path -> Int -> ObjId -> Sem r ()
modify'colblog = modifyCol adjustColBlog


modifyCol :: Eff'ISEJL r
          => ((Maybe ImgRef -> Maybe ImgRef) -> ObjId -> Sem r ())
          -> Path -> Int -> ObjId -> Sem r ()
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

modify'newcol :: Eff'ISEJL r => Name -> ObjId -> Sem r ()
modify'newcol nm i = do
  path  <- objid2path i
  _newi <- mkCollection (path `snocPath` nm)
  return ()

-- --------------------
--
-- rename a sub-collection in a given collection

modify'renamecol :: Eff'ISEJL r => Name -> ObjId -> Sem r ()
modify'renamecol newName i = do
  iParent <- getImgParent i

  -- duplicate collection in parent collection
  CR.dupColRec i iParent newName

  -- find position of objid i in parent collection
  ps <- flip findFstColEntry iParent $
        \ ce -> return (i == ce ^. theColEntry . theColObjId)
  let pos = maybe (-1) fst ps

  -- remove i in parent collection
  CR.rmRec i

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

modify'setMetaData :: Eff'ISEJL r
                   => [Int] -> MetaDataText -> ObjId -> ImgNode -> Sem r ()
modify'setMetaData ixs mdt oid n =
  do
    -- modify img/col metadata
    modify'setMetaData'' ixs
      (editMetaData mdi) (editMetaData mdp) (editMetaData mdc) n

    -- modify ColEntyM metadata
    unless (isEmpty mds) $
      adjustColEntries (isoSeqList %~ addMeta) oid
  where
    md :: MetaDataT
    md         = isoMetaDataMDT # mdt
    mds        = showMDT md
    md1        = noshowMDT md
    (mdi, mdp) = splitMDT  md1
    mdc        = colMDT    md1

    addMeta :: [ColEntryM] -> [ColEntryM]
    addMeta =
      zipWith addm ixs'
      where
        ixs' = ixs <> repeat (-1)

        addm :: Int -> ColEntryM -> ColEntryM
        addm i ce
          | i < 0     = ce
          | otherwise = ce & theColMeta %~ editMetaData mds

modify'setMetaData'' :: Eff'ISEJL r
                     => [Int]
                     -> (MetaData -> MetaData)
                     -> (MetaData -> MetaData)
                     -> (MetaData -> MetaData)
                     -> ImgNode -> Sem r ()
modify'setMetaData'' ixs edi edp edc n =
  traverse_ setm $ toPosList ixs
  where
    cs = n ^. theColEntries

    setm pos = maybe (return ()) sm $ cs ^? ix pos
      where
        sm = colEntryM'
             adjustImgMetaData      -- img entry
             (adjustMetaData edc)   -- col entry

        adjustImgMetaData ir@(ImgRef i _nm) = do
          adjustPartMetaData edp ir
          adjustMetaData     edi i

-- set meta data fields for a collection or a single collection entry

modify'setMetaData1 :: Eff'ISEJL r
                    => Int -> MetaDataText -> ObjId -> ImgNode -> Sem r ()
modify'setMetaData1 pos md oid n
  | pos < 0   = adjustMetaData ed oid               -- update COL or IMG metadata
  | otherwise = modify'setMetaData ixs md oid n     -- update COL entry metadata
  where
    ed  = editMetaData (isoMetaDataMDT # md)
    ixs = replicate pos (negate 1) ++ [1]

-- set the rating field for a list of selected collection entries

modify'setRating :: Eff'ISEJL r => [Int] -> Rating -> ImgNode -> Sem r ()
modify'setRating ixs r =
  modify'setMetaData'' ixs id mr mr
  where
    mr = mkRating r

-- set the rating field for a collection or a single collection entry

modify'setRating1 :: Eff'ISEJL r
                  => Int -> Rating -> ObjId -> ImgNode -> Sem r ()
modify'setRating1 pos r oid n
  | pos < 0   = adjustMetaData (mkRating r) oid
  | otherwise = modify'setRating ixs r n
  where
    ixs = replicate pos (negate 1) ++ [1]

-- --------------------
--

-- save a snapshot of the current image store
-- on client side, the 1. arg must be a path to an existing node
-- simply take p'archive ("/archive"), the root node

modify'snapshot :: Eff'CatIO r => Text -> Sem r ()
modify'snapshot = IO.snapshotImgStore

-- --------------------
--
-- sync a subcollection of /archive/photo with filesystem

modify'syncCol :: Eff'Sync r => ObjId -> Sem r ()
modify'syncCol i = do
  ts <- whatTimeIsIt
  syncColDir (SC.syncDirP ts) i

syncColDir :: Eff'Sync r => (Path -> Sem r ()) -> ObjId -> Sem r ()
syncColDir sync i = do
  path <- objid2path i
  unless (isPathPrefix p'photos path) $
    throwP i $ msgPath p'photos "syncColDir: collection does not have path prefix "

  let path'dir = substPathPrefix p'photos p'arch'photos path
  sync path'dir
  checkImgStore

-- sync a subcollection of /archive/photo with filesystem

modify'syncExif :: Eff'MDSync r
                => Bool -> Bool -> ObjId -> Sem r ()
modify'syncExif = MS.syncTheMetaData

-- import new subcollection of a collection in /archive/photo

modify'newSubCols :: Eff'Sync r => ObjId -> Sem r ()
modify'newSubCols = syncColDir SC.syncNewDirs

-- set checksum of an image part

modify'updateCheckSum :: Eff'CheckSum r => CheckSum -> Name -> ObjId -> Sem r ()
modify'updateCheckSum cs n i = CS.updateCheckSum i n cs

-- set timestamp of an image part

modify'updateTimeStamp :: Eff'CheckSum r => TimeStamp -> Name -> ObjId -> Sem r ()
modify'updateTimeStamp ts n i = CS.updateTimeStamp i n ts

-- ----------------------------------------
--
-- command for quering the catalog

-- read a whole collection

read'collection :: Eff'ISE r => ImgNode -> Sem r ImgNodeP
read'collection = mapObjId2Path

-- access restrictions on a collection

read'isWriteable :: Eff'ISE r => ImgNode -> Sem r Bool
read'isWriteable = return . isWriteableCol

read'isRemovable :: Eff'ISE r => ImgNode -> Sem r Bool
read'isRemovable = return . isRemovableCol

read'isSortable :: Eff'ISE r => ImgNode -> Sem r Bool
read'isSortable  = return . isSortableCol

-- --------------------
--
-- existence check of a collection

read'isCollection :: Eff'ISE r => Path -> Sem r Bool
read'isCollection p =
  maybe False (isCOL . snd) <$> lookupByPath p

-- --------------------
--
-- get the contents of a blog entry, already converted to HTML

read'blogcontents :: Eff'Html r => Int -> ObjId -> ImgNode -> Sem r Text
read'blogcontents pos _i n
  | pos < 0   = maybe
                (return mempty)             -- return nothing, when not there
                HT.getColBlogCont              -- else generate the HTML
                (n ^? theColBlog . traverse)

  | otherwise = processColEntryAt
                HT.getColBlogCont                        -- ImgEnt: entry is a blog text
                (\ i' -> do n' <- getImgVal i'        -- theColBlog
                            read'blogcontents (-1) i' n'
                )
                pos n

-- get the contents of a blog entry

read'blogsource :: Eff'Html r => Int -> ObjId -> ImgNode -> Sem r Text
read'blogsource pos i n
  | pos < 0   = do br <- maybe
                         (throwP i "getBlogCont: no blog entry set in collection")
                         return
                         (n ^? theColBlog . traverse)
                   HT.getColBlogSource br

  | otherwise = processColEntryAt
                HT.getColBlogSource
                (\ i' -> do n' <- getImgVal i'
                            read'blogsource (-1) i' n'
                )
                pos
                n

-- --------------------
--
-- get the meta data of a collection entry

read'metadata' :: Eff'ISE r => Int -> ObjId -> ImgNode -> Sem r MetaData
read'metadata' pos i n
  -- get metadata from ObjId i
  | pos < 0   = getMetaData i

  -- reference the ColEntryM entry at position pos in collection n with ObjId i
  | otherwise = do
      md1 <- processColEntryAt getImgMetaData getMetaData pos n
      md2 <- (^. theColMeta) <$> colEntryAt pos n
      return $ md2 <> md1   -- ColEntry metadata overwrites img/col metadata

-- read'metadata'' :: Eff'ISE r => ColEntry -> Sem r MetaData
-- read'metadata'' =
--  colEntry' getImgMetaData getMetaData

read'metadata :: Eff'ISE r => Int -> ObjId -> ImgNode -> Sem r MetaDataText
read'metadata pos i n =
  (^. isoMetaDataMDT) <$> read'metadata' pos i n

-- get the rating field of a collection entry

read'rating :: Eff'ISE r => Int -> ObjId -> ImgNode -> Sem r Rating
read'rating pos i n =
  lookupRating <$> read'metadata' pos i n

-- get the rating field of all entries in a collection

read'ratings :: Eff'ISE r => ImgNode -> Sem r [Rating]
read'ratings n =
  traverse f (n ^. theColEntries . isoSeqList)
  where
    f ce = lookupRating <$> colEntryM' getImgMetaData getMetaData ce

read'checkImgPart :: Eff'CheckSum r
                  => Bool -> Path -> Name -> ImgNode -> Sem r CheckSumRes
read'checkImgPart = CS.checkImgPart

-- --------------------

throwP :: Eff'ISE r => ObjId -> Text -> Sem r a
throwP i msg = do
  p <- objid2path i
  throw @Text (msgPath p $ msg <> ": ")

throwNoSync :: (EffCatEnv r, EffError r) => Text -> Sem r ()
throwNoSync msg = do
  noSync <- (^. catNoSync) <$> ask @CatEnv
  if noSync
    then throw @Text (msg <> " is disabled by server option --no-catalog-sync")
    else return ()

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

------------------------------------------------------------------------
