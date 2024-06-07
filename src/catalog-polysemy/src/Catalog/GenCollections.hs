------------------------------------------------------------------------------

module Catalog.GenCollections
       ( genSysCollections
       , genImportsCollection
       , genAllCollectionsByDir
       , genCollectionsByDir
       , genCollectionsByDir'
       , updateCollectionsByDate
       , updateImportsDir
       , img2colPath
       , modifyMetaDataRec
       )
where

import Catalog.Effects
       ( Eff'ISEJL
       , Eff'ISEJLT
       , Eff'ISEL
       , Eff'ISE
       , Sem
       , throw
       , log'verb
       , log'trc
       )
import Catalog.ImgTree.Fold
       ( foldColColEntries
       , foldCollections
       , foldImgDirs
       )
import Catalog.ImgTree.Access
       ( getImgVal
       , findFstColEntry
       , mergeColEntries
       , getImgVals
       , getMetaData
       , getImgName
       , sortColEntries
       , getRootImgDirId
       , lookupByPath
       , objid2path
       , getRootImgColId
       )
import Catalog.ImgTree.Modify
       ( mkImgCol
       , remColEntry
       , adjustColBlog
       , adjustColEntries
       , adjustMetaData
       )
import Catalog.Logging
       ( trc'Obj )

import Catalog.CopyRemove
       ( removeEmptyColls )

import Catalog.TimeStamp
       ( whatTimeIsIt )

import Data.ImgTree
       ( theColObjId
       , isCOL
       , theMimeType
       , isoImgPartsMap
       , theParts
       , theColEntries
       , colEntry
       , mkColColRef
       , thePartNamesI
       , mkColImgRef
       , ColEntry
       , ColEntry'(ImgEnt)
       , DirEntries
       , ImgParts
       , ColEntries
       )
import Data.MetaData
       ( MetaData
       , Access

       , metaDataAt
       , metaTextAt
       , metaAcc

       , lookupCreate
       , parseDate
       , parseTime
       , isoDateInt

       , all'restr
       , no'restr
       , no'delete
       , no'write
       , no'sort
       , no'user
       , (.|.)

       , descrAccess     -- meta keys
       , descrComment
       , descrCreateDate
       , descrOrderedBy
       , descrSubtitle
       , descrTitle
       )
import Data.Prim
       ( Foldable(fold)
       , Ixed(ix)
       , TimeStamp
       , IsoText(isoText)
       , isEmpty
       , Path
       , Name
       , ObjId
       , Text
       , (.~)
       , (&)
       , (^?)
       , (^..)
       , (^.)
       , (#)
       , iso8601TimeStamp
       , formatTimeStamp
       , p'imports
       , traverse_
       , foldlM
       , p'bycreatedate
       , unless
       , msgPath
       , timeStampToText
       , when
       , isTxtMT
       , sort
       , isoSeqList
       , listFromPath
       , viewBase
       , to'colandname
       , tailPath
       , toText
       , substPathPrefix
       , consPath
       , mkPath
       , mkName
       , viewTop
       , to'dateandtime
       , tt'day
       , tt'month
       , to'name
       , tt'year
       , void
       , snocPath
       , tt'bydate
       , n'bycreatedate
       , tt'imports
       , n'imports
       , tt'photos
       , n'photos
       , tt'albums
       , n'albums
       , tt'clipboard
       , n'clipboard
       , tt'collections
       )

import qualified Data.IntMap     as IM
import qualified Data.Sequence   as Seq
import qualified Data.Text       as T (intercalate)

-- ----------------------------------------
--

genSysCollections :: Eff'ISEJLT r => Sem r ()
genSysCollections = do
  log'verb $
    "genSysCollections: create/update system collections"
    <> "(clipboard, photos, timeline, imports)"

  -- the collection root is already there
  -- just set the meta data
  genCollectionRootMeta
  genClipboardCollection     -- clipboard
  genAlbumsCollection        -- collections created by user
  genPhotoCollection         -- collection hierachy for images on filesystem
  genByDateCollection        -- timeline
  genImportsCollection       -- import history

genCollectionRootMeta :: Eff'ISEJL r => Sem r ()
genCollectionRootMeta = do
  ic <- getRootImgColId
  adjustMetaData (defaultColMeta t s c o a <>) ic
  where
    t = tt'collections
    s = ""
    c = ""
    o = ""
    a = no'delete .|. no'user

-- create the special collections for clipboard and trash

genClipboardCollection :: Eff'ISEJLT r => Sem r ()
genClipboardCollection =
  genSysCollection (no'delete .|. no'user)
  n'clipboard tt'clipboard

-- collection tree created by user
genAlbumsCollection :: Eff'ISEJLT r => Sem r ()
genAlbumsCollection = genSysCollection no'restr n'albums tt'albums

-- collection hierachy representing the photos hierachy on disk
genPhotoCollection :: Eff'ISEJLT r => Sem r ()
genPhotoCollection =
  genSysCollection all'restr n'photos tt'photos

-- import collection is writeable
-- to enable removing old imports
genImportsCollection :: Eff'ISEJLT r => Sem r ()
genImportsCollection =
  genSysCollection (no'delete .|. no'sort .|. no'user)
  n'imports tt'imports

genByDateCollection :: Eff'ISEJLT r => Sem r ()
genByDateCollection =
  genSysCollection all'restr n'bycreatedate tt'bydate

genSysCollection :: Eff'ISEJLT r => Access -> Name -> Text -> Sem r ()
genSysCollection a n'sys tt'sys = do
  ic <- getRootImgColId
  pc <- objid2path ic
  let sys'path = pc `snocPath` n'sys
  ex <- lookupByPath sys'path

  case ex of
    Nothing ->
      void $ mkColByPath insertColByAppend (const $ mkColMeta' md) sys'path
    Just (i, _n) ->
      adjustMetaData (setAcc a) i
  where
    md = defaultColMeta t s c o a
      where
        t = tt'sys
        s = ""
        c = ""
        o = ""

-- create directory hierachy for Y/M/D
mkDateCol :: Eff'ISEJLT r
          => (String, String, String) -> Path -> Sem r (ObjId, ObjId, ObjId)
mkDateCol (y, m, d) pc = do
  yc <- mkColByPath insertColByName (setupYearCol  y    ) py
  mc <- mkColByPath insertColByName (setupMonthCol y m  ) pm
  dc <- mkColByPath insertColByName (setupDayCol   y m d) pd
  return (yc, mc, dc)
  where
    py = pc `snocPath` mkName y
    pm = py `snocPath` mkName (y <> "-" <> m)
    pd = pm `snocPath` mkName (y <> "-" <> m <> "-" <> d)

    setupYearCol y' _i = mkColMeta t "" "" o a
        where
          t = tt'year y'
          o = to'name
          a = all'restr

    setupMonthCol y' m' _i = mkColMeta t "" "" o a
        where
          t = tt'month y' m'
          o = to'name
          a = all'restr

    setupDayCol y' m' d' _i = mkColMeta t "" "" o a
        where
          t = tt'day y' m' d'
          o = to'dateandtime
          a = all'restr

-- ----------------------------------------

-- gen collection for whole img hierachy
--
-- the collection of all images in archive is generated
-- in the collection root with the same name as the archive
-- root.
--
-- The collections are sorted by subcollections first and then by name
--
-- The collections are updatet only if the corresponding archive
-- dir is newer than the collection. This makes an update pretty fast

img2colPath :: Eff'ISE r => Sem r (Path -> Path)
img2colPath = do
  pc <- getRootImgColId >>= objid2path -- the collection root path

  -- create root collection for archive dir hierachy
  let (rootName, pc1) = pc  ^. viewTop
  let (colName, _pc2) = pc1 ^. viewTop
  let old'px  = mkPath rootName
  let new'px  = rootName `consPath` mkPath colName

  return $ substPathPrefix old'px new'px

genAllCollectionsByDir :: Eff'ISEJLT r => Sem r ()
genAllCollectionsByDir =
  getRootImgDirId >>= genCollectionsByDir


genCollectionsByDir' :: Eff'ISEJLT r => Path -> Sem r ()
genCollectionsByDir' p = do
  mbi <- lookupByPath p
  maybe (return ())
        (genCollectionsByDir . fst)
        mbi

genCollectionsByDir :: Eff'ISEJLT r => ObjId -> Sem r ()
genCollectionsByDir di = do
  img2col <- img2colPath
  dp      <- objid2path di
  let cp  = img2col dp
  log'trc $
    "genCollectionsByDir: create byDir collection for " <> toText cp

  void $ mkColByPath insertColByName setupDirCol cp
  void $ genCol img2col di

  log'trc $
    "genCollectionsByDir: remove empty bydir collections in" <> toText cp
  removeEmptyColls cp
  where

    -- meta data for generated collections
    setupDirCol :: Eff'ISEJLT r => ObjId -> Sem r MetaData
    setupDirCol i = do
      p  <- tailPath . tailPath <$> objid2path i
      let t = path2Title p
          s = path2Subtitle p
          o = to'colandname
          a = all'restr
      mkColMeta t s "" o a

    path2Title :: Path -> Text
    path2Title p
      | isEmpty b && n == n'photos =
          tt'photos ^. isoText
      | otherwise =
          n ^. isoText
      where
        (b, n) = p ^. viewBase

    path2Subtitle :: Path -> Text
    path2Subtitle = T.intercalate " \8594 " . listFromPath
    -- path names separated by right arrow

    genCol :: Eff'ISEJLT r => (Path -> Path) -> ObjId -> Sem r ColEntries
    genCol fp =
      foldImgDirs imgA dirA
      where
        -- collect all processed jpg images for a single img

        imgA :: Eff'ISEJLT r
             => ObjId -> ImgParts -> MetaData -> Sem r ColEntries
        imgA i pts _md = do
          trc'Obj i $ "genCol img: " <> toText res
          return $ isoSeqList # res
            where
              res = map (mkColImgRef i) $ sort (pts ^.. thePartNamesI)

        -- generate a coresponding collection with all entries
        -- entries are sorted by name

        dirA :: Eff'ISEJLT r
             => (ObjId -> Sem r ColEntries)
             -> ObjId -> DirEntries -> TimeStamp -> Sem r ColEntries
        dirA go i es _ts = do
          p  <- objid2path i
          let cp = fp p
          trc'Obj i $ "genCol dir " <> toText cp

          -- check or create collection
          -- with action for meta data
          ic <- mkColByPath insertColByName setupDirCol cp

          -- get collection entries, and insert them into collection
          cs  <- fold <$> traverse go es

          trc'Obj ic "genCol dir: set dir contents"
          adjustColByName cs ic
          trc'Obj ic "genCol dir: dir contents is set"

          -- set the blog entry, if there's a txt entry in cs
          setColBlogToFstTxtEntry False ic
          trc'Obj ic "genCol dir: col blog set"

          return $ Seq.singleton $ mkColColRef ic

-- ----------------------------------------
--
-- collection sort

sortByName :: Eff'ISE r => ColEntries -> Sem r ColEntries
sortByName =
  sortColEntries getVal compare
  where

    -- collections come first and are sorted by name
    -- images are sorted by name and part name
    getVal :: Eff'ISE r => ColEntry -> Sem r (Either Name (Name, Name))
    getVal =
      colEntry
      (\ j n1 -> (\ n -> Right (n, n1))  <$> getImgName j)
      (fmap Left . getImgName)


sortByDate :: Eff'ISE r => ColEntries -> Sem r ColEntries
sortByDate =
  sortColEntries getVal compare
  where
    -- collections come first, should be redundant,
    -- there should be only images in the collection of a day
    --
    -- the images are sorted by creation time and
    -- if that fails, by name

    getVal =
      colEntry
      (\ j n1 -> do
          md  <- getMetaData j
          let t = lookupCreate parseTime md
          return $ Right (t, n1)
      )
      (fmap Left . getImgName )

-- ----------------------------------------
--
-- set/modify collection entries


-- add a collection in front of a col entry list

insertColByCons :: Eff'ISEJL r => ObjId -> ObjId -> Sem r ()
insertColByCons i = adjustColEntries (Seq.singleton (mkColColRef i) <>)

-- add a collection at the end of a col entry list

insertColByAppend :: Eff'ISEJL r => ObjId -> ObjId -> Sem r ()
insertColByAppend i = adjustColEntries (<> Seq.singleton (mkColColRef i))

-- insert a collection and sort entries by name

insertColByName :: Eff'ISEJL r => ObjId -> ObjId -> Sem r ()
insertColByName i = adjustColByName $ Seq.singleton $ mkColColRef i

adjustColByName :: Eff'ISEJL r => ColEntries -> ObjId -> Sem r ()
adjustColByName = adjustColBy sortByName

adjustColByDate :: Eff'ISEJL r => ColEntries -> ObjId -> Sem r ()
adjustColByDate = adjustColBy sortByDate

adjustColBy :: Eff'ISEJL r => (ColEntries -> Sem r ColEntries) ->
               ColEntries ->
               ObjId -> Sem r ()
adjustColBy sortCol cs parent'i = do
  -- log'trc $ "adjustColBy begin"
  cs'old <- getImgVals parent'i theColEntries
  -- log'trc $ "adjustColBy" <> show cs'old
  cs'new <- sortCol $ cs'old `mergeColEntries` cs
  -- log'trc $ "adjustColBy" <> show cs'new
  adjustColEntries (const cs'new) parent'i
  -- log'trc $ "adjustColBy end"

-- ----------------------------------------

findFstTxtEntry :: Eff'ISEJL r => ObjId -> Sem r (Maybe (Int, ColEntry))
findFstTxtEntry = findFstColEntry isTxtEntry
  where
    isTxtEntry =
      colEntry
      (\ i n -> do
          nd <- getImgVal i
          let ty = nd ^? theParts . isoImgPartsMap . ix n . theMimeType
          return $ maybe False isTxtMT ty
      )
      (const $ return False)

-- take the 1. text entry in a collection
-- and set the collection blog entry to this value
-- rm indicates, whether the entry is removed from the collection

setColBlogToFstTxtEntry :: Eff'ISEJL r => Bool -> ObjId -> Sem r ()
setColBlogToFstTxtEntry rm i = do
  fte <- findFstTxtEntry i
  maybe (return ()) setEntry fte
  where
    setEntry (pos, ImgEnt ir) = do
      log'trc $ "setColBlogToFstTxtEntry: " <> toText (i, pos, ir)
      adjustColBlog (const $ Just ir) i
      when rm $
        remColEntry pos i
    setEntry _ =
      return ()

-- ----------------------------------------

mkColMeta :: Eff'ISEJLT r
          => Text -> Text -> Text -> Text -> Access -> Sem r MetaData
mkColMeta t s c o a = mkColMeta' $ defaultColMeta t s c o a

mkColMeta' :: Eff'ISEJLT r => MetaData -> Sem r MetaData
mkColMeta' md0 = do
  tm <- timeStampToText <$> whatTimeIsIt
  let md = md0 & metaTextAt descrCreateDate .~ tm
  log'trc $ "mkColMeta: " <> toText md
  return md

defaultColMeta :: Text -> Text -> Text -> Text -> Access -> MetaData
defaultColMeta t s c o a =
  mempty
  & metaTextAt descrTitle      .~ t
  & metaTextAt descrSubtitle   .~ s
  & metaTextAt descrComment    .~ c
  & metaTextAt descrOrderedBy  .~ o
  & setAcc                        a

setAcc :: Access -> MetaData -> MetaData
setAcc a md = md & metaDataAt descrAccess .~ metaAcc # a

-- create collections recursively, similar to 'mkdir -p'
mkColByPath :: Eff'ISEJLT r
            => (ObjId -> ObjId -> Sem r ())
            -> (ObjId -> Sem r MetaData)
            -> Path
            -> Sem r ObjId
mkColByPath insertCol setupCol p = do
  log'trc $ msgPath p "mkColByPath: "

  -- check for legal path
  cid <- mkColByPath' insertCol p

  -- meta data update always done,
  -- neccessary e.g. if access rights or title generation has been modified
  md <- setupCol cid
  adjustMetaData (md <>) cid
  return cid


mkColByPath' :: Eff'ISEJLT r
             => (ObjId -> ObjId -> Sem r ())
             -> Path
             -> Sem r ObjId
mkColByPath' insertCol p = do
  log'trc $ msgPath p "mkColByPath': "
  -- check for legal path
  when (isEmpty $ tailPath p) $
    throw @Text $ msgPath p "mkColByPath: can't create collection"

  mid <- lookupByPath p
  case mid of

    -- new collection
    Nothing -> do
      -- compute (create) parent collection(s)
      -- meta data of parent collections remains unchanged

      let (p1, n) = p ^. viewBase
      ip <- mkColByPath insertCol (const $ return mempty) p1
      log'trc $ msgPath p1 "mkColByPath" <> "/" <> toText n

      -- create collection
      ic <- mkImgCol ip n

      -- insert collection into parent collection
      insertCol ic ip
      return ic

    -- collection already there
    Just (ip, vp) -> do
      unless (isCOL vp) $
        throw @Text $
        msgPath p  "mkColByPath: can't create collection, other entry already there"

      return ip

-- ----------------------------------------

type DateMap = IM.IntMap ColEntries

updateCollectionsByDate :: Eff'ISEJLT r => ColEntries -> Sem r ()
updateCollectionsByDate es =
  unless (isEmpty es) $ do
    log'verb $
       "updateCollectionsByDate: new refs are added to byDate collections: "
       <> toText es

    genByDateCollection
    dm <- colEntries2dateMap es
    dateMap2Collections p'bycreatedate dm

    log'trc "remove empty bydate collections"
    removeEmptyColls p'bycreatedate


-- group col entries by create date

colEntries2dateMap :: Eff'ISEL r => ColEntries -> Sem r DateMap
colEntries2dateMap es = do
  log'trc "colEntries2dateMap: build DateMap"

  foldlM add1 IM.empty es
  where

    add1 :: Eff'ISE r => DateMap -> ColEntry -> Sem r DateMap
    add1 acc ce = do
      meta <- getMetaData (ce ^. theColObjId)
      let mdate = (^. isoDateInt) <$> lookupCreate parseDate meta
      return $
        maybe acc
        (\ i' -> IM.insertWith (<>) i' (Seq.singleton ce) acc)
        mdate

-- create/update day collection with a col entry sets
-- pc is the path to the y/m/d collection hierachy

dateMap2Collections :: Eff'ISEJLT r => Path -> DateMap -> Sem r ()
dateMap2Collections pc dm =
  traverse_ insCol $ IM.toList dm
  where
    insCol (i, ces) = do
      (_yc, _mc, dc) <- mkDateCol ymd pc
      adjustColByDate ces dc
      log'trc $
        "dateMap2Collections: collection updated: " <> toText ymd
      return ()
      where
        ymd = isoDateInt # i

-- ----------------------------------------

updateImportsDir :: Eff'ISEJLT r => TimeStamp -> ColEntries -> Sem r ()
updateImportsDir ts es =
  unless (isEmpty es) $ do
    genImportsCollection
    idir <- mkImportCol ts p'imports
    adjustColByName es idir
    return ()

-- create import collection, if dir not yet there

mkImportCol :: Eff'ISEJLT r => TimeStamp -> Path -> Sem r ObjId
mkImportCol ts pc = do
  mid <- lookupByPath tsp
  case mid of
    Nothing ->
      mkColByPath insertColByCons setupImpCol tsp
    Just (ip, _vp) ->
      return ip

  where
    tsn   = formatTimeStamp  ts                    -- 2020-05-13 12:50:42
    tsn'  = mkName $ iso8601TimeStamp ts           -- 2020-05-13T12:50:42
    tsn'' = ("Import " <> tsn) ^. isoText          -- Import 2020-05-13 12:50:42
    tsp   = pc `snocPath` tsn'

    setupImpCol _i =
      mkColMeta tsn'' "" "" to'name (no'write .|. no'sort)

-- ----------------------------------------

modifyMetaDataRec :: Eff'ISEJL r => (MetaData -> MetaData) -> ObjId -> Sem r ()
modifyMetaDataRec mf =
  foldCollections colA
  where
    colA go i md im be cs = do
      adjustMetaData mf i
      foldColColEntries go i md im be cs

-- ----------------------------------------
