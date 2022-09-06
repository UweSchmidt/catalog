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

import Catalog.ImgTree.Access
import Catalog.ImgTree.Modify

import Catalog.Logging
       ( trc'Obj )

import Catalog.CopyRemove
       ( removeEmptyColls )

import Catalog.TimeStamp
       ( whatTimeIsIt )

import Data.ImgTree
import Data.MetaData
import Data.Prim

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
genCollectionRootMeta = withTF go
  where
    go t = adjustMetaData (defaultColMeta x s c o a <>) ic
      where
        ic = t ^. isoNavTree . theNode . theRootImgCol

        x = tt'collections
        s = ""
        c = ""
        o = ""
        a = no'delete .|. no'user

-- create the special collections for clipboard and trash

genClipboardCollection :: Eff'ISEJLT r => Sem r ()
genClipboardCollection =
  genSysCollection (no'delete .|. no'user) n'clipboard tt'clipboard

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
genSysCollection a n'sys tt'sys = withTF go
  where
    go t =
      case ex of
        Nothing ->
          void $ mkColByPath insertColByAppend (const $ mkColMeta' md) sp
        Just (i, _n) ->
          adjustMetaData (setAcc a) i
        where
          ic = t ^. isoNavTree . theNode . theRootImgCol
          pc = refPath ic t
          sp = pc `snocPath` n'sys
          ex = lookupImgPath sp t

          md = defaultColMeta x s c o a
            where
              x = tt'sys
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
-- The collections are updated only if the corresponding archive
-- dir is newer than the collection. This makes an update pretty fast

img2colPath :: Eff'ISE r => Sem r (Path -> Path)
img2colPath = liftTF go
  where
    go :: ImgTree -> (Path -> Path)
    go t = substPathPrefix old'px new'px
      where
        cr = t ^. isoNavTree . theNode . theRootImgCol
        pc = refPath cr t

        -- create root collection for archive dir hierachy
        (rootName, pc1) = pc  ^. viewTop
        (colName, _pc2) = pc1 ^. viewTop
        old'px          = mkPath rootName
        new'px          = rootName `consPath` mkPath colName

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
      | isempty b && n == n'photos =
          tt'photos ^. isoText
      | otherwise =
          n ^. isoText
      where
        (b, n) = p ^. viewBase

    path2Subtitle :: Path -> Text
    path2Subtitle = T.intercalate " \8594 " . listFromPath
    -- path names separated by right arrow

    genCol :: Eff'ISEJLT r => (Path -> Path) -> ObjId -> Sem r ColEntries
    genCol fp = go
      where
        go i = withTF $ \ t ->
          case (t, i) ^. theNode of
            n | isIMG n -> do
                  trc'Obj i $ "genCol img: " <> toText (ires ^. isoSeqList)
                  return ires

              | isDIR n -> do
                  trc'Obj i $ "genCol dir: " <> toText cp

                  -- check or create collection
                  -- with action for meta data
                  ic <- mkColByPath insertColByName setupDirCol cp

                  -- get collection entries, and insert them into collection
                  cs  <- fold <$> traverse go (n ^. theDirEntries)

                  trc'Obj ic "genCol dir: set dir contents"
                  adjustColByName cs ic
                  trc'Obj ic "genCol dir: dir contents is set"

                  -- set the blog entry, if there's a txt entry in cs
                  setColBlogToFstTxtEntry False ic
                  trc'Obj ic "genCol dir: col blog set"

                  return $ cres ic

              | otherwise ->
                  return mempty

              where
                cp :: Path
                cp = fp $ refPath i t

                ires :: ColEntries
                ires = isoSeqList #
                  (n ^.. theParts
                       . to (\pts -> pts ^.. thePartNamesI)
                       . to sort
                       . traverse
                       . to (mkColImgRef i)
                  )

                cres :: ObjId -> ColEntries
                cres ic' = isoSeqList # [mkColColRef ic']


-- ----------------------------------------
--
-- new adjust functions using pure ops to edit entries

adjustColBy' :: Eff'ISEJL r
             => (ImgTree -> ColEntries -> ColEntries)
             -> ColEntries
             -> ObjId
             -> Sem r ()
adjustColBy' sortCol cs parent'i =
  adjustColEntries' (sortMerge' cs sortCol) parent'i

-- --------------------
-- {- new pure

adjustColByName :: Eff'ISEJL r => ColEntries -> ObjId -> Sem r ()
adjustColByName = adjustColBy' sortByName

adjustColByDate :: Eff'ISEJL r => ColEntries -> ObjId -> Sem r ()
adjustColByDate = adjustColBy' sortByDate

insertColByName :: Eff'ISEJL r => ObjId -> ObjId -> Sem r ()
insertColByName cref = adjustColByName (Seq.singleton (mkColColRef cref))

insertColByAppend :: Eff'ISEJL r => ObjId -> ObjId -> Sem r ()
insertColByAppend i = adjustColEntries' $
                      const (<> Seq.singleton (mkColColRef i))

insertColByCons :: Eff'ISEJL r => ObjId -> ObjId -> Sem r ()
insertColByCons i = adjustColEntries' $
                    const (Seq.singleton (mkColColRef i) <>)
-- -}
-- --------------------
--
-- pure edit functions

sortColEntries :: forall a .
                  (ImgTree -> ColEntry -> a)
               -> (a -> a -> Ordering)
               -> ImgTree -> ColEntries -> ColEntries
sortColEntries getVal cmpVal t = go
  where
    go = fmap fst . Seq.sortBy (cmpVal `on` snd) . fmap mkC
      where
        mkC :: ColEntry -> (ColEntry, a)
        mkC ce =  (ce, ) $ getVal t ce

sortByName :: ImgTree -> ColEntries -> ColEntries
sortByName = sortColEntries getVal compare
  where
    getVal t =
      colEntry
      (\ j n1 -> (t, j) ^. theEntryName . to (\ n -> Right (n, n1)))
      (\ j    -> (t, j) ^. theEntryName . to         Left          )

sortByDate :: ImgTree -> ColEntries -> ColEntries
sortByDate = sortColEntries getVal compare
  where
    getVal t =
      colEntry
      (\ j n1 -> (t, j) ^. theNode
                         . to (\ n -> n ^. theMetaData)
                         . to (Right . (,n1) . lookupCreate parseTime)
      )
      (\ j    -> (t, j) ^. theEntryName
                         . to  Left
      )

sortMerge' :: ColEntries
           -> (ImgTree -> ColEntries -> ColEntries)
           -> ImgTree -> ColEntries -> ColEntries
sortMerge' cs'new sortCol t cs =
  sortCol t $ mergeColEntries cs cs'new
  where
    mergeColEntries :: ColEntries -> ColEntries -> ColEntries
    mergeColEntries es1 es2 =
      es1 <> Seq.filter (`notElem` es1) es2

-- ----------------------------------------

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
  when (isempty $ tailPath p) $
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
  unless (isempty es) $ do
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
  liftTF go
  where
    go :: ImgTree -> DateMap
    go t =
      foldl add1 IM.empty es
      where
        add1 :: DateMap -> ColEntry -> DateMap
        add1 acc ce =
          (maybe id ins mdate) acc
          where
            meta  = ce ^. theColObjId
                        . to (t,)
                        . theNode
                        . theMetaData

            mdate = meta ^? to (lookupCreate parseDate)
                        . traverse
                        . isoDateInt

            ins d = IM.insertWith (<>) d (Seq.singleton ce)

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
  unless (isempty es) $ do
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
modifyMetaDataRec mf = go
  where
    go i = withTF $ \ t ->
      case (t, i) ^. theNode of
        n | isCOL n -> do
              adjustMetaData mf i
              traverseOf_  (theColEntries . traverse . theColColRef) go n
          | otherwise ->
              return ()

-- ----------------------------------------
