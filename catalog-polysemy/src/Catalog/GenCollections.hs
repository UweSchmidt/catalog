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
       )
where

import Catalog.Effects
import Catalog.ImgTree.Fold
import Catalog.ImgTree.Access
import Catalog.ImgTree.Modify
import Catalog.Logging
import Catalog.CopyRemove     ( removeEmptyColls )
import Catalog.TimeStamp      ( whatTimeIsIt )

import Data.ImgTree
import Data.MetaData          ( MetaData
                              , metaTextAt
                              , lookupCreate
                              , parseDate
                              , parseTime
                              , isoDateInt

                              , descrAccess     -- meta keys
                              , descrComment
                              , descrCreateDate
                              , descrOrderedBy
                              , descrSubtitle
                              , descrTitle
                              )
import Data.Prim

import qualified Data.IntMap     as IM
import qualified Data.Sequence   as Seq
import qualified Data.Text       as T

-- ----------------------------------------
--
-- SemISEJLT: EffIStore, EffError, EffJournal, EffLogging, EffTime

genSysCollections :: Eff'ISEJLT r => Sem r ()
genSysCollections = do
  log'verb $
    "genSysCollections: create/update system collections"
    <> "(clipboard, albums, imports)"

  -- the collection root is already there
  -- just set the meta data
  genCollectionRootMeta
  genClipboardCollection     -- clipboard
  genPhotoCollection         -- hierachy for pictures imported from filesystem

genCollectionRootMeta :: Eff'ISEJL r => Sem r ()
genCollectionRootMeta = do
  ic <- getRootImgColId
  adjustMetaData (defaultColMeta t s c o a <>) ic
  where
    t = tt'collections
    s = ""
    c = ""
    o = ""
    a = no'delete

-- create the special collections for clipboard and trash

genClipboardCollection :: Eff'ISEJLT r => Sem r ()
genClipboardCollection = genSysCollection no'delete n'clipboard tt'clipboard

genPhotoCollection :: Eff'ISEJLT r => Sem r ()
genPhotoCollection = genSysCollection no'change n'photos tt'photos

-- genAlbumsCollection :: Eff'ISEJLT r => Sem r ()
-- genAlbumsCollection = genSysCollection no'restr n'albums tt'albums

genImportsCollection :: Eff'ISEJLT r => Sem r ()
genImportsCollection = genSysCollection no'change n'imports tt'imports

genByDateCollection :: Eff'ISEJLT r => Sem r ()
genByDateCollection = genSysCollection no'change n'bycreatedate tt'bydate

genSysCollection :: Eff'ISEJLT r => Text -> Name -> Text -> Sem r ()
genSysCollection a n'sys tt'sys = do
  ic <- getRootImgColId
  pc <- objid2path ic
  let sys'path = pc `snocPath` n'sys
  ex <- lookupByPath sys'path

  case ex of
    Nothing -> do
      void $ mkColByPath insertColByAppend (const $ mkColMeta' md) sys'path
    Just (i, _n) ->
      adjustMetaData (md <>) i
  where
    md = defaultColMeta t s c o a
      where
        t = tt'sys
        s = ""
        c = ""
        o = ""

-- create directory hierachy for Y/M/D
mkDateCol :: Eff'ISEJLT r => (String, String, String) -> Path -> Sem r (ObjId, ObjId, ObjId)
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
          a = no'change

    setupMonthCol y' m' _i = mkColMeta t "" "" o a
        where
          t = tt'month y' m'
          o = to'name
          a = no'change

    setupDayCol y' m' d' _i = mkColMeta t "" "" o a
        where
          t = tt'day y' m' d'
          o = to'dateandtime
          a = no'change

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
  void $ mkColByPath insertColByName setupDirCol (img2col dp)
  void $ genCol img2col di
  where

    -- meta data for generated collections
    setupDirCol :: Eff'ISEJLT r => ObjId -> Sem r MetaData
    setupDirCol i = do
      p  <- tailPath . tailPath <$> objid2path i
      let t = path2Title p
          s = path2Subtitle p
          o = to'colandname
          a = no'wrtdel
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
    genCol fp =
      foldImgDirs imgA dirA
      where
        -- collect all processed jpg images for a single img

        imgA :: Eff'ISEJLT r => ObjId -> ImgParts -> MetaData -> Sem r ColEntries
        imgA i pts _md = do
          trc'Obj i $ "genCol img: " <> toText res
          return $ isoSeqList # res
            where
              res = map (mkColImgRef i) $ sort (pts ^.. thePartNamesI)

        -- generate a coresponding collection with all entries
        -- entries are sorted by name

        dirA :: Eff'ISEJLT r => (ObjId -> Sem r ColEntries) ->
                ObjId -> DirEntries -> TimeStamp -> Sem r ColEntries
        dirA go i es _ts = do
          p  <- objid2path i
          let cp = fp p
          trc'Obj i $ "genCol dir " <> toText cp

          -- check or create collection
          -- with action for meta data
          ic <- mkColByPath insertColByName setupDirCol cp

          -- get collection entries, and insert them into collection
          cs  <- fold <$> traverse go (es ^. isoDirEntries)

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
      (\ j    ->         Left            <$> getImgName j)


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
      (\ j    -> Left <$> getImgName j )

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

-- insertColByDate :: ObjId -> ObjId -> SemISEJLT r ()
-- insertColByDate i = adjustColByDate [mkColColRef i]

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
          let ty = nd ^? theParts . isoImgPartsMap . ix n . theImgType
          return $ maybe False isTxt ty
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
    setEntry (pos, (ImgEnt ir)) = do
      log'trc $ "setColBlogToFstTxtEntry: " <> toText (i, pos, ir)
      adjustColBlog (const $ Just ir) i
      when rm $
        remColEntry pos i
    setEntry _ =
      return ()

-- ----------------------------------------

mkColMeta :: Eff'ISEJLT r
          => Text -> Text -> Text -> Text -> Text -> Sem r MetaData
mkColMeta t s c o a = mkColMeta' $ defaultColMeta t s c o a

mkColMeta' :: Eff'ISEJLT r => MetaData -> Sem r MetaData
mkColMeta' md0 = do
  tm <- timeStampToText <$> whatTimeIsIt
  let md = md0 & metaTextAt descrCreateDate .~ tm
  log'trc $ "mkColMeta: " <> toText md
  return md

defaultColMeta :: Text -> Text -> Text -> Text -> Text -> MetaData
defaultColMeta t s c o a =
  mempty
  & metaTextAt descrTitle      .~ t
  & metaTextAt descrSubtitle   .~ s
  & metaTextAt descrComment    .~ c
  & metaTextAt descrOrderedBy  .~ o
  & metaTextAt descrAccess     .~ a


-- create collections recursively, similar to 'mkdir -p'
mkColByPath :: Eff'ISEJLT r
            => (ObjId -> ObjId -> Sem r ())
            -> (ObjId -> Sem r MetaData)
            -> Path
            -> Sem r ObjId
mkColByPath insertCol setupCol p = do
  log'trc $ msgPath p "mkColByPath "

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
  log'trc $ msgPath p "mkColByPath' "
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
      log'verb $ msgPath p1 "mkColByPath" <> "/" <> toText n

      -- create collection
      ic <- mkImgCol ip n

      -- insert collection into parent collection
      insertCol ic ip
      return ic

    -- collection already there
    Just (ip, vp) -> do
      unless (isCOL vp) $
        throw @ Text $
        msgPath p  "mkColByPath: can't create collection, other entry already there"

      return ip

-- ----------------------------------------

type DateMap = IM.IntMap ColEntrySet

updateCollectionsByDate :: Eff'ISEJLT r => ColEntrySet -> Sem r ()
updateCollectionsByDate rs =
  unless (isempty rs) $ do
    log'verb $
       "updateCollectionsByDate: new refs are added to byDate collections: "
       <> toText rs

    genByDateCollection
    dm <- colEntries2dateMap rs
    dateMap2Collections p'bycreatedate dm

    log'verb "cleanup bydate collections"
    removeEmptyColls p'bycreatedate


-- group col entries by create date

colEntries2dateMap :: Eff'ISEL r => ColEntrySet -> Sem r DateMap
colEntries2dateMap rs = do
  log'verb "colEntries2dateMap: build DateMap"

  foldlM add1 IM.empty $ toSeqColEntrySet rs
  where

    add1 :: Eff'ISE r => DateMap -> ColEntry -> Sem r DateMap
    add1 acc ce = do
      meta <- getMetaData (ce ^. theColObjId)
      let mdate = (^. isoDateInt) <$> lookupCreate parseDate meta
      return $
        maybe acc
        (\ i' -> IM.insertWith (<>) i' (singletonColEntrySet ce) acc)
        mdate

-- create/update day collection with a col entry sets
-- pc is the path to the y/m/d collection hierachy

dateMap2Collections :: Eff'ISEJLT r => Path -> DateMap -> Sem r ()
dateMap2Collections pc dm =
  traverse_ insCol $ IM.toList dm
  where
    insCol (i, ces) = do
      (_yc, _mc, dc) <- mkDateCol ymd pc
      adjustColByDate cs dc
      log'verb $
        "dateMap2Collections: collection updated: " <> toText ymd
      return ()
      where
        ymd = isoDateInt # i
        cs  = toSeqColEntrySet ces

-- ----------------------------------------

updateImportsDir :: Eff'ISEJLT r => TimeStamp -> ColEntrySet -> Sem r ()
updateImportsDir ts ces =
  unless (isempty ces) $ do
    genImportsCollection
    idir <- mkImportCol ts p'imports
    adjustColByName (toSeqColEntrySet ces) idir
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

    setupImpCol _i = mkColMeta tsn'' "" "" to'name no'wrtsrt

-- ----------------------------------------
