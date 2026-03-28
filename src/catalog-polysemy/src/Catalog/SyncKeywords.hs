------------------------------------------------------------------------------
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Catalog.SyncKeywords
  ( syncKeywordCol
  , syncAllKeywordCols
  , allAlbumColsWithRef
  , allKeywords
  , allKeywordCols
  , allKeywordColsM
  , newKeywordCols
  , sortColEntriesByDate
  , Keywords
  , KeywordCols
  )
where

import Catalog.CopyRemove
       ( rmRec
       , dupColNoRec
       )
import Catalog.Effects
       ( Eff'ISEJL
       , Eff'ISEL
       , Eff'ISE
       , Sem
       , log'trc
       , log'dbg
       )
import Catalog.ImgTree.Access
       ( objid2path
       , lookupByPath
       , getImgName
       , getImgVal
       , getImgVals
       , getMetaData
       , getImgMetaData
       , getColRefMetaData
       , getId
       , sortAugmentedColEntries
       )
import Catalog.ImgTree.Fold
       ( foldCollections
       , foldColColEntries
       )

import Catalog.ImgTree.Modify
       ( adjustColEntries
       , adjustColImg
       , adjustMetaData
       , mkCollection
       )

import Data.ImgNode
       ( ObjIds )

import Data.ImgTree
       ( ColEntries
       , ColEntryM
       , ImgNode
       , ImgRef
       , ImgRef'(ImgRef)
       , colEntryM'
       , colEntryM
       , isColColRef
       , mkColColRefM
       , mkColImgRefM'
       , theColEntries
       , theColEntry
       , theColObjId
       , theColImg
       , theColImgRef
       , theMetaData
       )
import Data.MetaData
       -- ( MetaData )

import Data.Prim

import Text.ParsePretty
       ( toYMD
       , fmtDate
       , fmtKWTitle
       , fmtKWSubTitle
       , fmtYMDRange
       )

import Data.Sequence
       ( Seq( (:|>)
            , (:<|)
            )
       )

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Sequence   as Seq
import qualified Data.Text       as T

-- ----------------------------------------
--
-- search collection tree "colId" for
-- occurences of img ref ir
-- and collect col ids

allColsWithImgRef :: Eff'ISE r => ObjId -> ImgRef -> Sem r ObjIds
allColsWithImgRef colId ir = do
  foldCollections colA colId
  where
   colA go i _md _im _be cs = do
     fold <$> traverse (colEntryM' iref go) cs
       where
         iref ir' = do
           let res
                 | ir' == ir = S.singleton i
                 | otherwise =  mempty
           return res

allAlbumColsWithRef :: (Eff'ISEL r) => Path -> Path -> Name -> Sem r [(Path, Text)]
allAlbumColsWithRef colp p nm = do
  log'trc $
    "allAlbumColsWithRef: collection = "
    <> colp ^. isoText
    <> ", img = "  <> p  ^. isoText
    <> ", part = " <> nm ^. isoText

  rc <- lookupByPath colp
  ri <- lookupByPath p
  case (rc, ri) of
    (Just (ci, _), Just (i, _)) -> do
      colIds <- allColsWithImgRef ci (ImgRef i nm)
      traverse toPathTitle $ S.toList colIds

    _notFound -> do
      log'trc "allAlbumColsWithRef: argumet paths unknown"
      return []
  where
    toPathTitle i' = do
      p'  <- objid2path i'
      md' <- getMetaData i'
      return (p', md' ^. metaTextAt descrTitle)

-- ----------------------------------------

type Keywords    = Set Text
type KeywordCols = Map Text Path

-- global constants

kwSuffix :: Text           -- suffix for generated keyword collections
kwSuffix = ".keyword"

-- hidden keyword col name are used in very large
-- keyword collections to partition the entries into subcollections
-- or
-- for copies of collections labeled with keywords

isHiddenKWName :: Text -> Bool
isHiddenKWName n =
  isEmpty n
  ||
  T.take 1 n == "."
  ||
  T.isSuffixOf kwSuffix n

notToBeIndexed :: MetaData -> Bool
notToBeIndexed md =
  not (isIndexable md)

kwList :: MetaData -> [Text]
kwList md =
  md ^. metaDataAt descrKeywords     . metaTS
  <>
  md ^. metaDataAt descrKeywordsCopy . metaTS

kwSet :: MetaData -> Keywords
kwSet md = S.fromList $ md ^. metaDataAt descrKeywords . metaTS

syncAllKeywordCols :: Eff'ISEJL r => Int -> Sem r ()
syncAllKeywordCols maxImgEntries =
  getId p'keywords >>= syncKeywordCol maxImgEntries

-- --------------------

allKeywordCols :: (Eff'ISEL r) => Sem r Keywords
allKeywordCols = toKWS <$> allKeywordColsM (const True)
  where
    toKWS = S.fromList . M.keys

allKeywordColsM :: (Eff'ISEL r) => (Text -> Bool) -> Sem r KeywordCols
allKeywordColsM sel = do
  getId p'keywords >>= allKeywordColsM' sel

allKeywordColsM' :: (Eff'ISEL r) => (Text -> Bool) -> ObjId -> Sem r KeywordCols
allKeywordColsM' sel i0 = do
  foldCollections colA i0
  where
    colA go i _md _im _be cs = do
      kws1 <- do
        n' <- (^. isoText) <$> getImgName i
        if isHiddenKWName n'
           ||
           not (sel n')  -- kw not of interest, filtered out
          then
            return mempty
          else do
            p' <- objid2path i
            return $ M.singleton n' p'
      kws2 <- foldColColEntries go cs
      return $ M.union kws1 kws2

-- --------------------

allKeywords :: Eff'ISEL r => Sem r Keywords
allKeywords = getId p'albums >>=  allKeywords'

allKeywords' :: Eff'ISEL r => ObjId -> Sem r Keywords
allKeywords' i = do
  kws <- foldCollections colA i

  log'trc $ "allKeywords: " <> T.intercalate ", " (S.toAscList kws)
  return kws
  where
    colA go _i md _im _be cs = do
      let kws1 = S.fromList $ kwList md                      -- col keywords
      kws2 <- if notToBeIndexed md
              then
                return mempty                                -- stop looking for keywords
              else
                fold <$> traverse (colEntryM' iref go) cs    -- keywords in col entries
      return (S.union kws1 kws2)
      where
        iref ir' = (S.fromList . kwList) <$> getImgMetaData ir'

-- create keyword collection for new keywords
-- these are created in a subcollection of the
-- root collection for keywords "keywords""
newKeywordCols :: (Eff'ISEJL r) => Sem r ()
newKeywordCols = do
  kws     <- allKeywords
  kwcs    <- allKeywordCols
  let kws' = S.difference kws kwcs

  unless (S.null kws') $ do
    unlessM (isJust <$> lookupByPath pkn) $ do
      ci <- mkCollection pkn
      adjustMetaData (\md -> md & metaTextAt descrTitle .~ newCt) ci

    traverse_ initKeywordCol kws'
  where
    newCt  = "Neue Schlüsselwörter"
    newKws = "new"
    pkn    = p'keywords `snocPath` newKws

    initKeywordCol :: Eff'ISEJL r => Text -> Sem r ()
    initKeywordCol kw = do
      log'trc $ "newKeywords: init new keyword collection: " <> p ^. isoUrlText
      _ci <- mkCollection p
      return ()
      where
        p = pkn `snocPath` (isoText # kw)

cleanupKeywordCol :: Eff'ISEJL r => ObjId -> ImgNode -> Sem r ()
cleanupKeywordCol i n0 = do
  -- remove kw subcollections
  foldColColEntries remHidden $ n0 ^. theColEntries

  -- remove ImgRefs
  adjustColEntries (Seq.filter (\r -> isColColRef $ r ^. theColEntry)) i
  where
    remHidden :: (Eff'ISEJL r) => ObjId -> Sem r ()
    remHidden i' = do
      n' <- (^. isoText) <$> getImgName i'
      when (isHiddenKWName n') $
        rmRec i'

sortColEntriesByDate :: (Eff'ISE r) => ColEntries -> Sem r ColEntries
sortColEntriesByDate cs =
  sortAugmentedColEntries (cmpDate <> cmpImgPart)
  <$>
  augmentM getColRefMetaData cs

-- sort by createDate in metadata

cmpDate :: (a, MetaData) -> (a, MetaData) -> Ordering
cmpDate = compare `on` (\(_ce, md') -> lookupCreate id md')

-- collection entries first, then img entries

_cmpColImg :: (ColEntryM, m) -> (ColEntryM, m) -> Ordering
_cmpColImg = compare `on` (\(ce, _md) -> colEntryM (const $ const True) (const False) ce)

-- compare img part names

cmpImgPart :: (ColEntryM, m) -> (ColEntryM, m) -> Ordering
cmpImgPart = compare `on` (\(ce, _md) -> colEntryM (\_i nm -> nm) (const mempty) ce)


addColRefsToKeywordCol :: (Eff'ISEJL r) => ObjId -> ColEntries -> Sem r ()
addColRefsToKeywordCol i rs0 = do
  rs1 <- sortColEntriesByDate rs0
  void $ Seq.traverseWithIndex
    ( \i' r' -> do
        let ci = r' ^. theColEntry . theColObjId
        let px = (show i' <> ".") ^. isoText
        cp <- objid2path ci
        cn <- (\n -> (isoText # px) <> n <> (isoText # kwSuffix)) <$> getImgName ci
        cd <- dupColNoRec ci i cn
        adjustMetaData (\md -> md & metaTextAt descrCollectionRef .~ cp ^. isoText) cd

        -- set collection img, if not already set
        cv <- getImgVal ci
        adjustColImg (<|> join (cv ^? theColImg)) i
    )
    rs1

addDate :: Eff'ISE r => ColEntryM -> Sem r (Tuple3 Int)
addDate cr' = lookupCreate toYMD <$> getColRefMetaData cr'

sortKWColByDate :: Eff'ISEJL r => ObjId -> Sem r ()
sortKWColByDate i =
  -- if the kw collection allows sorting
  -- the subcollections are ordered by create date

  whenM (isKWsortable <$> getMetaData i) $
    do
      rs'old <- getImgVals i theColEntries
      rs'new <- sortColEntriesByDate rs'old
      adjustColEntries (const rs'new) i


addImgRefsToKeywordCol :: Eff'ISEJL r => Int -> Text -> Bool -> ObjId -> ColEntries -> Sem r ()
addImgRefsToKeywordCol maxImgEntries kw forceSubCol i rs0 = do
  -- sort enties by create date
  rs1 <- sortColEntriesByDate rs0

  let (fst' :<| _  )  = rs1
  let (_    :|> lst') = rs1

  fr'       <- addDate fst'
  to'       <- addDate lst'
  limited   <- isKWlimited <$> getMetaData i

  let mxe
        | limited   = maxImgEntries
        | otherwise = maxBound

  let noSplit =
        not limited                      -- unlimited col size
        ||
        maxImgEntries <= 0               -- unlimited col size
        ||
        fr' == to'                       -- all images on the same day
        ||
        Seq.length rs1 <= mxe            -- # images fits into a single col

  if forceSubCol || not noSplit
    then
      do
        splitIntoSubCols mxe kw i rs1
        sortKWColByDate i
    else
      do
        let colTitle        = ": " <> fmtYMDRange fr' to'
        let colCreateDate   = fmtDate fr'

        adjustMetaData   (\md -> md & metaTextAt descrSubtitle   %~ (<> colTitle)
                                    & metaTextAt descrCreateDate .~ colCreateDate
                         ) i
        adjustColEntries (<> rs1) i

  -- set collection img, if not already set
  adjustColImg (<|> cImg rs1) i

  where
    cImg :: ColEntries -> Maybe ImgRef
    cImg rs' = rs' ^? ix 0 . theColEntry . theColImgRef

-- ----------------------------------------

splitIntoSubCols :: (Eff'ISEJL r) => Int -> Text -> ObjId -> ColEntries -> Sem r ()
splitIntoSubCols maxImgEntries kw i rs = do
  p <- objid2path i
  log'trc $ "addImgRefsToKeywordCol: split keyword col in subcols: " <> p ^. isoUrlText

  let mxi | maxImgEntries <= 0 = maxBound
          | otherwise          = maxImgEntries

  let groupCols =
        zip [1..]
        . map fst
        . group (tooLargeAuE mxi) distAuE mergeAuE
        . toAugDist
        . map toAugEntry

  grouped'rs <- groupCols <$> augmentM addDate (foldr (:) [] rs)

  traverse_ (uncurry $ addCol p) grouped'rs
    where
      addCol :: Eff'ISEJL r => Path -> Int -> AugColEntries -> Sem r ()
      addCol p' ix' (rs', (fr', to')) = do
        let colName     = isoText # (ix' ^. isoText <> ".group-" <> kw <> kwSuffix)
        let colTitle    = fmtKWTitle fr' to' kw
        let colSubTitle = fmtKWSubTitle (0, Seq.length rs' ,0)
        let colCreateDt = fmtDate fr'
        let colPath     = p' `snocPath` colName

        log'trc $ "addImgRefsToKeywordCol: add subcol: " <> colPath ^. isoUrlText

        ci             <- mkCollection colPath
        adjustColEntries  (const rs') ci
        adjustMetaData    (\md -> md & metaTextAt descrTitle      .~ colTitle
                                     & metaTextAt descrSubtitle   .~ colSubTitle
                                     & metaTextAt descrCreateDate .~ colCreateDt
                          ) ci
        adjustColImg      (const colImg) ci
          where
            colImg :: Maybe ImgRef
            colImg = rs' ^? ix 0 . theColEntry . theColImgRef

type Date           = Tuple3 Int
type AugColEntries  = (ColEntries, (Date, Date))
type AugColEntriesD = (AugColEntries, Int)

mergeAuE :: AugColEntriesD -> AugColEntriesD -> AugColEntriesD
mergeAuE ((cs1, (day11, _) ), _d1) ((cs2, (_, day22) ), d2) =
  ((cs1 <> cs2, (day11, day22)), d2)

tooLargeAuE :: Int -> AugColEntriesD -> Bool
tooLargeAuE maxAuE ((cs, _), _) = Seq.length cs >= maxAuE

distAuE :: AugColEntriesD -> Int
distAuE = snd

toAugEntry :: (ColEntryM, Date) -> AugColEntries
toAugEntry (ce, d) = (Seq.singleton ce, (d, d))

toAugDist :: [AugColEntries] -> [AugColEntriesD]
toAugDist (x : xs1@(x1 : _xs2)) = (x, dist x x1) : toAugDist xs1
  where
    dist e1 e2 = (toDays . fst . snd $ e2) - (toDays . fst . snd $ e1)
      where
        toDays (y, m, d) = y * 360 + m * 30 + d

toAugDist (x : [])              = (x, maxBound)  : []
toAugDist []                    = []

-- ----------------------------------------

syncKeywordCol :: Eff'ISEJL r => Int -> ObjId -> Sem r ()
syncKeywordCol maxImgEntries i = do
  p <- objid2path i
  log'trc $ "syncKeywordCol: path = " <> p ^.isoText <> ", maxImgEntries = " <> i ^. isoText

  -- collect all keywords to be updated
  kws <- allKeywordColsM' (const True) i
  log'KeywordCols kws

  -- collect all references for these keywords
  rfm <- getId p'albums >>= buildRefsMap (`M.member` kws)
  log'RefsMap rfm

  -- build new keyword collections
  void $ M.traverseWithKey (updateKeywordCol maxImgEntries rfm) kws

updateKeywordCol :: Eff'ISEJL r => Int -> RefsMap -> Text -> Path -> Sem r ()
updateKeywordCol maxImgEntries rfm kw p = do
  log'trc $ "updateKeywordCol: update keyword collection for " <> p ^. isoUrlText

  i  <- getId p
  n0 <- getImgVal i

  -- set collection title if not yet set
  when (T.null $ n0 ^. theMetaData . metaTextAt descrTitle) $
    adjustMetaData (\md -> md & metaTextAt descrTitle .~ kw) i

  -- after cleanup only real (not generated) keyword subcollections remain in i
  cleanupKeywordCol i n0

  -- n0 is no longer up do date
  n1 <- getImgVal i
  let es        = n1 ^. theColEntries
  let subColCnt = Seq.length es

  -- get image and collection refs containing keyword
  let _refs@(imgRefs, colRefs) = lookupRefsMap kw rfm
  let imgCnt = S.size imgRefs
  let colCnt = S.size colRefs

  -- log'Refs _refs

  setKWMeta (subColCnt, imgCnt, colCnt) i

  when (colCnt > 0) $ do
    let colEnts = foldMap (Seq.singleton . mkColColRefM) colRefs
    addColRefsToKeywordCol i colEnts

  when (imgCnt > 0) $ do
    let forceSubCol = subColCnt > 0 || colCnt > 0
    let imgEnts = foldMap (Seq.singleton . mkColImgRefM') imgRefs
    addImgRefsToKeywordCol maxImgEntries kw forceSubCol i imgEnts

  when (subColCnt > 0 && imgCnt == 0) $ do
    sortKWColByDate i

  log'dbg $ "updateKeywordCol: keyword collection update for " <> kw <> " finished"

  return ()

-- --------------------

setKWMeta :: Eff'ISEJL r => (Int, Int, Int) -> ObjId -> Sem r ()
setKWMeta cs i = do
  adjustMetaData addSub i
  where
    addSub md =
      md & metaTextAt descrSubtitle .~ fmtKWSubTitle cs

-- --------------------

newtype Refs = Refs (Set ImgRef, Set ObjId)

instance Semigroup Refs where
  (<>) :: Refs -> Refs -> Refs
  Refs (s11, s12) <> Refs (s21, s22) = Refs (S.union s11 s21, S.union s12 s22)

instance Monoid Refs where
  mempty :: Refs
  mempty = Refs (mempty, mempty)

newtype RefsMap = RM (Map Text Refs)

instance Semigroup RefsMap where
  (<>) :: RefsMap -> RefsMap -> RefsMap
  RM m1 <> RM m2 = RM $ M.unionWith(<>) m1 m2

instance Monoid RefsMap where
  mempty :: RefsMap
  mempty = RM M.empty

mkRefsImg :: Text -> ImgRef -> RefsMap
mkRefsImg kw ir = RM $ M.singleton kw $ Refs (S.singleton ir, mempty)

mkRefsCol :: Text -> ObjId -> RefsMap
mkRefsCol kw cr = RM $ M.singleton kw $ Refs (mempty, S.singleton cr)

lookupRefsMap :: Text -> RefsMap -> (Set ImgRef, Set ObjId)
lookupRefsMap kw (RM m) =
  maybe (mempty, mempty) (\(Refs p) -> p) $ M.lookup kw m

-- single traversal for collection of all references
-- for all keywords matching predicate "matchKeyword"

buildRefsMap :: Eff'ISEL r => (Text -> Bool) -> ObjId -> Sem r RefsMap
buildRefsMap matchKeyword i0 =
  foldCollections colA i0
  where
    colA _go i md _im _be cs = do
      -- p'' <- objid2path i
      -- log'trc $ "buildRefsMap: p = " <> p'' ^.isoText

      rm2 <-
        if notToBeIndexed md
        then
          return mempty
        else do
          addKW

      -- log'trc "this collection"
      -- log'RefsMap rm1
      -- log'trc "sub collections"
      -- log'RefsMap rm2
      -- log'trc $ "done" <> p'' ^. isoText

      return (rm1 <> rm2)
      where
        kws :: Keywords
        kws  = kwSet md

        rm1 :: RefsMap              -- collect col ref for all keywords
        rm1  = foldMap ( \kw' ->
                           if matchKeyword kw'
                           then mkRefsCol kw' i
                           else mempty
                       ) kws

        -- addKW :: Eff'ISE r => Text -> Sem r RefsMap
        -- traverse col entries
        addKW =
          fold <$> traverse (colEntryM' iref go') cs
          where
            go' = buildRefsMap matchKeyword'

            matchKeyword' :: Text -> Bool
            matchKeyword' kw' =
              not (kw' `elem` kws)
              &&
              matchKeyword kw'

            -- collect img ref for all keywords
            iref :: (Eff'ISE r) => ImgRef -> Sem r RefsMap
            iref ir = do
              kws' <- kwSet <$> getImgMetaData ir
              return $
                foldMap ( \kw' ->
                            if matchKeyword' kw'
                            then mkRefsImg kw' ir
                            else mempty
                        ) kws'

-- --------------------
--
-- logging functions for kewword stuff

log'KeywordCols :: Eff'ISEL r => KeywordCols -> Sem r ()
log'KeywordCols kws = do
  log'trc "Keyword to Path map:"
  void $
    M.traverseWithKey
    (\kw p ->
       log'trc $ kw ^. isoText <> ": " <> p ^. isoText
    ) kws


log'ImgRefs :: Eff'ISEL r => Set ImgRef -> Sem r ()
log'ImgRefs imgRefs =
  traverse_
  ( \(ImgRef i' nm') -> do
      p' <- objid2path i'
      log'trc $ "found: img " <> p' ^. isoText <> ", " <> nm' ^. isoText
  )
  imgRefs

log'ColRefs :: Eff'ISEL r => Set ObjId -> Sem r ()
log'ColRefs colRefs =
  traverse_
  ( \i' -> do
      p' <- objid2path i'
      log'trc $ "found: col " <> p' ^. isoText
  )
  colRefs

log'Refs :: Eff'ISEL r => (Set ImgRef, Set ObjId) -> Sem r ()
log'Refs (imgRefs, colRefs) = do
  log'ColRefs colRefs
  log'ImgRefs imgRefs

log'RefsMap :: (Eff'ISEL r) => RefsMap -> Sem r ()
log'RefsMap (RM m) = do
  void $
    M.traverseWithKey
    (\kw (Refs refs) -> do
        log'trc $ "found: keyword " <> kw
        log'Refs refs
        )
    m

------------------------------------------------------------------------

 {-
type E = (Int, [Int])

tooLargeE :: E -> Bool
tooLargeE = (> 4) . length . snd

distE :: E -> Int
distE = fst

mergeE :: E -> E -> E
mergeE (d1, es1) (d2, es2) = (d2, es1 <> es2)

es :: [E]
es = [ (1, replicate 1 0)
     , (0, replicate 4 1)
     , (1, replicate 1 2)
     , (1, replicate 3 3)
     , (1, replicate 4 4)
     , (1, replicate 5 5)
     , (2, replicate 3 6)
     , (0, replicate 3 7)
     , (0, replicate 3 8)
     , (3, replicate 3 9)
     , (2, replicate 3 0)
     , (1000000, [])
     ]

groupE :: [E] -> [E]
groupE = group tooLargeE distE mergeE

-- -}

-- ----------------------------------------------------------------------

group :: (a -> Bool) -> (a -> Int) -> (a -> a -> a) -> [a] -> [a]
group tooLarge dist merge = go . go0
  where
    -- group elements of distance 0
    go0 (x : xs1@(x1 : xs2))
      | dist x == 0         = go0 (merge x x1 : xs2)
      | otherwise           = x : go0 xs1
    go0 xs                  = xs

    go (x : xs1@(x1 : xs2))
      | tooLarge x          = x : go xs1              -- 1. elem too large to be combined
      | tooLarge x1         = x : x1 : go xs2         -- 2. elem too large to be combinde
      | dist x <= dist x1   = go (merge x x1 : xs2)   -- 2. elem nearer to 1. elem than to 3. elem
                                                      -- 2. elem nearer to 3. elem than to 1. elem
                                                      --    process rest of list first, then
      | tooLarge x1'        = x : xs1'                -- 1. elem of rest too large: put 1. elem in front
      | otherwise           = merge x x1' : xs2'      -- merge 1. elem with 1. elem of rest
        where
          xs1'@(x1' : xs2') = go xs1
    go xs                   = xs                      -- empty list or singleton list

------------------------------------------------------------------------
