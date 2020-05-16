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
{-# LANGUAGE TupleSections #-}

------------------------------------------------------------------------------

module Catalog.SyncWithFileSys
  -- ( )
where

import Catalog.CatEnv          ( catSyncDir )
import Catalog.CopyRemove      ( cleanupColByPath
                               , cleanupAllRefs
                               , rmRec
                               )
import Catalog.Data.TextPath
import Catalog.Effects
import Catalog.GenCollections  ( img2colPath
                               , genSysCollections
                               , genCollectionsByDir'   -- remind the '
                               , updateCollectionsByDate
                               , updateImportsDir
                               )
import Catalog.ImgTree.Access
import Catalog.ImgTree.Fold
import Catalog.ImgTree.Modify
import Catalog.Invariant       ( checkImgStore )
import Catalog.Logging         ( trc'Obj )
import Catalog.MetaData.Sync   ( syncMetaData )
import Catalog.TextPath        ( path2SysPath )
import Catalog.TimeStamp       ( whatTimeIsIt, lastModified )

import Data.ImgTree
import Data.MetaData
import Data.Prim

-- ----------------------------------------

type Eff'Sync r = ( EffIStore   r   -- any effects missing?
                  , EffError    r
                  , EffJournal  r
                  , EffLogging  r
                  , EffCatEnv   r
                  , EffTime     r
                  , EffExecProg r
                  , EffFileSys  r
                  )

-- ----------------------------------------

syncDirPath :: Eff'Sync r => Sem r Path
syncDirPath = do
  p <- (^. catSyncDir) <$> ask
  return (n'archive `consPath` p)

allColEntries' :: Eff'ISEL r
               => Path -> Sem r ColEntrySet
allColEntries' p = do
  log'verb $ msgPath p "allColEntries': "
  mbi <- lookupByPath p
  maybe (return mempty) (allColEntries . fst) mbi

allColEntries :: Eff'ISEL r
              => ObjId -> Sem r ColEntrySet
allColEntries =
  foldMT imgA dirA rootA colA
  where
    -- collect all ImgRef's by recursing into subcollections
    -- union subcollection results and imgrefs together
    colA :: (EffIStore r, EffLogging r)
         => (ObjId -> Sem r ColEntrySet)
         -> ObjId
         -> MetaData
         -> Maybe ImgRef
         -> Maybe ImgRef
         -> ColEntries
         -> Sem r ColEntrySet
    colA  go  i _md im be cs = do
      p              <- objid2path i
      log'verb       $  msgPath p "allColEntries: "
      let imref      =  im ^.. traverse . to mkColImgRef'
      let beref      =  be ^.. traverse . to mkColImgRef'
      let (crs, irs) =  partition isColColRef (cs ^. isoSeqList)
      iss            <- traverse go (crs ^.. traverse . theColColRef)
      return         $  foldl' (<>) (fromListColEntrySet $ imref <> beref <> irs) iss

    -- jump from the dir hierachy to the assosiated collection hierarchy
    dirA  go i _es  _ts = do
      p <- objid2path i
      log'verb $ msgPath p "allColEntries: "

      img2col <- img2colPath
      dp      <- objid2path i
      ci      <- fst <$> getIdNode' (img2col dp)
      go ci

    -- traverse the collection hierarchy
    rootA go _i _dir col = go col

    -- do nothing for img nodes, just to get a complete definition
    imgA  _     _pts _md = return mempty

trcColEntrySet :: EffIStore r
               => ColEntrySet -> Sem r [(Path, Name)]
trcColEntrySet cs =
  traverse trcCE $ toListColEntrySet cs
  where
    trcCE = colEntry ir cr
      where
        ir i n = (, n)      <$> objid2path i
        cr i   = (, mempty) <$> objid2path i

-- ----------------------------------------

-- sync the whole photo archive with disk contents

syncFS :: Eff'Sync r => ObjId -> Sem r ()
syncFS = idSyncFS True

-- sync a single entry of the archive (image or dir) with disk contents

syncNode :: Eff'Sync r => ObjId -> Sem r ()
syncNode = idSyncFS False

-- sync a dir tree, given as path, in the archive with disk contents

syncDir :: Eff'Sync r => Sem r ()
syncDir = do
  ts <- whatTimeIsIt
  -- check whether clipboard, and other collections are there
  log'verb "syncDir: check/create the system collections"
  genSysCollections

  -- get the dir path for the (sub-)dir to be synchronized
  -- and start sync
  syncDirPath >>= syncDirP ts

syncDirP :: Eff'Sync r => TimeStamp -> Path -> Sem r ()
syncDirP ts p = do
  log'verb $ msgPath p "syncDir: at "

  -- remember all ImgRef's in dir to be synchronized
  old'refs <- allColEntries' p
  log'verb $ "syncDir: old'refs: " <> toText old'refs

  -- sync the dir
  syncDir' p

  -- throw away all ImgRef's in associated collection of the synchronized dir
  cp <- ($ p) <$> img2colPath
  cleanupColByPath cp

  log'verb $ msgPath p "syncDir: create the collections for the archive dir: "
  genCollectionsByDir' p
  log'verb "syncDir: create the collections finished"

  -- now the associated collection for dir is up to date and
  -- contains all ImgRef's, which have been updated,
  -- now collect all synchronized refs in assoc colllections
  upd'refs <- allColEntries' p
  log'trc $ "syncDir: upd'refs: " <> toText upd'refs

  let rem'refs = old'refs `diffColEntrySet` upd'refs
  let new'refs = upd'refs `diffColEntrySet` old'refs

  -- rem'p <- trcColEntrySet rem'refs
  -- new'p <- trcColEntrySet new'refs

  log'verb $ "syncDir: images removed: " <> toText rem'refs
  log'verb   "syncDir: remove these refs in all collections"
  cleanupAllRefs rem'refs

  log'verb $ "syncDir: images added:   " <> toText new'refs
  updateCollectionsByDate new'refs
  updateImportsDir ts new'refs

  -- final action: check integrity, especially
  -- whether there are dead enties in dir tree
  checkImgStore
  return ()

syncDir' :: Eff'Sync r => Path -> Sem r ()
syncDir' p = do
  log'verb $
    msgPath p "syncDir': sync the archive dir with the file system: "

  mbi <- lookupByPath p
  i <- case mbi of
    Nothing -> do
      -- try to create new dir in parent dir, parent must already be there

      let (p1, n) = p ^. viewBase
      (di, dn) <- getIdNode' p1

      unless (isDIR dn) $
        throw @Text $ msgPath p1"syncDir: parent isn't an image dir: "

      mkImgDir di n

    Just (i', n') -> do
      unless (isDIR n') $
        throw @Text $ msgPath p "syncDIR: path isn't an image dir: "

      return i'

  idSyncFS True i

-- ----------------------------------------

syncNewDirs :: Eff'Sync r => Path -> Sem r ()
syncNewDirs p = do
  log'verb $
    msgPath p"syncNewDirs: add new subdirs from filesystem into directory "

  mbi <- lookupByPath p
  case mbi of
    Nothing ->
      log'warn $ msgPath p "syncNewDir: directory not found: "

    Just (i', n') -> do
      unless (isDIR n') $
        throw @Text $ msgPath p "syncNewDirs: path isn't an image dir: "

      whenM ( do pp <- objid2path i'
                 sp <- path2SysPath pp
                 dirExist (sp ^. isoTextPath)
            ) $
        syncNewDirsCont i'

syncNewDirsCont :: Eff'Sync r => ObjId -> Sem r ()
syncNewDirsCont i = do
  ts      <- whatTimeIsIt
  p       <- objid2path i
  cont    <- objid2contNames i
  newdirs <- (filter (`notElem` cont) . fst) <$>
             collectDirCont i
  log'trc $ "syncNewDirsCont: " <> toText newdirs
  traverse_ (syncDirP ts . (p `snocPath`)) newdirs

-- ----------------------------------------
-- the work horse

idSyncFS :: Eff'Sync r => Bool -> ObjId -> Sem r ()
idSyncFS recursive i = getImgVal i >>= go
  where
    go e
      | isIMG e = do
          trc'Obj i "idSyncFS: syncing image"
          p  <- objid2path i
          ps <- collectImgCont i
          syncImg i p ps

      | isDIR e = do
          trc'Obj i "idSyncFS: syncing directory"
          sp <- objid2path i >>= path2SysPath
          ex <- dirExist (sp ^. isoTextPath)
          if ex
            then do
              syncDirCont recursive i
              t <- whatTimeIsIt
              setSyncTime t i
              checkEmptyDir i
            else do
              log'verb $ "sync: fs dir not found: " <> toText sp
              rmRec i

      | isROOT e = do
          trc'Obj i "idSyncFS: syncing root"
          idSyncFS recursive (e ^. theRootImgDir)

      | otherwise = do
          trc'Obj i "idSyncFS: nothing done for collection"
          return ()


syncDirCont :: Eff'Sync r => Bool -> ObjId -> Sem r ()
syncDirCont recursive i = do
  -- trc'Obj i "syncDirCont: syncing entries in dir "
  (subdirs, imgfiles) <- collectDirCont i
  log'trc $ "syncDirCont: " <> toText (subdirs, imgfiles)
  p  <- objid2path i

  cont <- objid2contNames i
  let lost = filter (`notElem` subdirs <> (map (fst . snd . head) imgfiles)) cont
  log'trc $ "syncDirCont: lost = " <> toText lost

  -- remove lost stuff
  traverse_ (remDirCont p) lost

  -- recurse into subdirs
  when recursive $
    traverse_ (syncSubDir p) subdirs

  -- sync the images
  mapM_ (syncImg i p) imgfiles
  where

    syncSubDir p n = do
      -- log'trc $ "syncSubDir: " <> toText p <//> toText n
      whenM (isNothing <$> getTreeAt new'i) $
        void $ mkImgDir i n

      idSyncFS recursive new'i
      where
        new'i = mkObjId (p `snocPath` n)

    remDirCont p n = do
      trc'Obj i $ "remDirCont: remove entry " <> toText n <> " from dir"
      rmRec new'i
      where
        new'i = mkObjId (p `snocPath` n)


collectImgCont :: (EffError r, EffLogging r, EffCatEnv r, EffIStore r, EffFileSys r)
               => ObjId -> Sem r ClassifiedNames
collectImgCont i = do
  nm <- getImgName   i
  ip <- getImgParent i
  cs <- snd <$> collectDirCont ip
  return $ concat . take 1 . filter (^. to head . _2 . _1 . to (== nm)) $ cs


collectDirCont :: (EffLogging r, EffCatEnv r, EffIStore r, EffFileSys r)
               => ObjId -> Sem r ([Name], [ClassifiedNames])
collectDirCont i = do
  -- trc'Obj i "collectDirCont: group entries in dir "
  sp <- objid2path i >>= path2SysPath
  es <- parseDirCont sp
  log'trc $ "collectDirCont: entries found " <> toText es

  let (others, rest) =
        partition (hasImgType isempty) es
  let (subdirs, rest2) =
        partition (hasImgType isImgSubDir) rest
  let (imgfiles, rest3) =
        partition (hasImgType isAnImgPart) rest2

  traverse_
    (\ n -> log'verb $ "sync: fs entry ignored " <> toText (fst n))
    others

  realsubdirs <- filterM (isSubDir sp) subdirs

  unless (null rest3) $
    log'trc $ "collectDirCont: files ignored " <> toText rest3
  unless (null realsubdirs) $
    log'trc $ "collectDirCont: subdirs "       <> toText realsubdirs
  unless (null imgfiles) $
    log'trc $ "collectDirCont: imgfiles "      <> toText imgfiles

  return ( realsubdirs ^.. traverse . _1
         , partClassifiedNames imgfiles
         )
  where
    isSubDir :: EffFileSys r
             => SysTextPath -> (Name, (Name, ImgType)) -> Sem r Bool
    isSubDir sp n =
      dirExist tp
      where
        tp = sp ^. isoTextPath <//> n ^. _1 . isoText

-- ----------------------------------------

type ClassifiedName  = (Name, (Name, ImgType))
type ClassifiedNames = [ClassifiedName]

syncImg :: Eff'Sync r
        => ObjId -> Path -> ClassifiedNames -> Sem r ()
syncImg ip pp xs = do
  -- new image ?
  whenM (not <$> existsEntry i) $
    void $ mkImg ip n

  unlessM (isIMG <$> getImgVal i) $
    log'warn $
      msgPath p "syncImg: entry for image conflicts with directory entry:"
                <> ", entry ignored"

  -- trc'Obj i $ "syncImg: " <> toText (pp, xs)

  -- is there at least a jpg image
  -- a txt (something, that can be shown)
  -- or a movie,
  -- then update entry, else (e.g. raw only) ignore it

  if has (traverse . _2 . _2 . isA isShowablePart {- OrRaw -}) xs
    then do
      adjustImg (<> mkImgParts ps) i
      syncParts i pp
    else do
      p' <- objid2path i
      log'warn $
        msgPath p' "sync: no showable part found for "
                   <> ", parts: " <> toText xs
      rmImgNode i
  where
    p  = pp `snocPath` n
    i  = mkObjId p
    n  = xs ^. to head . _2 . _1
    ps = xs &  traverse %~ uncurry mkImgPart . second snd


syncParts :: Eff'Sync r => ObjId -> Path -> Sem r ()
syncParts i pp = do
  -- trc'Obj i "syncParts: syncing img parts for "
  ps  <- getImgVals i (theParts . isoImgParts)
  ps' <- traverse syncPart ps
  adjustImg (const $ mkImgParts ps') i
  syncMetaData i
  where
    syncPart p = do
      sp <- path2SysPath (pp `snocPath` (p ^. theImgName))
      ts <- lastModified (sp ^. isoTextPath)

      -- if file has changed, update timestamp and reset checksum
      return $
        if ts > p ^. theImgTimeStamp
        then p & theImgTimeStamp .~ ts
               & theImgCheckSum  .~ mempty
        else p


checkEmptyDir :: Eff'ISEJL r => ObjId -> Sem r ()
checkEmptyDir i =
  whenM (isempty <$> getImgVal i) $ do
    p <- objid2path i
    log'verb $ msgPath p "sync: empty image dir ignored "
    rmImgNode i

-- --------------------
--
-- low level directory and file ops

parseDirCont :: (EffFileSys r, EffLogging r)
             => SysTextPath -> Sem r ClassifiedNames
parseDirCont p = do
  (es, jpgdirs)  <- classifyNames <$> readDir (p ^. isoTextPath)
  log'trc $ "parseDirCont: " <> toText (es, jpgdirs)
  jss <- traverse
         (parseImgSubDirCont p)                       -- process jpg subdirs
         (jpgdirs ^.. traverse . _1 . isoText)
  log'trc $ "parseDirCont: " <> toText jss
  return $ es <> mconcat jss
  where

    classifyNames :: [TextPath] -> (ClassifiedNames, ClassifiedNames)
    classifyNames =
      partition (hasImgType (not . isJpgSubDir)) -- select jpg img subdirs
      .
      filter    (hasImgType (not . isBoring))    -- remove boring stuff
      .
      map ((isoText #) &&& textPathToImgType)


parseImgSubDirCont :: (EffFileSys r, EffLogging r)
                   => SysTextPath -> TextPath -> Sem r ClassifiedNames
parseImgSubDirCont p d = do
  ex <- dirExist sp
  if ex
    then
      classifyNames <$> readDir sp
    else do
      log'verb $ "parseImgSubDirCont: not a directory: " <> toText sp
      return []
  where
    sp :: TextPath
    sp = p ^. isoTextPath <//> d

    classifyNames =
      filter (\ n -> isShowablePart (n ^. _2 . _2))
      .
      map (\ n -> let dn = d <//> n
                  in (isoText # dn, textPathToImgType dn)
          )

hasImgType :: (ImgType -> Bool) -> ClassifiedName -> Bool
hasImgType p = p . snd . snd
{-# INLINE hasImgType #-}

partClassifiedNames :: ClassifiedNames -> [ClassifiedNames]
partClassifiedNames = unfoldr part . sortBy (compare `on` (^. key))
  where
    key :: Lens' ClassifiedName Name
    key   = _2 . _1

    part :: ClassifiedNames -> Maybe (ClassifiedNames, ClassifiedNames)
    part [] = Nothing
    part (x : xs) = Just $ part1 x xs


    part1 :: ClassifiedName
          -> ClassifiedNames
          -> (ClassifiedNames, ClassifiedNames)

    part1 x1 []             = ([x1], [])
    part1 x1 xs@(x2 : xs')
      | k1 `equiv` k2  = part1 x2' xs' & _1 %~ (x1 :)
      | otherwise      = ([x1], xs)
      where
        k1    = x1 ^. key
        k2    = x2 ^. key
        x2'   = x2 &  key .~ k1

        equiv n1 n2 =
          n1 == n2
          ||
          s1 == dropVirtualCopyNo s2  -- equality relative to virtual copy no
          where
            s1 = n1 ^. isoText
            s2 = n2 ^. isoText

------------------------------------------------------------------------
