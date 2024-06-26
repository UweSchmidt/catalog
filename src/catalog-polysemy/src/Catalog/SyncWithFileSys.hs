------------------------------------------------------------------------------

module Catalog.SyncWithFileSys
  ( Eff'Sync
  , syncDir
  , syncDirP
  , syncFS
  , syncNode
  , syncNewDirs
  )
where

import Catalog.CopyRemove
       ( rmRec
       , AdjustImgRef
       , AdjustColEnt
       , cleanupRefs'
       )
import Catalog.Effects
       ( Eff'ISEJL
       , Eff'ISEL
       , EffCatEnv
       , EffError
       , EffExecProg
       , EffFileSys
       , EffIStore
       , EffJournal
       , EffLogging
       , EffTime
       , Sem
       , TextPath
       , dirExist
       , fileExist
       , log'info
       , log'trc
       , log'verb
       , log'warn
       , readDir
       , throw
       )
import Catalog.GenCollections
       ( genSysCollections
       , genCollectionsByDir'   -- remind the '
       , updateCollectionsByDate
       , updateImportsDir
       )
import Catalog.ImgTree.Access
       ( getImgVals
       , objid2path
       , getRootImgColId
       , lookupByPath
       , getImgName
       , getImgVal
       , existsEntry
       , getImgParent
       , getTreeAt
       , objid2contNames
       , getIdNode'
       )
import Catalog.ImgTree.Fold
       ( foldMT )

import Catalog.ImgTree.Modify
       ( rmImgNode
       , adjustImg
       , mkImg
       , setSyncTime
       , mkImgDir
       )
import Catalog.Logging
       ( trc'Obj )

import Catalog.MetaData.Sync
       ( syncMetaData )

import Catalog.TextPath
       ( toFileSysPath
       , toFileSysTailPath
       , buildImgPath0
       )
import Catalog.TimeStamp
       ( whatTimeIsIt
       , lastModified
       )

import Data.ImgTree
       ( ColEntries
       , ColEntry
       , ImgNode'(IMG)
       , ImgRef
       , ImgRef'(ImgRef, _iname)
       , colEntry'
       , isColColRef
       , isDIR
       , isIMG
       , isROOT
       , isoImgParts
       , mkColColRef
       , mkColImgRef'
       , mkImgPart
       , mkImgParts
       , theColColRef
       , theColImgRef
       , theImgCheckSum
       , theImgName
       , theImgTimeStamp
       , thePartNamesI
       , theParts
       , theRootImgDir
       )
import Data.MetaData
       ( MetaData )

import Data.Prim
import Data.TextPath
       ( ClassifiedName
       , ClassifiedNames
       , classifyPaths
       , isImgCopiesDir
       , (<//>)
       , imgNames
       )

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Sequence   as Seq
import qualified Data.Text       as T

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
--
-- collect all ImgRef's used in a collection

type ImgRefMap = Map ObjId (Set ImgRef)

-- edit ImgRef's in a collection
-- value is Nothing: delete ref
-- value is Just ir: substitute ref by ir

type ImgRefUpdateMap = Map ImgRef (Maybe ImgRef)

buildImgRefUpdates :: ImgRefMap -> ImgRefMap -> ImgRefUpdateMap
buildImgRefUpdates old'refs upd'refs =
  M.foldrWithKey op1 mempty old'refs
  where
    op1 :: ObjId -> Set ImgRef -> ImgRefUpdateMap -> ImgRefUpdateMap
    op1 i irs acc = case M.lookup i upd'refs of
      -- all refs must be deleted
      Nothing  ->
        foldr (`M.insert` Nothing) acc irs

      -- some refs maybe renamed, some removed or unchanged
      Just urs ->
        foldr (\ ir' -> ins ir' (upd ir')) acc irs
        where
          urs1    = foldr (\ ur' -> M.insert (_iname ur') ur') mempty urs

          upd ir' = M.lookup (_iname ir') urs1

          ins ir' (Just ur') acc'
            | ir' == ur'    =                   acc'    -- ref remains as it is
          ins ir' mur' acc' = M.insert ir' mur' acc'

buildNewColEntries :: ImgRefMap -> ImgRefMap -> ImgRefUpdateMap -> ColEntries
buildNewColEntries new'refs old'refs upd'refs =
  foldr (\ ir acc -> mkColImgRef' ir Seq.<| acc) mempty res
  where
    res = new `S.difference` old `S.difference` upd
    new = foldr S.union mempty new'refs
    old = foldr S.union mempty old'refs
    upd = foldr (\ mr acc ->
                    maybe acc (`S.insert` acc) mr
                ) mempty upd'refs

-- --------------------

collectImgRefs' :: Eff'ISEL r
                   => Path -> Sem r ImgRefMap
collectImgRefs' p = do
  log'trc $ msgPath p "collectImgRefs': "
  mbi <- lookupByPath p
  maybe (return mempty) (collectImgRefs . fst) mbi

collectImgRefs :: Eff'ISEL r
               => ObjId -> Sem r ImgRefMap
collectImgRefs =
  foldMT imgA dirA rootA colA
  where
    -- collect all used ImgRef's by traversing the collection hierachy
    colA :: (EffIStore r, EffLogging r)
         => (ObjId -> Sem r ImgRefMap)
         -> ObjId
         -> MetaData
         -> Maybe ImgRef
         -> Maybe ImgRef
         -> ColEntries
         -> Sem r ImgRefMap
    colA go  i _md im be cs = do
      p              <- objid2path i
      log'trc        $  msgPath p "collectImgRefs: used refs in coll "
      let imref      =  im ^.. traverse
      let beref      =  be ^.. traverse
      let (crs, irs) =  partition isColColRef (cs ^. isoSeqList)
      let irs1       =  irs ^.. traverse . theColImgRef
      let irs2       =  S.fromList $ imref <> beref <> irs1
      subs           <- traverse go (crs ^.. traverse . theColColRef)
      return         $  M.insert i irs2 $ M.unions subs

    -- collect all defined ImgRef's by traversing the dir hierachy
    -- the refs to an IMG are processed in place
    -- the sub DIR's are processed by the recursive call go i'
    dirA go i es  _ts = do
      p <- objid2path i
      log'trc $ msgPath p "collectImgRefs: defined refs in dir "

      foldM (\ acc' i' -> do
                m1 <- coll i'
                return $ M.unionWith S.union m1 acc'
            ) mempty es
      where
        coll i' = do
          n' <- getImgVal i'
          case n' of
            IMG pts _md -> do
              let irs =
                    foldr (S.insert . ImgRef i') mempty $
                    pts ^.. thePartNamesI
              return $ M.singleton i irs

            _dir ->
              go i'

    -- traverse the collection hierarchy
    -- all defined and used image refs
    rootA go _i dir col =
      M.unionWith S.union <$> go dir <*> go col

    -- do nothing for img nodes, just to get a complete definition
    -- the IMG entries are processed in dirA
    imgA _ _pts _md =
      return mempty

-- --------------------

updateAllImgRefs :: Eff'ISEJL r => ImgRefUpdateMap -> Sem r ()
updateAllImgRefs um =
  getRootImgColId >>= updateImgRefs um

updateImgRefs :: Eff'ISEJL r => ImgRefUpdateMap -> ObjId -> Sem r ()
updateImgRefs um i0
  | isEmpty um = return ()
  | otherwise  = cleanupRefs' adjIR adjCE i0
  where
    adjIR :: AdjustImgRef         -- Maybe ImgRef -> Maybe (Maybe ImgRef)
    adjIR (Just ir) = M.lookup ir um
    adjIR _         = Nothing


    adjCE :: AdjustColEnt         -- ColEntries -> Maybe ColEntries
    adjCE es
      | any mustBeUpdated es =
          -- only rebuild the list es if any refs must be deleted
          Just $ foldr (\ ce es' -> upd ce <> es') mempty es

      | otherwise =
          Nothing

      where
        mustBeUpdated :: ColEntry -> Bool
        mustBeUpdated = colEntry' (`M.member` um) (const False)

        upd :: ColEntry -> ColEntries
        upd ce = colEntry'
          (\ ir' -> case M.lookup ir' um of
                      Nothing         -> Seq.singleton ce
                      Just Nothing    -> mempty
                      Just (Just ur') -> Seq.singleton . mkColImgRef' $ ur'
          )
          (Seq.singleton . mkColColRef)
          ce

-- ----------------------------------------

-- sync the whole photo archive with disk contents

syncFS :: Eff'Sync r => ObjId -> Sem r ()
syncFS = idSyncFS True

-- sync a single entry of the archive (image or dir) with disk contents

syncNode :: Eff'Sync r => ObjId -> Sem r ()
syncNode = idSyncFS False

-- sync the whole DIR hierachy with file system

syncDir :: Eff'Sync r => Sem r ()
syncDir = do
  ts <- whatTimeIsIt

  -- check whether clipboard, and other system collections are there
  genSysCollections

  -- get the dir path for the (sub-)dir to be synchronized
  -- and start sync
  syncDirP ts p'arch'photos

-- sync a dir tree, given as path, in the archive with disk contents

syncDirP :: Eff'Sync r => TimeStamp -> Path -> Sem r ()
syncDirP ts p = do
  log'sync1

  -- remember all ImgRef's in dir to be synchronized
  old'refs <- collectImgRefs' p
  log'trc $ "syncDir: refs before sync:\n" <> prettyJSONText [] (toList old'refs)

  -- sync the dir
  syncDir' p

  -- collect all ImgRef' after dir sync
  new'refs <- collectImgRefs' p
  log'trc $ "syncDir: refs after sync\n" <> prettyJSONText [] (toList new'refs)

  -- compute refs to be deleted or renamed in collection hierachy
  let mod'refs = buildImgRefUpdates old'refs new'refs

  -- cleanup collections
  log'syncRM $ M.toList mod'refs
  updateAllImgRefs mod'refs  -- cleanupAllRefs rem'refs

  -- compute set of ImgRef's for new images
  let es = buildNewColEntries new'refs old'refs mod'refs
  log'syncAD es

  -- generate dir collection
  log'verb $ msgPath p "syncDir: create the assocciated collection for: "
  genCollectionsByDir' p

  -- insert the new images in the ByDate collections
  log'verb $ msgPath p "syncDir: update the ByDate collections: "
  updateCollectionsByDate es

  -- insert the new images into a new import collection
  log'verb $ msgPath p "syncDir: update the imports collection: "
  updateImportsDir ts es

  log'sync9
  where
    qt t = "\"" <> t <> "\""

    log'sync1 = do
      fsp <- toFileSysPath p
      log'info $ msgPath p ("syncDir: catalog dir: ")
      log'info $            "syncDir: filesys dir: " <> qt fsp

    log'sync9 =
      log'info "syncDir: finished"

    log'syncAD refs
      | null refs = log'info "syncDir: no images added"
      | otherwise = traverse_ (colEntry' logIref logCref) refs
      where
        logIref ref = do
          tp <- buildImgPath0 ref
          log'info $ "syncDir: image added: " <> qt tp

        logCref ref = do
          tp <- (^. isoText) <$> objid2path ref
          log'info $ "syncDir: dir   added: " <> qt tp

    log'syncRM refs
      | null refs = log'info "syncDir: no images removed or renamed"
      | otherwise = traverse_ (uncurry logRef) refs
      where
        logRef ref Nothing = do
          tp <- buildImgPath0 ref
          log'info $ "syncDir: image removed: " <> qt tp

        logRef ref (Just nref) = do
          tp <- buildImgPath0 ref
          np <- buildImgPath0 nref
          log'info $ "syncDir: image " <> qt tp <> " renamed to " <> qt np


syncDir' :: Eff'Sync r => Path -> Sem r ()
syncDir' p = do
  log'trc $
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
  log'info $
    msgPath p "syncDir: import new filesystem dirs into catalog at: "

  mbi <- lookupByPath p
  case mbi of
    Nothing ->
      log'warn $ msgPath p "syncNewDir: directory not found: "

    Just (i', n') -> do
      unless (isDIR n') $
        throw @Text $ msgPath p "syncNewDirs: path isn't an image dir: "

      whenM ( do pp <- objid2path i'
                 sp <- toFileSysTailPath pp
                 dirExist sp
            ) $
        syncNewDirsCont i'
  log'info "syncDir: import of new filesystem dirs finished"


syncNewDirsCont :: Eff'Sync r => ObjId -> Sem r ()
syncNewDirsCont i = do
  ts      <- whatTimeIsIt
  p       <- objid2path i
  cont    <- objid2contNames i
  newdirs <- filter (`notElem` cont) . fst <$>
             collectDirCont i
  log'trc $ "syncNewDirsCont: " <> prettyJSONText [] newdirs
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
          p' <- objid2path i
          sp <- toFileSysTailPath p'
          ex <- dirExist sp
          if ex
            then do
              syncDirCont recursive i
              t <- whatTimeIsIt
              setSyncTime t i
              checkEmptyDir i
            else do
              log'trc $ "sync: fs dir not found: " <> toText sp
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
  log'trc $ "syncDirCont: subdirs:\n"     <> prettyJSONText [] subdirs
  log'trc $ "syncDirCont: image files:\n" <> prettyJSONText [] imgfiles
  p  <- objid2path i

  cont <- objid2contNames i
  let lost = filter (`notElem` subdirs <> imgNames imgfiles) cont
  log'trc $ "syncDirCont: entries lost:\n" <> prettyJSONText [] lost

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
  return
    . concat
    . take 1
    . filter (maybe False ((== nm) . fst . snd . fst) . uncons) $ cs


collectDirCont :: (EffLogging r, EffCatEnv r, EffIStore r, EffFileSys r)
               => ObjId -> Sem r ([Name], [ClassifiedNames])
collectDirCont i = do
  -- trc'Obj i "collectDirCont: group entries in dir "
  p' <- objid2path i

  (subdirs, es) <- parseDir p'

  log'trc $ "collectDirCont: files found:\n"   <> prettyJSONText [] es
  log'trc $ "collectDirCont: subdirs found:\n" <> prettyJSONText [] subdirs

  let (others,   es1) = partition (hasMimeType isOtherMT    ) es
  let (imgfiles, es2) = partition (hasMimeType isAnImgPartMT) es1
  let imgfiles'       = partClassifiedNames imgfiles

  traverse_
    (\ n -> log'warn $ "sync: fs entry ignored " <> toText (fst n))
    es2

  unless (null others) $
    log'trc $ "collectDirCont: ignored:\n" <> prettyJSONText [] others
  unless (null imgfiles') $
    log'trc $ "collectDirCont: imgfiles\n" <> prettyJSONText [] imgfiles'

  return (subdirs, imgfiles')

-- ----------------------------------------

syncImg :: Eff'Sync r
        => ObjId -> Path -> ClassifiedNames -> Sem r ()
syncImg ip pp xs = do
  trc'Obj i $ "syncImg:\n" <> prettyJSONText [] (p, xs)

  -- new image ?
  whenM (not <$> existsEntry i) $
    void $ mkImg ip n

  unlessM (isIMG <$> getImgVal i) $
    log'warn $
      msgPath p "syncImg: entry for image conflicts with directory entry:"
                <> ", entry ignored"


  -- is there at least a jpg image
  -- a txt (something, that can be shown)
  -- or a movie,
  -- then update entry, else (e.g. raw only) ignore it

  if has (traverse . _2 . _2 . filtered isShowablePartMT {- OrRaw -}) xs
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
    n  = maybe mempty (fst . snd . fst) . uncons $ xs
    ps = xs &  traverse %~ uncurry mkImgPart . second snd


syncParts :: Eff'Sync r => ObjId -> Path -> Sem r ()
syncParts i pp = do
  trc'Obj i "syncParts: syncing img parts for "
  ps  <- getImgVals i (theParts . isoImgParts)
  ps' <- traverse syncPart ps
  adjustImg (const $ mkImgParts ps') i
  syncMetaData i
  where
    syncPart p = do
      sp <- toFileSysTailPath (pp `snocPath` (p ^. theImgName))
      ts <- lastModified sp

      -- if file has changed, update timestamp and reset checksum
      return $
        if ts > p ^. theImgTimeStamp
        then p & theImgTimeStamp .~ ts
               & theImgCheckSum  .~ mempty
        else p


checkEmptyDir :: Eff'ISEJL r => ObjId -> Sem r ()
checkEmptyDir i =
  whenM (isEmpty <$> getImgVal i) $ do
    p <- objid2path i
    log'trc $ msgPath p "sync: empty DIR ignored: "
    rmImgNode i

-- --------------------
--
-- low level dir access

parseDir0 :: (EffFileSys r, EffLogging r, EffCatEnv r)
          => TextPath -> Sem r ([Text], [Text])
parseDir0 sp = do
  ns0 <- readDir sp
  -- log'trc $ "parseDir0: " <> toText ns0

  let ns = filter noDotFile ns0
  -- log'trc $ "parseDir0: " <> toText ns

  (fs, rs) <- partitionM (\ n -> fileExist $ sp <//> n) ns
  (ds, _ ) <- partitionM (\ n ->  dirExist $ sp <//> n) rs
  -- log'trc $ "parseDir0: " <> toText (ds, fs)

  return (ds, fs)
  where
    noDotFile n = not ("." `T.isPrefixOf` n)

parseDir :: (EffFileSys r, EffLogging r, EffCatEnv r)
          => Path -> Sem r ([Name], ClassifiedNames)
parseDir p = do
  sp <- toFileSysTailPath p

  log'trc $ "parseDir: path = " <> sp

  -- get the files and the dirs
  (ds0, fs0) <- parseDir0 sp

  -- get the subdirs for image copies and the real sub dirs
  let (dsi, ds) = partition isImgCopiesDir ds0

  -- the dir parser for img copies subdirs
  -- extend file names with subdir name and ignore sub-subdirs
  let parseSubDir n = do
        (_ds', fs') <- parseDir0 (sp <//> n)
        return $
          map (n <//>) fs'

  -- collect all file names contained in these dirs
  fss <- traverse parseSubDir dsi
  log'trc $ "parseDir: files:\n" <> prettyJSONText [] fss

  -- collect all file names in this dir and img copy subdirs
  let fs = fs0 <> mconcat fss

  -- classifiy all potentital img file names by name and extension
  let cns = classifyPaths fs

  -- return all real subdirs and all classified files
  return (map (isoText #) ds, cns)


hasMimeType :: (MimeType -> Bool) -> ClassifiedName -> Bool
hasMimeType p = p . snd . snd
{-# INLINE hasMimeType #-}

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
      | k1 == k2  = part1 x2' xs' & _1 %~ (x1 :)
      | otherwise      = ([x1], xs)
      where
        k1    = x1 ^. key
        k2    = x2 ^. key
        x2'   = x2 &  key .~ k1

------------------------------------------------------------------------
