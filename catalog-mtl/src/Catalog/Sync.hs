{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Sync
  ( syncDir
  , syncDirP
  , syncDirPath
  , syncFS
  , syncNode
  , syncNewDirs
  )
where

-- catalog-mtl
import Catalog.Cmd
import Catalog.System.ExifTool (syncMetaData)

-- catalog-data
import Data.ImgTree
import Data.MetaData
import Data.Prim
import Data.FilePath           (filePathToImgType, dropVirtualCopyNo)

-- ----------------------------------------

syncDirPath :: Cmd Path
syncDirPath =
  (n'archive `consPath`) . readPath . ("/" ++) <$>
  view envSyncDir

allColEntries' :: Path -> Cmd ColEntrySet
allColEntries' p = do
  verbose $ "allColEntries': " ++ quotePath p
  mbi <- lookupByPath p
  maybe (return mempty) (allColEntries . fst) mbi

allColEntries :: ObjId -> Cmd ColEntrySet
allColEntries =
  foldMT imgA dirA rootA colA
  where
    -- collect all ImgRef's by recursing into subcollections
    -- union subcollection results and imgrefs together
    colA :: (ObjId -> Cmd ColEntrySet)
         -> ObjId
         -> MetaData
         -> Maybe ImgRef
         -> Maybe ImgRef
         -> ColEntries
         -> Cmd ColEntrySet
    colA  go  i _md im be cs = do
      p              <- objid2path i
      verbose        $  "allColEntries: " ++ quotePath p
      let imref      =  im ^.. traverse . to mkColImgRef'
      let beref      =  be ^.. traverse . to mkColImgRef'
      let (crs, irs) =  partition isColColRef (cs ^. isoSeqList)
      iss            <- traverse go (crs ^.. traverse . theColColRef)
      return         $  foldl' (<>) (fromListColEntrySet $ imref ++ beref ++ irs) iss

    -- jump from the dir hierachy to the assosiated collection hierarchy
    dirA  go i _es  _ts = do
      p <- objid2path i
      verbose $ "allColEntries: " ++ quotePath p

      img2col <- img2colPath
      dp      <- objid2path i
      ci      <- fst <$> getIdNode' (img2col dp)
      go ci

    -- traverse the collection hierarchy
    rootA go _i _dir col = go col

    -- do nothing for img nodes, just to get a complete definition
    imgA  _     _pts _md = return mempty


{- not used

trcColEntrySet :: ColEntrySet -> Cmd [(Path, Name)]
trcColEntrySet cs =
  traverse trcCE $ toListColEntrySet cs
  where
    trcCE = colEntry ir cr
      where
        ir i n = (, n)      <$> objid2path i
        cr i   = (, mempty) <$> objid2path i
-}
-- ----------------------------------------

-- sync the whole photo archive with disk contents

syncFS :: ObjId -> Cmd ()
syncFS = idSyncFS True

-- sync a single entry of the archive (image or dir) with disk contents

syncNode :: ObjId -> Cmd ()
syncNode = idSyncFS False

-- sync a dir tree, given as path, in the archive with disk contents

syncDir :: Cmd ()
syncDir = do
  ts <- now
  -- check whether clipboard, and other collections are there
  verbose "syncDir: check/create the system collections"
  genSysCollections

  -- get the dir path for the (sub-)dir to be synchronized
  -- and start sync
  syncDirPath >>= syncDirP ts

syncDirP :: TimeStamp -> Path -> Cmd ()
syncDirP ts p = do
  verbose $ "syncDir: at " ++ quotePath p

  -- remember all ImgRef's in dir to be synchronized
  old'refs <- allColEntries' p
  verbose $ "syncDir: old'refs: " ++ show old'refs

  -- sync the dir
  syncDir' p

  -- throw away all ImgRef's in associated collection of the synchronized dir
  cp <- ($ p) <$> img2colPath
  cleanupColByPath cp

  verbose $ "syncDir: create the collections for the archive dir: " ++ quotePath p
  genCollectionsByDir' p
  verbose "syncDir: create the collections finished"

  -- now the associated collection for dir is up to date and
  -- contains all ImgRef's, which have been updated,
  -- now collect all synchronized refs in assoc colllections
  upd'refs <- allColEntries' p
  trc $ "syncDir: upd'refs: " ++ show upd'refs

  let rem'refs = old'refs `diffColEntrySet` upd'refs
  let new'refs = upd'refs `diffColEntrySet` old'refs

  -- rem'p <- trcColEntrySet rem'refs
  -- new'p <- trcColEntrySet new'refs

  verbose $ "syncDir: images removed: " ++ show rem'refs
  verbose   "syncDir: remove these refs in all collections"
  cleanupAllRefs rem'refs

  verbose $ "syncDir: images added:   " ++ show new'refs
  updateCollectionsByDate new'refs
  updateImportsDir ts new'refs

  -- final action: check integrity, especially
  -- whether there are dead enties in dir tree
  checkImgStore
  return ()

syncDir' :: Path -> Cmd ()
syncDir' p = do
  verbose $
    "syncDir': sync the archive dir with the file system: " ++ quotePath p
  mbi <- lookupByPath p
  i <- case mbi of
    Nothing -> do
      -- try to create new dir in parent dir, parent must already be there
      let (p1, n) = p ^. viewBase
      (di, dn) <- getIdNode' p1
      unless (isDIR dn) $
        abort $ "syncDir: parent isn't an image dir: " ++ quotePath p1
      mkImgDir di n

    Just (i', n') -> do
      unless (isDIR n') $
        abort $ "syncDIR: path isn't an image dir: " ++ quotePath p
      return i'

  idSyncFS True i

-- ----------------------------------------

syncNewDirs :: Path -> Cmd ()
syncNewDirs p = do
  verbose $
    "syncNewDirs: add new subdirs from filesystem into directory " ++ quotePath p
  mbi <- lookupByPath p
  case mbi of
    Nothing ->
      warn $ "syncNewDir: directory not found: " ++ quotePath p

    Just (i', n') -> do
      unless (isDIR n') $
        abort $ "syncNewDirs: path isn't an image dir: " ++ quotePath p
      whenM (objid2path i' >>= path2SysPath >>= dirExist) $
        syncNewDirsCont i'

syncNewDirsCont :: ObjId -> Cmd ()
syncNewDirsCont i = do
  ts      <- now
  p       <- objid2path i
  cont    <- objid2contNames i
  newdirs <- (filter (`notElem` cont) . fst) <$>
             collectDirCont i
  trc $ "syncNewDirsCont: " ++ show newdirs
  mapM_ (syncDirP ts . (p `snocPath`)) newdirs

-- ----------------------------------------
-- the work horse

idSyncFS :: Bool -> ObjId -> Cmd ()
idSyncFS recursive i = getImgVal i >>= go
  where
    go e
      | isIMG e = do
          trcObj i "idSyncFS: syncing image"
          p  <- objid2path i
          ps <- collectImgCont i
          syncImg i p ps

      | isDIR e = do
          trcObj i "idSyncFS: syncing directory"
          fp <- objid2path i >>= path2SysPath
          ex <- dirExist fp
          if ex
            then do
              syncDirCont recursive i
              setSyncTime i
              checkEmptyDir i
            else do
              verbose $ "sync: fs dir not found: " ++ show fp
              rmRec i

      | isROOT e = do
          trcObj i "idSyncFS: syncing root"
          idSyncFS recursive (e ^. theRootImgDir)

      | otherwise = do
          trcObj i "idSyncFS: nothing done for collection"
          return ()

syncDirCont :: Bool -> ObjId -> Cmd ()
syncDirCont recursive i = do
  -- trcObj i "syncDirCont: syncing entries in dir "
  (subdirs, imgfiles) <- collectDirCont i
  trc $ "syncDirCont: " ++ show (subdirs, imgfiles)
  p  <- objid2path i

  cont <- objid2contNames i
  let lost = filter (`notElem` subdirs ++ (map (fst . snd . head) imgfiles)) cont
  trc $ "syncDirCont: lost = " ++ show lost
  -- remove lost stuff
  mapM_ (remDirCont p) lost

  -- recurse into subdirs
  when recursive $
    mapM_ (syncSubDir p) subdirs

  -- sync the images
  mapM_ (syncImg i p) imgfiles
  where

    syncSubDir :: Path -> Name -> Cmd ()
    syncSubDir p n = do
      -- trc $ "syncSubDir: " ++ show p ++ "/" ++ show n
      whenM (isNothing <$> getTreeAt new'i) $
        void $ mkImgDir i n

      idSyncFS recursive new'i
      where
        new'i = mkObjId (p `snocPath` n)

    remDirCont :: Path -> Name -> Cmd ()
    remDirCont p n = do
      trcObj i $ "remDirCont: remove entry " ++ show n ++ " from dir"
      rmRec new'i
      where
        new'i = mkObjId (p `snocPath` n)

collectImgCont :: ObjId -> Cmd ClassifiedNames
collectImgCont i = do
  nm <- getImgName   i
  ip <- getImgParent i
  cs <- snd <$> collectDirCont ip
  return $ concat . take 1 . filter (^. to head . _2 . _1 . to (== nm)) $ cs

collectDirCont :: ObjId -> Cmd ([Name], [ClassifiedNames])
collectDirCont i = do
  -- trcObj i "collectDirCont: group entries in dir "
  fp <- objid2path i >>= path2SysPath
  es <- parseDirCont fp
  trc $ "collectDirCont: entries found " ++ show es

  let (others, rest) =
        partition (hasImgType isempty) es
  let (subdirs, rest2) =
        partition (hasImgType isImgSubDir) rest
  let (imgfiles, rest3) =
        partition (hasImgType isAnImgPart) rest2

  mapM_ (\ n -> verbose $ "sync: fs entry ignored " ++ show (fst n)) others
  realsubdirs <- filterM (isSubDir fp) subdirs

  unless (null rest3) $
    trc $ "collectDirCont: files ignored " ++ show rest3
  unless (null realsubdirs) $
    trc $ "collectDirCont: subdirs "       ++ show realsubdirs
  unless (null imgfiles) $
    trc $ "collectDirCont: imgfiles "      ++ show imgfiles

  return ( realsubdirs ^.. traverse . _1
         , partClassifiedNames imgfiles
         )
  where
    isSubDir :: SysPath -> (Name, (Name, ImgType)) -> Cmd Bool
    isSubDir fp n =
      dirExist ((</> (n ^. _1 . isoString)) <$> fp)


type ClassifiedName  = (Name, (Name, ImgType))
type ClassifiedNames = [ClassifiedName]

syncImg :: ObjId -> Path -> ClassifiedNames -> Cmd ()
syncImg ip pp xs = do
  -- new image ?
  whenM (not <$> existsEntry i) $
    void $ mkImg ip n

  unlessM (isIMG <$> getImgVal i) $
    warn $ "syncImg: entry for image conflicts with directory entry:"
           ++ quotePath p ++ ", entry ignored"

  -- trcObj i $ "syncImg: " ++ show (pp, xs)

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
      warn $ "sync: no showable part found for "
             ++ quotePath p' ++ ", parts: " ++ show xs
      rmImgNode i
  where
    p  = pp `snocPath` n
    i  = mkObjId p
    n  = xs ^. to head . _2 . _1
    ps = xs &  traverse %~ uncurry mkImgPart . second snd

syncParts :: ObjId -> Path -> Cmd ()
syncParts i pp = do
  -- trcObj i $ "syncParts: syncing img parts for "
  ps  <- getImgVals i (theParts . isoImgParts)
  ps' <- traverse syncPart ps
  adjustImg (const $ mkImgParts ps') i
  syncMetaData i
  where
    syncPart p = do
      fsp <- path2SysPath (pp `snocPath` (p ^. theImgName))
      ts  <- fsTimeStamp <$> fsFileStat fsp

      -- if file has changed, update timestamp and reset checksum
      return $
        if ts > p ^. theImgTimeStamp
        then p & theImgTimeStamp .~ ts
               & theImgCheckSum  .~ mempty
        else p

checkEmptyDir :: ObjId -> Cmd ()
checkEmptyDir i =
  whenM (isempty <$> getImgVal i) $ do
    p <- objid2path i
    verbose $ "sync: empty image dir ignored " ++ quotePath p
    rmImgNode i

-- --------------------
--
-- low level directory and file ops

parseDirCont :: SysPath -> Cmd ClassifiedNames
parseDirCont p = do
  (es, jpgdirs)  <- classifyNames <$> readDir p
  trc $ "parseDirCont: " ++ show (es, jpgdirs)
  jss <- traverse
         (parseImgSubDirCont p)                       -- process jpg subdirs
         (jpgdirs ^.. traverse . _1 . isoString)
  trc $ "parseDirCont: " ++ show jss
  return $ es ++ concat jss
  where
    classifyNames =
      partition (hasImgType (not . isJpgSubDir)) -- select jpg img subdirs
      .
      filter    (hasImgType (not . isBoring))    -- remove boring stuff
      .
      map (mkName &&& filePathToImgType)

parseImgSubDirCont :: SysPath -> FilePath -> Cmd ClassifiedNames
parseImgSubDirCont p d = do
  ex <- dirExist sp
  if ex
    then
      classifyNames <$> readDir sp
    else do
      verbose $ "parseImgSubDirCont: not a directory: " ++ show sp
      return []
  where
    sp = (</> d) <$> p

    classifyNames =
      filter (\ n -> isShowablePart (n ^. _2 . _2))
      .
      map (\ n -> let dn = d </> n
                  in (mkName dn, filePathToImgType dn)
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

    part1 :: ClassifiedName -> ClassifiedNames -> (ClassifiedNames, ClassifiedNames)
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
            s1 = n1 ^. isoString
            s2 = n2 ^. isoString

-- ----------------------------------------
