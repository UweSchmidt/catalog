{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Cmd.Basic
  ( dt
  , getTree
  , getImgName
  , getImgParent
  , getImgVal
  , getImgVals
  , getImgSubDirs
  , getMetaData
  , getRootId
  , getRootImgDirId
  , getRootImgColId
  , existsObjId
  , lookupByName
  , lookupByPath
  , getIdNode
  , getIdNode'
  , objid2path
  , objid2type
  , objid2contNames
  , mapObjId2Path
  , mapImgStore2Path
  , mapPath2ObjId
  , mapImgStore2ObjId
  , mkImgDir
  , mkImgCol
  , mkImg
  , rmImgNode
  , mkCollection
  , mkCollectionC
  , adjustImg
  , adjustDirEntries
  , adjustMetaData
  , adjustColImg
  , adjustColBlog
  , adjustColEntries
  , remColEntry
  , setSyncTime
  , findAllColEntries
  , findFstColEntry
  , sortColEntries
  , mergeColEntries
  , colEntryAt
  , processColEntryAt
  , processColImgEntryAt
    -- basic combinators
  , fromJustCmd
  , liftE
  , catchAll
  , runDry
  , trcObj
  , trcCmd
  , journalChange
  , buildImgPath0
  , buildImgPath
  -- file system path
  , toSysPath
  , path2SysPath
  , path2ExifSysPath
  )
where

import           Catalog.Cmd.Types
import           Catalog.FilePath  (addJpg)
import           Catalog.Journal
import           Data.ImageStore
import           Data.ImgTree
import           Data.MetaData
import           Data.Prim

-- ----------------------------------------

-- simple monadic ops

dt :: Cmd ImgTree
dt = use theImgTree
{-# INLINE dt #-}

getTree :: Getting a ImgTree a -> Cmd a
getTree l = use (theImgTree . l)
{-# INLINE getTree #-}

getImgName :: ObjId -> Cmd Name
getImgName i = getTree (theNode i . nodeName)
{-# INLINE getImgName #-}

getImgParent :: ObjId -> Cmd ObjId
getImgParent i = getTree (theNode i . parentRef)
{-# INLINE getImgParent #-}

getImgVal :: ObjId -> Cmd ImgNode
getImgVal i = getTree (theNode i . nodeVal)
{-# INLINE getImgVal #-}

getImgVals :: ObjId -> Getting a ImgNode a -> Cmd a
getImgVals i l = getTree (theNode i . nodeVal . l)
{-# INLINE getImgVals #-}

getImgSubDirs :: DirEntries -> Cmd [ObjId]
getImgSubDirs es =
  filterM (\ i' -> getImgVals i' (to isDIR)) (es ^. isoDirEntries)

-- ----------------------------------------

getRootId :: Cmd ObjId
getRootId = getTree rootRef
{-# INLINE getRootId #-}

getRootImgDirId :: Cmd ObjId
getRootImgDirId = do
  ri <- getRootId
  getImgVals ri theRootImgDir
{-# INLINE getRootImgDirId #-}

getRootImgColId :: Cmd ObjId
getRootImgColId = do
  ri <- getRootId
  getImgVals ri theRootImgCol
{-# INLINE getRootImgColId #-}

existsObjId :: ObjId -> Cmd Bool
existsObjId i =
  isJust <$> getTree (entryAt i)
{-# INLINE existsObjId #-}

lookupByName :: Name -> ObjId -> Cmd (Maybe (ObjId, ImgNode))
lookupByName n i = do
  p <- (`snocPath` n) <$> objid2path i
  lookupByPath p

lookupByPath :: Path -> Cmd (Maybe (ObjId, ImgNode))
lookupByPath p = lookupImgPath p <$> dt
{-# INLINE lookupByPath #-}

-- save lookup by path

getIdNode :: String -> Path -> Cmd (ObjId, ImgNode)
getIdNode msg p = do
  mv <- lookupImgPath p <$> dt
  case mv of
    Nothing ->
      abort $ msg ++ " " ++ quotePath p
    Just res ->
      return res

getIdNode' :: Path -> Cmd (ObjId, ImgNode)
getIdNode' p =
  getIdNode ("cant' find entry for path:") p

-- check path not there

alreadyTherePath :: String -> Path -> Cmd ()
alreadyTherePath msg p = do
  whenM (isJust <$> lookupByPath p) $
    abort $ msg ++ " " ++ quotePath p

-- ----------------------------------------

-- | ref to path
objid2path :: ObjId -> Cmd Path
objid2path i = dt >>= go
  where
    go t = return (refPath i t)

-- | ref to type
objid2type :: ObjId -> Cmd String
objid2type i = getImgVal i >>= go
  where
    go e = return $ concat $
      e ^.. ( theParts      . to (const "IMG")  <>
              theDirEntries . to (const "DIR")  <>
              theImgRoot    . to (const "Root") <>
              theImgCol     . to (const "COL")
            )

objid2contNames :: ObjId -> Cmd [Name]
objid2contNames i = getImgVal i >>= go
  where
    go e
      | isIMG e =
          return (e ^. theParts . isoImgParts . traverse . theImgName . to (:[]))

      | isDIR e =
          traverse getImgName (e ^. theDirEntries . isoDirEntries)

      | isROOT e = let (i1, i2) = e ^. theImgRoot in do
          n1 <- getImgName i1
          n2 <- getImgName i2
          return [n1, n2]

      | otherwise =
          return []

getMetaData :: ObjId -> Cmd MetaData
getMetaData i = getImgVals i theMetaData

-- ----------------------------------------

-- replace an ObjId by a Path in an arbitrary functor, e.g. ImgNode'
-- ObjId, the internal key type, can be implemented more efficiently
-- than taking the whole path as key, e.g. by a hash. But the path variant
-- is more readable, and it's needed by clients, cilents can't handle
-- internal keys, like hashes or similar data

mapObjId2Path :: Functor f => f ObjId -> Cmd (f Path)
mapObjId2Path x =
  (<$> x) <$> objid2pathMap
  -- do f <- objid2pathMap
  --    return $ fmap f x

-- convert an ImgStore to an isomorphic ImgStore' Path with paths as keys
mapImgStore2Path :: Cmd (ImgStore' Path)
mapImgStore2Path = do
  x <- get
  (`mapImgStore` x) <$> objid2pathMap

-- get the mapping from internal keys, ObjId, to paths as keys
objid2pathMap :: Cmd (ObjId -> Path)
objid2pathMap = dt >>= go
  where
    go t = return (`refPath` t)

mapPath2ObjId :: Functor f => f Path -> f ObjId
mapPath2ObjId = fmap mkObjId

mapImgStore2ObjId :: ImgStore' Path -> ImgStore
mapImgStore2ObjId = mapImgStore mkObjId

-- ----------------------------------------
--
-- smart constructors

mkImg' :: (ObjId -> Name -> Journal) ->
          (ImgNode -> Bool) ->
          ImgNode ->
          ObjId ->
          Name -> Cmd ObjId
mkImg' mkj isN v i n = dt >>= go
  where
    go t = do
      (d, t') <- liftE $ mkNode isN n i v t
      theImgTree .= t'
      journalChange $ mkj i n
      return d

mkImgDir :: ObjId -> Name -> Cmd ObjId
mkImgDir = mkImg' MkDIR isDIR emptyImgDir
{-# INLINE mkImgDir #-}

mkImgCol :: ObjId -> Name -> Cmd ObjId
mkImgCol = mkImg' MkCOL isCOL emptyImgCol
{-# INLINE mkImgCol #-}

mkImg :: ObjId -> Name -> Cmd ObjId
mkImg = mkImg' MkIMG isDIR emptyImg
{-# INLINE mkImg #-}

rmImgNode :: ObjId -> Cmd ()
rmImgNode i = do
  ex <- existsObjId i
  if ex
    then dt >>= go
    else warn $ "rmImgNode: ObjId doesn't exist: " ++ i ^. isoString
  where
    go t = do
      -- journal output must be done first, before calling removeImgNode
      journalChange $ RmObj i
      t' <- liftE $ removeImgNode i t
      theImgTree .= t'

-- ----------------------------------------
--
-- simple "file system" ops

-- create a new empty subcollection and append it to the colrefs

mkCollection :: Path -> Cmd ObjId
mkCollection = mkCollection' (\ c cs -> cs ++ [c])

-- create a new empty subcollection and cons it to the colrefs
mkCollectionC :: Path -> Cmd ObjId
mkCollectionC = mkCollection' (:)

mkCollection' :: (ColEntry -> [ColEntry] -> [ColEntry]) ->
                 Path -> Cmd ObjId
mkCollection' merge target'path = do
  -- parent exists
  (parent'id, parent'node) <- getIdNode "mkCollection: parent doesn't exist" parent'path

  -- parent is a collection
  -- TODO exists check
  unless (isCOL parent'node) $
    abort $ "mkCollection: parent isn't a collection " ++ quotePath parent'path

  -- check collection does not yet exist
  alreadyTherePath "mkCollection: target collection already exists" target'path

  -- create a new empty collection and append it to the parent collection
  col'id <- mkImgCol parent'id target'name
  adjustColEntries (merge $ mkColColRef col'id) parent'id
  return col'id
  where
    (parent'path, target'name) = target'path ^. viewBase

-- ----------------------------------------
--
-- basic modification commands

adjustImg :: (ImgParts -> ImgParts) -> ObjId -> Cmd ()
adjustImg = adjustNodeVal AdjImgParts theParts

adjustDirEntries :: (DirEntries -> DirEntries) -> ObjId -> Cmd ()
adjustDirEntries = adjustNodeVal AdjDirEntries theDirEntries

adjustMetaData :: (MetaData -> MetaData) -> ObjId -> Cmd ()
adjustMetaData = adjustNodeVal AdjMetaData theMetaData

adjustColImg :: (Maybe ImgRef -> Maybe ImgRef) -> ObjId -> Cmd ()
adjustColImg = adjustNodeVal AdjColImg theColImg

adjustColBlog :: (Maybe ImgRef -> Maybe ImgRef) -> ObjId -> Cmd ()
adjustColBlog = adjustNodeVal AdjColBlog theColBlog

adjustColEntries :: ([ColEntry] -> [ColEntry]) -> ObjId -> Cmd ()
adjustColEntries = adjustNodeVal AdjColEntries theColEntries

remColEntry :: Int -> ObjId -> Cmd ()
remColEntry pos = adjustColEntries (removeAt pos)

setSyncTime :: ObjId -> Cmd ()
setSyncTime i = do
  t <- now
  adjustNodeVal SetSyncTime theSyncTime (const t) i

adjustNodeVal :: Show a =>
                 (ObjId -> a -> Journal) ->
                 Traversal' ImgNode a -> (a -> a) -> ObjId -> Cmd ()
adjustNodeVal mkj theComp f i = do
  theImgTree . theNodeVal i . theComp %= f
  -- journal the changed result
  dt >>= journalAdjust
  where
    journalAdjust t =
      case t ^.. theNodeVal i . theComp of
        -- the expected case
        [new'v] ->
          journalChange $ mkj i new'v

        -- the error cases
        [] ->
          warn $ "adjustNodeVal: nothing changed, component to be modified does not exist in " ++ name'i

        -- the critical case
        vs ->
          warn $ "adjustNodeVal: mulitple components have been changed in " ++ name'i ++ ": " ++ show vs
      where
        name'i = show $ i ^. isoString

-- ----------------------------------------
--
-- search, sort and merge ops for collections

findAllColEntries :: (ColEntry -> Cmd Bool) -> ObjId -> Cmd [(Int, ColEntry)]
findAllColEntries p i = do
  es <- getImgVals i theColEntries
  filterM (p . snd) $ zip [0..] es

findFstColEntry  :: (ColEntry -> Cmd Bool) -> ObjId -> Cmd (Maybe (Int, ColEntry))
findFstColEntry p i = listToMaybe <$> findAllColEntries p i


sortColEntries :: (ColEntry -> Cmd a) ->
                  (a -> a -> Ordering) ->
                  [ColEntry] -> Cmd [ColEntry]
sortColEntries getVal cmpVal es =
  map fst . sortBy (cmpVal `on` snd) <$> mapM mkC es
  where
    -- mkC :: ColEntry -> Cmd (ColEntry, a)
    mkC ce = do
      v <- getVal ce
      return (ce, v)


-- merge old and new entries
-- old entries are removed from list of new entries
-- the remaining new entries are appended

mergeColEntries :: [ColEntry] -> [ColEntry] -> [ColEntry]
mergeColEntries es1 es2 =
  es1 ++ filter (`notElem` es1) es2

-- get the collection entry at an index pos
-- if it's not there an error is thrown
colEntryAt :: Int -> ImgNode -> Cmd ColEntry
colEntryAt pos n =
  maybe
  (abort $ "colEntryAt: illegal index in collection: " ++ show pos)
  return
  (n ^? theColEntries . ix pos)

-- process a collection entry at an index pos
-- if the entry isn't there, an error is thrown
processColEntryAt :: (ImgRef -> Cmd a) ->
                     (ObjId  -> Cmd a) ->
                     Int ->
                     ImgNode -> Cmd a
processColEntryAt imgRef colRef pos n =
  colEntryAt pos n >>=
  colEntry' imgRef colRef

-- process a collection image entry at an index pos
-- if the entry isn't there, an error is thrown
processColImgEntryAt :: Monoid a =>
                        (ImgRef -> Cmd a) ->
                        Int ->
                        ImgNode -> Cmd a
processColImgEntryAt imgRef =
  processColEntryAt imgRef (const $ return mempty)

-- ----------------------------------------
--
-- basic Cmd combinators

fromJustCmd :: String -> Maybe a -> Cmd a
fromJustCmd _   (Just x) = return x
fromJustCmd msg Nothing  = abort msg

liftE :: Except String a -> Cmd a
liftE cmd =
  case runExcept cmd of
    Left  msg -> abort msg
    Right res -> return res

catchAll :: Cmd () -> Cmd ()
catchAll c =
  c `catchError` (\ e -> warn $ "catchAll: error caught: " ++ show e)

runDry :: String -> Cmd () -> Cmd ()
runDry msg cmd = do
  dry <- view envDryRun
  if dry
    then do
      logg (^. envDryRun) "dry-run" msg
    else do
      verbose $ msg
      cmd

-- ----------------------------------------
--
-- trace commands

trcObj :: ObjId -> String -> Cmd ()
trcObj r msg = dt >>= \ t ->
  trc $ msg ++ " " ++ show (r, refPath r t)

trcCmd :: Show a => Cmd a -> Cmd a
trcCmd cmd
  = do res <- cmd
       trc $ "cmd: res = " ++ show res
       return res

journalChange :: Journal -> Cmd ()
journalChange j = do
  j' <- toJournalPath j
  logg (^. envJournal) "journal" (show j')

toJournalPath :: Journal -> Cmd (Journal' Path)
toJournalPath j = dt >>= go
  where
    go t = return ((`refPath` t) <$> j)

-- ----------------------------------------

buildImgPath0 :: ImgRef -> Cmd FilePath
buildImgPath0 (ImgRef i n) = do
  p <- objid2path i
  return $ substPathName n p ^. isoString

buildImgPath :: ImgRef -> Cmd FilePath
buildImgPath ir = addJpg <$> buildImgPath0 ir

-- ----------------------------------------
--
-- basic ops for files system paths

toSysPath :: FilePath -> Cmd SysPath
toSysPath fp@('/' : _) = (fmap (++ fp)) <$> view envMountPath
toSysPath fp           = toSysPath ('/' : fp)

-- build a file system path from an internal path
-- remove the redundant "/archive" top level dir

path2SysPath :: Path -> Cmd SysPath
path2SysPath p =
  toSysPath $ tailPath p ^. isoString

-- build a file system path from an internal image path
--
-- "/archive/photos/2016/emil"
-- -->
-- "<mountpath>/cache/exif-meta/photos/2016/emil.json"

path2ExifSysPath :: Path -> Cmd SysPath
path2ExifSysPath ip =
  toSysPath $ ps'exifcache ++ tailPath ip ^. isoString ++ ".json"

-- --------------------