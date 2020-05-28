{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Cmd.Basic
  ( -- basic Cmd's
    dt
  , getTreeAt
  , getImgName
  , getImgParent
  , getImgVal
  , getImgVals
  , getImgSubDirs
  , getMetaData
  , getRootId
  , getRootImgDirId
  , getRootImgColId
  , existsEntry
  , lookupByName
  , lookupByPath
  , getIdNode
  , getIdNode'
  , objid2path
  , objid2type
  , objid2list
  , objid2contNames
  , isPartOfTree
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
  , adjustColEntry
  , remColEntry
  , setSyncTime
  , findAllColEntries
  , findFstColEntry
  , sortColEntries
  , mergeColEntries
  , colEntryAt
  , processColEntryAt
  , processColImgEntryAt

    -- * basic combinators
  , liftE
  , catchAll
  , runDry
  , trcObj
  , warnObj
  , verbObj
  , trcCmd
  , journalChange
  , buildImgPath0
  , buildImgPath

    -- * file system path
  , toSysPath
  , path2SysPath

    -- * ObjIds
  , filterObjIds
  , foldObjIds
  )
where

import           Catalog.Cmd.Types
import           Catalog.FilePath  (addJpg)
import           Data.Journal
import           Data.ImageStore
import           Data.ImgTree
import           Data.MetaData
import           Data.Prim

import qualified Data.Set      as S
import qualified Data.Sequence as Seq

-- ----------------------------------------

-- simple monadic ops

dt :: Cmd ImgTree
dt = use theImgTree
{-# INLINE dt #-}

getTreeAt :: ObjId -> Cmd (Maybe UplNode)
getTreeAt i = use (theImgTree . entryAt i)
{-# INLINE getTreeAt #-}

getTree' :: Getting a UplNode a -> ObjId -> Cmd a
getTree' l i = do
  mn <- getTreeAt i
  case mn of
    Just n  -> return (n ^. l)
    Nothing -> abort $ "getTree: illegal ObjId: " ++ show i

getImgName :: ObjId -> Cmd Name
getImgName = getTree' nodeName
{-# INLINE getImgName #-}

getImgParent :: ObjId -> Cmd ObjId
getImgParent = getTree' parentRef
{-# INLINE getImgParent #-}

getImgVal :: ObjId -> Cmd ImgNode
getImgVal = getTree' nodeVal
{-# INLINE getImgVal #-}

getImgVals :: ObjId -> Getting a ImgNode a -> Cmd a
getImgVals i l = getTree' (nodeVal . l) i
{-# INLINE getImgVals #-}

getImgSubDirs :: DirEntries -> Cmd (Seq ObjId)
getImgSubDirs es =
  filterSeqM (\ i' -> getImgVals i' (to isDIR)) (es ^. isoDirEntries)

-- ----------------------------------------

getRootId :: Cmd ObjId
getRootId = use (theImgTree . rootRef)
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

existsEntry :: ObjId -> Cmd Bool
existsEntry i =
  isJust <$> use (theImgTree . entryAt i)
{-# INLINE existsEntry #-}

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
--
-- | ref to path
objid2path :: ObjId -> Cmd Path
objid2path i = dt >>= return . refPath i

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

objid2list :: ObjId -> Cmd [ObjId]
objid2list i =
  dt >>= return . refObjIdPath i

isPartOfTree :: ObjId -> ObjId -> Cmd Bool
isPartOfTree r p =
  dt >>= return . refInTree r p

objid2contNames :: ObjId -> Cmd [Name]
objid2contNames i = getImgVal i >>= go
  where
    go e
      | isIMG e =
          return (e ^. theParts . isoImgParts . traverse . theImgName . to (:[]))

      | isDIR e =
          traverse getImgName (e ^. theDirEntries . isoDirEntries . isoSeqList)

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
objid2pathMap = dt >>= return . flip refPath

mapPath2ObjId :: Functor f => f Path -> f ObjId
mapPath2ObjId = fmap mkObjId

mapImgStore2ObjId :: ImgStore' Path -> ImgStore
mapImgStore2ObjId = mapImgStore mkObjId

-- ----------------------------------------
--
-- smart constructors

-- make an image catalog node

mkImg' :: (ObjId -> Name -> Journal)  -- ^ journal action
       -> (ImgNode -> Bool)           -- ^ parent editable
       -> ImgNode                     -- ^ the catalog node value
       -> ObjId                       -- ^ parent node
       -> Name                        -- ^ the name of the node
       -> Cmd ObjId                   -- ^ the new ref
mkImg' mkj isN v i n = dt >>= go
  where
    go t = do
      (d, t') <- liftE $ mkNode isN n i v t
      theImgTree .= t'
      journalChange $ mkj i n
      return d

-- create a new empty DIR node
mkImgDir :: ObjId -> Name -> Cmd ObjId
mkImgDir = mkImg' MkDIR isDIR emptyImgDir
{-# INLINE mkImgDir #-}

-- create a new empty COL node
mkImgCol :: ObjId -> Name -> Cmd ObjId
mkImgCol = mkImg' MkCOL isCOL emptyImgCol
{-# INLINE mkImgCol #-}

-- create a new empty IMG node
mkImg :: ObjId -> Name -> Cmd ObjId
mkImg = mkImg' MkIMG isDIR emptyImg
{-# INLINE mkImg #-}

-- remove an entry from catalog tree
rmImgNode :: ObjId -> Cmd ()
rmImgNode i = do
  ex <- existsEntry i
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
mkCollection = mkCollection' $ flip (Seq.|>)

-- create a new empty subcollection and cons it to the colrefs
mkCollectionC :: Path -> Cmd ObjId
mkCollectionC = mkCollection' (Seq.<|)

mkCollection' :: (ColEntry -> ColEntries -> ColEntries)
              -> Path
              -> Cmd ObjId
mkCollection' merge target'path = do
  -- parent exists
  (parent'id, parent'node) <-
    getIdNode "mkCollection: parent doesn't exist" parent'path

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

adjustColEntries :: (ColEntries -> ColEntries) -> ObjId -> Cmd ()
adjustColEntries = adjustNodeVal AdjColEntries theColEntries

adjustColEntry :: (ColEntry -> ColEntry) -> Int -> ObjId -> Cmd ()
adjustColEntry f i = adjustColEntries f'
  where
    f' :: ColEntries -> ColEntries
    f' = Seq.adjust f i

remColEntry :: Int -> ObjId -> Cmd ()
remColEntry pos = adjustColEntries (Seq.deleteAt pos)

setSyncTime :: ObjId -> Cmd ()
setSyncTime i = do
  t <- now
  adjustNodeVal SetSyncTime theSyncTime (const t) i

adjustNodeVal :: Show a
              => (ObjId -> a -> Journal)
              -> Traversal' ImgNode a
              -> (a -> a)
              -> ObjId
              -> Cmd ()
adjustNodeVal mkj theComp f i = do
  theImgTree . entryAt i . traverse . nodeVal . theComp %= f
  -- journal the changed result
  dt >>= journalAdjust
    where
      journalAdjust t =
        case t ^.. entryAt i . traverse . nodeVal . theComp of
          -- the expected case
          [new'v] ->
            journalChange $ mkj i new'v

          -- the error cases
          [] ->
            warn $ "adjustNodeVal: nothing changed, "
                   <> "component to be modified does not exist in "
                   <> name'i

          -- the critical case
          vs ->
            warn $ "adjustNodeVal: mulitple components have been changed in "
                   <> name'i
                   <> ": "
                   <> show vs
        where
          name'i = show $ i ^. isoString

-- ----------------------------------------
--
-- search, sort and merge ops for collections

findAllColEntries :: (ColEntry -> Cmd Bool)    -- ^ the filter predicate
                  -> ObjId                     -- ^ the collection
                  -> Cmd [(Int, ColEntry)]     -- ^ the list of entries with pos
findAllColEntries p i = do
  es <- getImgVals i theColEntries
  filterM (p . snd) $ zip [0..] (es ^. isoSeqList)

findFstColEntry  :: (ColEntry -> Cmd Bool)
                 -> ObjId
                 -> Cmd (Maybe (Int, ColEntry))
findFstColEntry p i = listToMaybe <$> findAllColEntries p i


sortColEntries :: (ColEntry -> Cmd a)
               -> (a -> a -> Ordering)
               -> ColEntries
               -> Cmd ColEntries
sortColEntries getVal cmpVal es =
  fmap fst . Seq.sortBy (cmpVal `on` snd) <$> traverse mkC es
  where
    -- mkC :: ColEntry -> Cmd (ColEntry, a)
    mkC ce = do
      v <- getVal ce
      return (ce, v)

-- merge old and new entries
-- old entries are removed from list of new entries
-- the remaining new entries are appended

mergeColEntries :: ColEntries -> ColEntries -> ColEntries
mergeColEntries es1 es2 =
  es1 <> Seq.filter (`notElem` es1) es2


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

processColEntryAt :: (ImgRef -> Cmd a)
                  -> (ObjId  -> Cmd a)
                  -> Int
                  -> ImgNode
                  -> Cmd a
processColEntryAt imgRef colRef pos n =
  colEntryAt pos n >>=
  colEntry' imgRef colRef


-- process a collection image entry at an index pos
-- if the entry isn't there, an error is thrown

processColImgEntryAt :: Monoid a
                     => (ImgRef -> Cmd a)
                     -> Int
                     -> ImgNode -> Cmd a
processColImgEntryAt imgRef =
  processColEntryAt imgRef (const $ return mempty)

-- ----------------------------------------
--
-- basic Cmd combinators

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
-- log an dtrace commands

logObj :: (String -> Cmd ()) -> ObjId -> String -> Cmd ()
logObj lg r msg = dt >>= \ t ->
  lg $ msg ++ " " ++ show (r, refPath r t)

trcObj, warnObj, verbObj :: ObjId -> String -> Cmd ()
trcObj  = logObj trc
warnObj = logObj warn
verbObj = logObj verbose

trcCmd :: Show a => Cmd a -> Cmd a
trcCmd cmd
  = do res <- cmd
       trc $ "cmd: res = " ++ show res
       return res

journalChange :: Journal -> Cmd ()
journalChange j = do
  j' <- traverse objid2path j
  logg (^. envJournal) "journal" (show j')

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

-- --------------------

foldObjIds :: (Monoid m) => (ObjId -> Cmd m) -> ObjIds -> Cmd m
foldObjIds cmd os = mconcat <$> traverse cmd (toList os)
{-# INLINE foldObjIds #-}

filterObjIds :: (ImgNode -> Bool) -> ObjIds -> Cmd ObjIds
filterObjIds p =
  foldObjIds sel
  where
    sel i = do
      v <- getImgVal i
      return $
        if p v
        then S.singleton i
        else S.empty

-- --------------------
