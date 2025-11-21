------------------------------------------------------------------------------

module Catalog.ImgTree.Access
  ( -- * basic image tree access
    dt
  , getTreeAt
  , getImgName
  , getImgParent
  , getImgVal
  , getImgVals
  , getImgSubDirs
  , getMetaData
  , getImgMetaData
  , getRootId
  , getRootImgDirId
  , getRootImgColId
  , existsEntry
  , lookupByName
  , lookupByPath
  , getIdNode
  , getIdNode'
  , getId
  , getNode
  , alreadyTherePath
  , objid2path
  , objid2type
  , objid2list
  , objid2contNames
  , isPartOfTree
  , mapObjId2Path
  , mapImgStore2Path
  , mapPath2ObjId
  , mapImgStore2ObjId
  , findAllColEntries
  , findFstColEntry
  , sortColEntries
  , mergeColEntries
  , colEntryAt
  , processColEntryAt
  , processColImgEntryAt
  , foldObjIds
  , filterObjIds
  , bitsUsedInImgTreeMap
  )
where

import Catalog.Effects
       ( Sem
       , EffIStore
       , Eff'ISE
       , get
       , throw
       )

import Data.ImageStore
       ( ImgStore
       , ImgStore'
       , mapImgStore
       , theImgTree
       )
import Data.ImgTree
       ( ColEntries
       , ColEntryM
       , DirEntries
       , ImgNode
       , ImgRef
       , ImgRef'(ImgRef)
       , ImgTree
       , ObjIds
       , UplNode
       , colEntryM'
       , entryAt
       , isDIR
       , isIMG
       , isROOT
       , isoDirEntries
       , isoImgParts
       , keysImgTree
       , lookupImgPath
       , nodeName
       , nodeVal
       , parentRef
       , refInTree
       , refObjIdPath
       , refPath
       , rootRef
       , theColEntries
       , theDirEntries
       , theImgCol
       , theImgMeta
       , theImgName
       , theImgPart
       , theImgRoot
       , theMetaData
       , theParts
       , theRootImgCol
       , theRootImgDir
       )

import Data.MetaData
       ( MetaData
       , addFileMetaData
       )

import Data.Prim
       ( Path
       , Name
       , ObjId
       , mkObjId
       , msgPath
       , noOfBitsUsedInKeys
       , snocPath
       )
import Data.Prim.Prelude

import qualified Data.Set      as S
import qualified Data.Sequence as Seq

------------------------------------------------------------------------------
--
-- basic image tree access
--
-- only ImgStore get actions
-- and error effect used
-- no logging, no journaling

dt :: EffIStore r => Sem r ImgTree
dt = do
  s <- get
  return (s ^. theImgTree)
{-# INLINE dt #-}

getTreeAt :: EffIStore r => ObjId -> Sem r (Maybe UplNode)
getTreeAt i = do
  s <- get
  return (s ^. theImgTree . entryAt i)
{-# INLINE getTreeAt #-}

getTree' :: Eff'ISE r => Getting a UplNode a -> ObjId -> Sem r a
getTree' l i = do
  mn <- getTreeAt i
  case mn of
    Just n  -> return (n ^. l)
    Nothing -> throw $ "getTree': illegal ObjId: " <> i ^. isoText
{-# INLINE getTree' #-}

getImgName :: Eff'ISE r => ObjId -> Sem r Name
getImgName = getTree' nodeName
{-# INLINE getImgName #-}

getImgParent :: Eff'ISE r => ObjId -> Sem r ObjId
getImgParent = getTree' parentRef
{-# INLINE getImgParent #-}

getImgVal :: Eff'ISE r => ObjId -> Sem r ImgNode
getImgVal = getTree' nodeVal
{-# INLINE getImgVal #-}

getImgVals :: Eff'ISE r => ObjId -> Getting a ImgNode a -> Sem r a
getImgVals i l = getTree' (nodeVal . l) i
{-# INLINE getImgVals #-}

getImgSubDirs :: Eff'ISE r => DirEntries -> Sem r (Seq ObjId)
getImgSubDirs es =
  filterSeqM (\ i' -> getImgVals i' (to isDIR)) (es ^. isoDirEntries)

-- ----------------------------------------

getRootId :: EffIStore r => Sem r ObjId
getRootId = do
  s <- get
  return (s ^. theImgTree . rootRef)
{-# INLINE getRootId #-}

getRootImgDirId :: Eff'ISE r => Sem r ObjId
getRootImgDirId = do
  ri <- getRootId
  getImgVals ri theRootImgDir
{-# INLINE getRootImgDirId #-}

getRootImgColId :: Eff'ISE r => Sem r ObjId
getRootImgColId = do
  ri <- getRootId
  getImgVals ri theRootImgCol
{-# INLINE getRootImgColId #-}

existsEntry :: EffIStore r => ObjId -> Sem r Bool
existsEntry i = do
  s <- get
  return $ isJust (s ^. theImgTree . entryAt i)
{-# INLINE existsEntry #-}

lookupByName :: EffIStore r => Name -> ObjId -> Sem r (Maybe (ObjId, ImgNode))
lookupByName n i = do
  p <- (`snocPath` n) <$> objid2path i
  lookupByPath p

lookupByPath :: EffIStore r => Path -> Sem r (Maybe (ObjId, ImgNode))
lookupByPath p = lookupImgPath p <$> dt
{-# INLINE lookupByPath #-}

-- save lookup by path

getIdNode :: Eff'ISE r => Text -> Path -> Sem r (ObjId, ImgNode)
getIdNode msg p = do
  mv <- lookupImgPath p <$> dt
  case mv of
    Nothing ->
      throw $ msgPath p msg
    Just res ->
      return res

getIdNode' :: Eff'ISE r => Path -> Sem r (ObjId, ImgNode)
getIdNode' = getIdNode "cant' find entry for path:"
{-# INLINE getIdNode' #-}

getId :: Eff'ISE r => Path -> Sem r ObjId
getId p = fst <$> getIdNode' p
{-# INLINE getId #-}

getNode :: (Eff'ISE r) => Path -> Sem r ImgNode
getNode p = snd <$> getIdNode' p
{-# INLINE getNode #-}

-- check path not there

alreadyTherePath :: Eff'ISE r => Text -> Path -> Sem r ()
alreadyTherePath msg p = do
  whenM (isJust <$> lookupByPath p) $
    throw $ msgPath p msg

-- ----------------------------------------
--
-- | ref to path

objid2path :: EffIStore r => ObjId -> Sem r Path
objid2path i = refPath i <$> dt

objid2list :: EffIStore r => ObjId -> Sem r [ObjId]
objid2list i = refObjIdPath i <$> dt

isPartOfTree :: EffIStore r => ObjId -> ObjId -> Sem r Bool
isPartOfTree r p = refInTree r p <$> dt


-- | ref to type
objid2type :: Eff'ISE r => ObjId -> Sem r Text
objid2type i = getImgVal i >>= go
  where
    go e = return $ mconcat $
      e ^.. ( theParts      . to (const "IMG")  <>
              theDirEntries . to (const "DIR")  <>
              theImgRoot    . to (const "Root") <>
              theImgCol     . to (const "COL")
            )

objid2contNames :: Eff'ISE r => ObjId -> Sem r [Name]
objid2contNames i =
  getImgVal i >>= go
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

getMetaData :: Eff'ISE r => ObjId -> Sem r MetaData
getMetaData i = getImgVals i theMetaData

getImgMetaData :: Eff'ISE r => ImgRef -> Sem r MetaData
getImgMetaData (ImgRef i nm) = do
  partMD <- getImgVals  i (theImgPart nm . theImgMeta)
  imgMD  <- getMetaData i
  p      <- objid2path i
  return $ addFileMetaData p nm (partMD <> imgMD)

-- ----------------------------------------

-- replace an ObjId by a Path in an arbitrary functor, e.g. ImgNode'
-- ObjId, the internal key type, can be implemented more efficiently
-- than taking the whole path as key, e.g. by a hash. But the path variant
-- is more readable, and it's needed by clients, cilents can't handle
-- internal keys, like hashes or similar data

mapObjId2Path :: (EffIStore r, Functor f) => f ObjId -> Sem r (f Path)
mapObjId2Path x =
  (<$> x) <$> objid2pathMap
  -- do f <- objid2pathMap
  --    return $ fmap f x

-- convert an ImgStore to an isomorphic ImgStore' Path with paths as keys
mapImgStore2Path :: EffIStore r => Sem r (ImgStore' Path)
mapImgStore2Path = do
  x <- get
  (`mapImgStore` x) <$> objid2pathMap

-- get the mapping from internal keys, ObjId, to paths as keys
objid2pathMap :: EffIStore r => Sem r (ObjId -> Path)
objid2pathMap = flip refPath <$> dt

mapPath2ObjId :: Functor f => f Path -> f ObjId
mapPath2ObjId = fmap mkObjId

mapImgStore2ObjId :: ImgStore' Path -> ImgStore
mapImgStore2ObjId = mapImgStore mkObjId

-- ----------------------------------------
--
-- search, sort and merge ops for collections

findAllColEntries :: Eff'ISE r
                  => (ColEntryM -> Sem r Bool)    -- ^ the filter predicate
                  -> ObjId                       -- ^ the collection
                  -> Sem r [(Int, ColEntryM)]     -- ^ the list of entries with pos
findAllColEntries p i = do
  es <- getImgVals i theColEntries
  filterM (p . snd) $ zip [0..] (es ^. isoSeqList)
{-# INLINE findAllColEntries #-}

findFstColEntry  :: Eff'ISE r
                 => (ColEntryM -> Sem r Bool)
                 -> ObjId
                 -> Sem r (Maybe (Int, ColEntryM))
findFstColEntry p i = listToMaybe <$> findAllColEntries p i
{-# INLINE findFstColEntry #-}


sortColEntries :: Eff'ISE r
               => (ColEntryM -> Sem r a)
               -> (a -> a -> Ordering)
               -> ColEntries
               -> Sem r ColEntries
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

colEntryAt :: Eff'ISE r => Int -> ImgNode -> Sem r ColEntryM
colEntryAt pos n =
  maybe
    (throw $ "colEntryAt: illegal index in collection: " <> pos ^. isoText)
    return
    (n ^? theColEntries . ix pos)


-- process a collection entry at an index pos
-- if the entry isn't there, an error is thrown

processColEntryAt :: Eff'ISE r
                  => (ImgRef -> Sem r a)
                  -> (ObjId  -> Sem r a)
                  -> Int
                  -> ImgNode
                  -> Sem r a
processColEntryAt imgRef colRef pos n =
  colEntryAt pos n >>=
  colEntryM' imgRef colRef


-- process a collection image entry at an index pos
-- if the entry isn't there, an error is thrown

processColImgEntryAt :: (Eff'ISE r, Monoid a)
                     => (ImgRef -> Sem r a)
                     -> Int
                     -> ImgNode
                     -> Sem r a
processColImgEntryAt imgRef =
  processColEntryAt imgRef (const $ return mempty)


-- --------------------

foldObjIds :: (Monoid m) => (ObjId -> Sem r m) -> ObjIds -> Sem r m
foldObjIds cmd os = mconcat <$> traverse cmd (toList os)
{-# INLINE foldObjIds #-}

filterObjIds :: Eff'ISE r => (ImgNode -> Bool) -> ObjIds -> Sem r ObjIds
filterObjIds p =
  foldObjIds sel
  where
    sel :: Eff'ISE r => ObjId -> Sem r ObjIds
    sel i = do
      v <- getImgVal i
      return $
        if p v
        then S.singleton i
        else S.empty

-- ----------------------------------------

bitsUsedInImgTreeMap :: EffIStore r => Sem r Int
bitsUsedInImgTreeMap = do
  t <- dt
  return (noOfBitsUsedInKeys . keysImgTree $ t)

-- ----------------------------------------
