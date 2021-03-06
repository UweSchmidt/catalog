{-# LANGUAGE
    ConstraintKinds,
    DataKinds,
    FlexibleContexts,
    GADTs,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)

{-# LANGUAGE OverloadedStrings #-}

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

import Data.ImageStore
import Data.ImgTree
import Data.MetaData
import Data.Prim

import qualified Data.Set      as S
import qualified Data.Sequence as Seq

------------------------------------------------------------------------------
--
-- basic image tree access
--
-- only ImgStore get actions
-- and error effect used
-- no logging, no journaling

dt :: SemIS r ImgTree
dt = do
  s <- get
  return (s ^. theImgTree)
{-# INLINE dt #-}

getTreeAt :: ObjId -> SemIS r (Maybe UplNode)
getTreeAt i = do
  s <- get
  return (s ^. theImgTree . entryAt i)
{-# INLINE getTreeAt #-}

getTree' :: Getting a UplNode a -> ObjId -> SemISE r a
getTree' l i = do
  mn <- getTreeAt i
  case mn of
    Just n  -> return (n ^. l)
    Nothing -> throw $ "getTree': illegal ObjId: " <> i ^. isoText
{-# INLINE getTree' #-}

getImgName :: ObjId -> SemISE r Name
getImgName = getTree' nodeName
{-# INLINE getImgName #-}

getImgParent :: ObjId -> SemISE r ObjId
getImgParent = getTree' parentRef
{-# INLINE getImgParent #-}

getImgVal :: ObjId -> SemISE r ImgNode
getImgVal = getTree' nodeVal
{-# INLINE getImgVal #-}

getImgVals :: ObjId -> Getting a ImgNode a -> SemISE r a
getImgVals i l = getTree' (nodeVal . l) i
{-# INLINE getImgVals #-}

getImgSubDirs :: DirEntries -> SemISE r (Seq ObjId)
getImgSubDirs es =
  filterSeqM (\ i' -> getImgVals i' (to isDIR)) (es ^. isoDirEntries)

-- ----------------------------------------

getRootId :: SemIS r ObjId
getRootId = do
  s <- get
  return (s ^. theImgTree . rootRef)
{-# INLINE getRootId #-}

getRootImgDirId :: SemISE r ObjId
getRootImgDirId = do
  ri <- getRootId
  getImgVals ri theRootImgDir
{-# INLINE getRootImgDirId #-}

getRootImgColId :: SemISE r ObjId
getRootImgColId = do
  ri <- getRootId
  getImgVals ri theRootImgCol
{-# INLINE getRootImgColId #-}

existsEntry :: ObjId -> SemIS r Bool
existsEntry i = do
  s <- get
  return $ isJust (s ^. theImgTree . entryAt i)
{-# INLINE existsEntry #-}

lookupByName :: Name -> ObjId -> SemIS r (Maybe (ObjId, ImgNode))
lookupByName n i = do
  p <- (`snocPath` n) <$> objid2path i
  lookupByPath p

lookupByPath :: Path -> SemIS r (Maybe (ObjId, ImgNode))
lookupByPath p = lookupImgPath p <$> dt
{-# INLINE lookupByPath #-}

-- save lookup by path

getIdNode :: Text -> Path -> SemISE r (ObjId, ImgNode)
getIdNode msg p = do
  mv <- lookupImgPath p <$> dt
  case mv of
    Nothing ->
      throw $ msgPath p msg
    Just res ->
      return res

getIdNode' :: Path -> SemISE r (ObjId, ImgNode)
getIdNode' p =
  getIdNode ("cant' find entry for path:") p

getId :: Path -> SemISE r ObjId
getId p = fst <$> getIdNode' p

-- check path not there

alreadyTherePath :: Text -> Path -> SemISE r ()
alreadyTherePath msg p = do
  whenM (isJust <$> lookupByPath p) $
    throw $ msgPath p msg

-- ----------------------------------------
--
-- | ref to path

objid2path :: ObjId -> SemIS r Path
objid2path i =
  dt >>= return . refPath i

objid2list :: ObjId -> SemIS r [ObjId]
objid2list i =
  dt >>= return . refObjIdPath i

isPartOfTree :: ObjId -> ObjId -> SemIS r Bool
isPartOfTree r p =
  dt >>= return . refInTree r p


-- | ref to type
objid2type :: ObjId -> SemISE r Text
objid2type i = getImgVal i >>= go
  where
    go e = return $ mconcat $
      e ^.. ( theParts      . to (const "IMG")  <>
              theDirEntries . to (const "DIR")  <>
              theImgRoot    . to (const "Root") <>
              theImgCol     . to (const "COL")
            )

objid2contNames :: ObjId -> SemISE r [Name]
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

getMetaData :: ObjId -> SemISE r MetaData
getMetaData i = getImgVals i theMetaData

getImgMetaData :: ImgRef -> SemISE r MetaData
getImgMetaData (ImgRef i nm) =
  (<>) <$> getImgVals i (theImgPart nm . theImgMeta)
       <*> getMetaData i

-- ----------------------------------------

-- replace an ObjId by a Path in an arbitrary functor, e.g. ImgNode'
-- ObjId, the internal key type, can be implemented more efficiently
-- than taking the whole path as key, e.g. by a hash. But the path variant
-- is more readable, and it's needed by clients, cilents can't handle
-- internal keys, like hashes or similar data

mapObjId2Path :: Functor f => f ObjId -> SemIS r (f Path)
mapObjId2Path x =
  (<$> x) <$> objid2pathMap
  -- do f <- objid2pathMap
  --    return $ fmap f x

-- convert an ImgStore to an isomorphic ImgStore' Path with paths as keys
mapImgStore2Path :: SemIS r (ImgStore' Path)
mapImgStore2Path = do
  x <- get
  (`mapImgStore` x) <$> objid2pathMap

-- get the mapping from internal keys, ObjId, to paths as keys
objid2pathMap :: SemIS r (ObjId -> Path)
objid2pathMap = dt >>= return . flip refPath

mapPath2ObjId :: Functor f => f Path -> f ObjId
mapPath2ObjId = fmap mkObjId

mapImgStore2ObjId :: ImgStore' Path -> ImgStore
mapImgStore2ObjId = mapImgStore mkObjId

-- ----------------------------------------
--
-- search, sort and merge ops for collections

findAllColEntries :: (ColEntry -> Sem r Bool)    -- ^ the filter predicate
                  -> ObjId                       -- ^ the collection
                  -> SemISE r [(Int, ColEntry)]  -- ^ the list of entries with pos
findAllColEntries p i = do
  es <- getImgVals i theColEntries
  filterM (p . snd) $ zip [0..] (es ^. isoSeqList)
{-# INLINE findAllColEntries #-}

findFstColEntry  :: (ColEntry -> Sem r Bool)
                 -> ObjId
                 -> SemISE r (Maybe (Int, ColEntry))
findFstColEntry p i = listToMaybe <$> findAllColEntries p i
{-# INLINE findFstColEntry #-}


sortColEntries :: (ColEntry -> Sem r a)
               -> (a -> a -> Ordering)
               -> ColEntries
               -> SemISE r ColEntries
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

colEntryAt :: Int -> ImgNode -> SemISE r ColEntry
colEntryAt pos n =
  maybe
    (throw $ "colEntryAt: illegal index in collection: " <> pos ^. isoText)
    return
    (n ^? theColEntries . ix pos)


-- process a collection entry at an index pos
-- if the entry isn't there, an error is thrown

processColEntryAt :: (ImgRef -> Sem r a)
                  -> (ObjId  -> Sem r a)
                  -> Int
                  -> ImgNode
                  -> SemISE r a
processColEntryAt imgRef colRef pos n =
  colEntryAt pos n >>=
  colEntry' imgRef colRef


-- process a collection image entry at an index pos
-- if the entry isn't there, an error is thrown

processColImgEntryAt :: Monoid a
                     => (ImgRef -> Sem r a)
                     -> Int
                     -> ImgNode
                     -> SemISE r a
processColImgEntryAt imgRef =
  processColEntryAt imgRef (const $ return mempty)


-- --------------------

foldObjIds :: (Monoid m) => (ObjId -> Sem r m) -> ObjIds -> Sem r m
foldObjIds cmd os = mconcat <$> traverse cmd (toList os)
{-# INLINE foldObjIds #-}

filterObjIds :: (ImgNode -> Bool) -> ObjIds -> SemISE r ObjIds
filterObjIds p =
  foldObjIds sel
  where
    sel i = do
      v <- getImgVal i
      return $
        if p v
        then S.singleton i
        else S.empty

-- ----------------------------------------

bitsUsedInImgTreeMap :: SemIS r Int
bitsUsedInImgTreeMap = do
  t <- dt
  return (noOfBitsUsedInKeys . keysImgTree $ t)

-- ----------------------------------------
