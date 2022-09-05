------------------------------------------------------------------------------

module Catalog.ImgTree.Access
  ( -- * basic image tree access
    liftTF
  , withTF
  , toTF
  , toTF'
  , itoTF
  , itoTF'

  , dt
  , getTreeAt
  , getTreeAtMaybe
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

  , findAllColEntries'
  , findFstColEntry'
  , findFstTxtEntry'
  , findFstPosEntry'
  )
where

import Catalog.Effects

import Data.ImageStore
       ( ImgStore
       , ImgStore'
       , mapImgStore
       , theImgTree
       )
import Data.ImgTree
import Data.MetaData
       ( MetaData )

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

-- the only action used for state access

withTF :: EffIStore r => (ImgTree -> Sem r a) -> Sem r a
withTF cmd = liftTF id >>= cmd

liftTF :: EffIStore r => (ImgTree -> a) -> Sem r a
liftTF f = view (theImgTree . to f) <$> get
{-# INLINE liftTF #-}

toTF :: Getting a ImgTree a -> (ImgTree -> a)
toTF = view
{-# INLINE toTF #-}

toTF' :: Getting a ImgTree a -> (ImgTree -> a -> b) -> (ImgTree -> b)
toTF' gt f = \ t -> f t (t ^. gt)
{-# INLINE toTF' #-}

itoTF :: Getting a UplNode a -> ObjId -> (ImgTree -> a)
itoTF gt i = toTF (entryAt i . gt)
{-# INLINE itoTF #-}

itoTF' :: Getting a UplNode a -> (ImgTree -> a -> b) -> ObjId -> (ImgTree -> b)
itoTF' gt f i = toTF' (entryAt i . gt) f
{-# INLINE itoTF' #-}

{-
liftTF :: EffIStore r
      => Getting a ImgTree a -> (ImgTree -> a -> b) -> Sem r b
liftTF gt f = liftTF (toTF' gt f)


getItAt :: EffIStore r => (ImgTree -> UplNode -> a) -> ObjId -> Sem r a
getItAt tf i = liftTF $ toTF' (entryAt i) tf
-}
{-
getAt :: EffIStore r
      => (ImgTree -> a -> b) -> Getting a UplNode a -> ObjId -> Sem r b
getAt tf gt i = liftTF $ itoTF' gt tf i

getAt0 :: EffIStore r => Getting a UplNode a -> ObjId -> Sem r a
getAt0 gt i = liftTF $ itoTF gt i

-- --------------------

getOptAt' :: EffIStore r => Fold UplNode a -> ObjId -> Sem r (Maybe a)
getOptAt' theA i = liftTF $ itoTF (toMaybe (filtered isn'tempty .  theA)) i
{-# INLINE getOptAt' #-}
-}

-- ----------

getTreeAt :: EffIStore r => ObjId -> Sem r UplNode
getTreeAt i = liftTF $ itoTF id i
{-# INLINE getTreeAt #-}

getTreeAtMaybe :: EffIStore r => ObjId -> Sem r (Maybe UplNode)
getTreeAtMaybe i = liftTF $ itoTF (toMaybe (filtered isn'tempty)) i
{-# INLINE getTreeAtMaybe #-}

-- --------------------

dt :: EffIStore r => Sem r ImgTree
dt = liftTF id
{-# INLINE dt #-}

getImgName :: Eff'ISE r => ObjId -> Sem r Name
getImgName i = liftTF $ itoTF nodeName i
{-# INLINE getImgName #-}

getImgParent :: Eff'ISE r => ObjId -> Sem r ObjId
getImgParent i = liftTF $ itoTF parentRef i
{-# INLINE getImgParent #-}

getImgVal :: Eff'ISE r => ObjId -> Sem r ImgNode
getImgVal i = liftTF $ itoTF nodeVal i
{-# INLINE getImgVal #-}

getImgVals :: Eff'ISE r => ObjId -> Getting a ImgNode a -> Sem r a
getImgVals i l = liftTF $ itoTF (nodeVal . l) i
{-# INLINE getImgVals #-}

getImgSubDirs :: Eff'ISE r => DirEntries -> Sem r (Seq ObjId)
getImgSubDirs es = liftTF f
  where
    f :: ImgTree -> Seq ObjId
    f t = filterDirEntries p es ^. isoDirEntries
      where
        p i = (t, i) ^. theNode . to isDIR

-- ----------------------------------------

getRootId :: EffIStore r => Sem r ObjId
getRootId = liftTF $ toTF rootRef
{-# INLINE getRootId #-}

getRootImgDirId :: Eff'ISE r => Sem r ObjId
getRootImgDirId = liftTF $ toTF (isoNavTree . theNode . theRootImgDir)
{-# INLINE getRootImgDirId #-}

getRootImgColId :: Eff'ISE r => Sem r ObjId
getRootImgColId = liftTF $ toTF (isoNavTree . theNode . theRootImgCol)
{-# INLINE getRootImgColId #-}

existsEntry :: EffIStore r => ObjId -> Sem r Bool
existsEntry i = liftTF $ itoTF' (toMaybe (filtered isn'tempty)) (const isJust) i

lookupByName :: EffIStore r => Name -> ObjId -> Sem r (Maybe (ObjId, ImgNode))
lookupByName n i = liftTF go
  where
    go t = lookupImgPath (refPath i t `snocPath` n) t

lookupByPath :: EffIStore r => Path -> Sem r (Maybe (ObjId, ImgNode))
lookupByPath p = liftTF $ lookupImgPath p
{-# INLINE lookupByPath #-}

-- save lookup by path

getIdNode :: Eff'ISE r => Text -> Path -> Sem r (ObjId, ImgNode)
getIdNode msg p = do
  mv <- liftTF $ lookupImgPath p
  case mv of
    Nothing ->
      throw $ msgPath p msg
    Just res ->
      return res

getIdNode' :: Eff'ISE r => Path -> Sem r (ObjId, ImgNode)
getIdNode' = getIdNode "cant' find entry for path:"

getId :: Eff'ISE r => Path -> Sem r ObjId
getId p = fst <$> getIdNode' p

-- check path not there

alreadyTherePath :: Eff'ISE r => Text -> Path -> Sem r ()
alreadyTherePath msg p = do
  whenM (isJust <$> lookupByPath p) $
    throw $ msgPath p msg

-- ----------------------------------------
--
-- | ref to path

objid2path :: EffIStore r => ObjId -> Sem r Path
objid2path i = liftTF $ refPath i

objid2list :: EffIStore r => ObjId -> Sem r [ObjId]
objid2list i = liftTF $ refObjIdPath i

isPartOfTree :: EffIStore r => ObjId -> ObjId -> Sem r Bool
isPartOfTree r p = liftTF $ refInTree r p


-- | ref to type
objid2type :: Eff'ISE r => ObjId -> Sem r Text
objid2type i = liftTF $ itoTF (nodeVal . to imgNodeType) i

objid2contNames :: Eff'ISE r => ObjId -> Sem r [Name]
objid2contNames i = liftTF $ itoTF' nodeVal names i
  where
    names :: ImgTree -> ImgNode -> [Name]
    names t n
      | isIMG  n  = n ^.. theParts . isoImgParts . traverse . theImgName
      | isDIR  n
        ||
        isROOT n  = n ^.. imgNodeRefs . to (t,)  . theEntry . nodeName
      | otherwise = []

getMetaData :: Eff'ISE r => ObjId -> Sem r MetaData
getMetaData i = liftTF $ itoTF (nodeVal . theMetaData) i

getImgMetaData :: Eff'ISE r => ImgRef -> Sem r MetaData
getImgMetaData (ImgRef i nm) = liftTF $ itoTF (nodeVal . to f) i
  where
    f :: ImgNode -> MetaData
    f n =
      n ^. theImgPart nm . theImgMeta
      <>
      n ^. theMetaData

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
objid2pathMap = liftTF $ flip refPath

mapPath2ObjId :: Functor f => f Path -> f ObjId
mapPath2ObjId = fmap mkObjId

mapImgStore2ObjId :: ImgStore' Path -> ImgStore
mapImgStore2ObjId = mapImgStore mkObjId

-- ----------------------------------------

findAllColEntries' :: Eff'ISE r
                  => (ImgTree -> ColEntry -> Bool)    -- ^ the filter predicate
                  -> ObjId                            -- ^ the collection
                  -> Sem r [(Int, ColEntry)]          -- ^ the list of entries with pos
findAllColEntries' p i = liftTF $ itoTF' (nodeVal . theColEntries) go i
  where
    go :: ImgTree -> ColEntries -> [(Int, ColEntry)]
    go t cs = filter (p t . snd) $ zip [0..] (cs ^. isoSeqList)

findFstColEntry'  :: Eff'ISE r
                  => (ImgTree -> ColEntry -> Bool)
                  -> ObjId
                  -> Sem r (Maybe (Int, ColEntry))
findFstColEntry' p i = listToMaybe <$> findAllColEntries' p i


findFstTxtEntry' :: Eff'ISEJL r => ObjId -> Sem r (Maybe (Int, ColEntry))
findFstTxtEntry' = findFstColEntry' isTxtEntry
  where
    isTxtEntry :: ImgTree -> ColEntry -> Bool
    isTxtEntry t =
      colEntry ist (const False)
      where
        ist :: ObjId -> Name -> Bool
        ist i n = maybe False isTxtMT ty
          where
          ty = (t, i) ^? theNode
                       . theParts
                       . isoImgPartsMap
                       . ix n
                       . theMimeType

findFstPosEntry' :: Eff'ISEJL r
                 => ObjId
                 -> ObjId -> Sem r Int
findFstPosEntry' i2 i =
  maybe (-1) fst <$> findFstColEntry' (const isPosEntry) i
  where
    isPosEntry :: ColEntry -> Bool
    isPosEntry ce = i2 == ce ^. theColObjId


-- ----------------------------------------
--
-- search, sort and merge ops for collections

findAllColEntries :: Eff'ISE r
                  => (ColEntry -> Sem r Bool)    -- ^ the filter predicate
                  -> ObjId                       -- ^ the collection
                  -> Sem r [(Int, ColEntry)]     -- ^ the list of entries with pos
findAllColEntries p i = do
  es <- getImgVals i theColEntries
  filterM (p . snd) $ zip [0..] (es ^. isoSeqList)
{-# INLINE findAllColEntries #-}

findFstColEntry  :: Eff'ISE r
                 => (ColEntry -> Sem r Bool)
                 -> ObjId
                 -> Sem r (Maybe (Int, ColEntry))
findFstColEntry p i = listToMaybe <$> findAllColEntries p i
{-# INLINE findFstColEntry #-}

sortColEntries :: Eff'ISE r
               => (ColEntry -> Sem r a)
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

colEntryAt :: Eff'ISE r => Int -> ImgNode -> Sem r ColEntry
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
  colEntry' imgRef colRef


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
bitsUsedInImgTreeMap = liftTF $ noOfBitsUsedInKeys . keysImgTree

-- ----------------------------------------
