------------------------------------------------------------------------------

module Catalog.ImgTree.Fold
where

import Catalog.Effects
       ( EffError
       , EffIStore
       , Sem
       , throw
       )
import Catalog.ImgTree.Access
       ( getTreeAt )

import Data.ImgTree
       ( ColEntries
       , DirEntries
       , ImgNode'(COL, IMG, DIR, ROOT)
       , ImgParts
       , ImgRef
       , ImgRef'(_iref)
       , ObjIds
       , nodeVal
       , singleObjId
       , theColColRef
       , theColObjId
       )
import Data.MetaData
       ( MetaData )

import Data.Prim
       ( Foldable(fold)
       , IsoText(isoText)
       , ObjId
       , Text
       , TimeStamp
       , (^.)
       , (^..)
       )

-- ----------------------------------------

type Act r a = ObjId -> Sem r a

-- traverse a catalog object (@ObjId@) with all it's @ObjId@'s to
-- sub objects
-- for all object variants (IMG, DIR, COL and ROOT) a processing
-- funtion is given,
-- for the "inner" objects ROOt, DIR and COL the recursive call
-- of the fold is added as an extra param
-- so the processing functions can decide, when to make the recursive
-- call
-- illegal ObjIds (in an inconsistent catalog) are handled by @undefId@

foldMT' :: (EffIStore r)
        => (           ObjId                               -> Sem r a)  -- ^ undef id
        -> (           ObjId -> ImgParts     -> MetaData   -> Sem r a)  -- ^ IMG
        -> (Act r a -> ObjId -> DirEntries   -> TimeStamp  -> Sem r a)  -- ^ DIR
        -> (Act r a -> ObjId -> ObjId        -> ObjId      -> Sem r a)  -- ^ ROOT
        -> (Act r a -> ObjId -> MetaData     -> Maybe ImgRef
                             -> Maybe ImgRef -> ColEntries -> Sem r a)  -- ^ COL
        -> Act r a
foldMT' undefId imgA dirA' rootA' colA' i0 = do
  go i0
  where
    dirA  = dirA'  go
    rootA = rootA' go
    colA  = colA'  go
    go i  = do
      -- trcObj i $ "foldMT"
      mn <- getTreeAt i
      -- log'trc $ "foldMT: " <> toText mn
      case mn of
        Nothing ->
          undefId i
        Just n ->
          case n ^. nodeVal of
            IMG  pts md       -> imgA  i pts md
            DIR  es  ts       -> dirA  i es  ts
            ROOT dir col      -> rootA i dir col
            COL  md  im be es -> colA  i md  im be es

{-# INLINE foldMT' #-}

-- same as foldMT', but with defaut error handling

foldMT :: (EffIStore r, EffError r)
       => (           ObjId -> ImgParts     -> MetaData   -> Sem r a)  -- ^ IMG
       -> (Act r a -> ObjId -> DirEntries   -> TimeStamp  -> Sem r a)  -- ^ DIR
       -> (Act r a -> ObjId -> ObjId        -> ObjId      -> Sem r a)  -- ^ ROOT
       -> (Act r a -> ObjId -> MetaData     -> Maybe ImgRef
                            -> Maybe ImgRef -> ColEntries -> Sem r a)  -- ^ COL
       -> Act r a
foldMT = foldMT' undefId
  where
    undefId :: EffError r => ObjId -> Sem r a
    undefId i = do
      throw @Text $ "foldMT: undefined obj id found: " <> i ^. isoText

{-# INLINE foldMT #-}


-- same as foldMT', but with defaut error handling

foldMTU :: (EffIStore r, Monoid a)
        => (           ObjId -> ImgParts     -> MetaData   -> Sem r a)  -- ^ IMG
        -> (Act r a -> ObjId -> DirEntries   -> TimeStamp  -> Sem r a)  -- ^ DIR
        -> (Act r a -> ObjId -> ObjId        -> ObjId      -> Sem r a)  -- ^ ROOT
        -> (Act r a -> ObjId -> MetaData     -> Maybe ImgRef
                             -> Maybe ImgRef -> ColEntries -> Sem r a)  -- ^ COL
        -> Act r a
foldMTU = foldMT' ignoreUndefId

{-# INLINE foldMTU #-}

-- ----------------------------------------
--
-- recurse into DIR and IMG entries
-- used when syncing with file system

foldImgDirs :: (EffIStore r, EffError r, Monoid a)
            => (           ObjId -> ImgParts   -> MetaData  -> Sem r a)  -- ^ IMG
            -> (Act r a -> ObjId -> DirEntries -> TimeStamp -> Sem r a)  -- ^ DIR
            -> Act r a
foldImgDirs imgA dirA =
  foldMT imgA dirA foldRootDir ignoreCol

{-# INLINE foldImgDirs #-}

-- ----------------------------------------
--
-- traverse DIR hierachy and process all IMG entries

foldImages :: (EffIStore r, EffError r, Monoid a)
           => (ObjId -> ImgParts -> MetaData -> Sem r a)
           -> Act r a
foldImages imgA =
  foldImgDirs imgA foldDir

{-# INLINE foldImages #-}

-- ----------------------------------------

-- recurse into COL hierachy and process collections

foldCollections :: (EffIStore r, EffError r, Monoid a)
  => (Act r a -> ObjId -> MetaData     -> Maybe ImgRef
                       -> Maybe ImgRef -> ColEntries -> Sem r a) -- ^ COL
  -> Act r a

foldCollections = foldMT ignoreImg ignoreDir foldRootCol

{-# INLINE foldCollections #-}

-- ----------------------------------------
--
-- compute all ObjIds reachable from a starting ObjId, e.g. the root

allObjIds :: EffIStore r => ObjId -> Sem r ObjIds
allObjIds =
  foldMTU imgA dirA rootA colA
  where
    imgA i _pts _md =
      return $ singleObjId i

    dirA go i es ts = do
      s1 <- foldDir go i es ts
      return $
        singleObjId i <> s1

    colA go i md im be es = do
      s1 <- foldCol go i md im be es
      return $
        singleObjId i <> s1

    rootA go i dir col = do
      s1 <- foldRoot go i dir col
      return $
        singleObjId i <> s1

{-# INLINE allObjIds #-}

-- ----------------------------------------
--
-- compute all image ObjIds reachable from an ObjId, e.g. a collection

allImgObjIds :: EffIStore r => ObjId -> Sem r ObjIds
allImgObjIds =
  foldMTU imgA foldDir foldRoot foldCol
  where
    imgA i _pts _md =
      return $ singleObjId i

{-# INLINE allImgObjIds #-}

-- ----------------------------------------
--
-- compute all subcollection ObjIds reachable from an ObjId, e.g. a collection

allColObjIds :: EffIStore r => ObjId -> Sem r ObjIds
allColObjIds =
  foldMTU ignoreImg ignoreDir foldRootCol colA
  where
    colA go i md im be es = do
      s1 <- foldColEntries go i md im be es
      return $
        singleObjId i <> s1

{-# INLINE allColObjIds #-}

-- ----------------------------------------
--
-- compute all dead ObjIds
-- for consistency check of ImgTree

allUndefObjIds :: EffIStore r => ObjId -> Sem r ObjIds
allUndefObjIds =
  foldMT' undefId ignoreImg foldDir foldRoot foldCol
  where
    undefId i =
      return $ singleObjId i

{-# INLINE allUndefObjIds #-}

-- ----------------------------------------

ignoreUndefId :: Monoid a => i -> Sem r a
ignoreUndefId _i = return mempty
{-# INLINE ignoreUndefId #-}

ignoreImg :: Monoid a =>  p1 -> p2 -> p3 -> Sem r a
ignoreImg _i _pts _md = return mempty
{-# INLINE ignoreImg #-}

-- --------------------
--
-- root folds

ignoreRoot :: Monoid a => p0 -> p1 -> p2 -> p3 -> Sem r a
ignoreRoot _go _i _dir _col = return mempty
{-# INLINE ignoreRoot #-}

foldRoot :: Monoid a => (ObjId -> Sem r a) -> p1 -> ObjId -> ObjId -> Sem r a
foldRoot go _i dir col = (<>) <$> go dir <*> go col
{-# INLINE foldRoot #-}

foldRootDir :: (ObjId -> Sem r a) -> p1 -> ObjId -> p3 -> Sem r a
foldRootDir go _i dir _col = go dir
{-# INLINE foldRootDir #-}

foldRootCol :: (ObjId -> Sem r a) -> p1 -> p2 -> ObjId -> Sem r a
foldRootCol go _i _dir = go
{-# INLINE foldRootCol #-}

-- --------------------
--
-- dir folds

ignoreDir :: Monoid a =>  p0 -> p1 -> p2 -> p3 -> Sem r a
ignoreDir _go _i _es _ts = return mempty
{-# INLINE ignoreDir #-}

foldDir :: Monoid a => (ObjId -> Sem r a) -> p1 -> DirEntries -> p3 -> Sem r a
foldDir go _i es _ts = fold <$> traverse go es
{-# INLINE foldDir #-}

-- --------------------
--
-- col folds

ignoreCol :: Monoid a =>  p0 -> p1 -> p2 -> p3 -> p4 -> p5 -> Sem r a
ignoreCol _go _i _md _im _be _es = return mempty
{-# INLINE ignoreCol #-}


foldCol :: Monoid a
        => (ObjId -> Sem r a) -> p1 -> p2
        -> Maybe ImgRef -> Maybe ImgRef -> ColEntries
        -> Sem r a
foldCol go _i _md im be es = do
  s1 <- fold <$> traverse (go . _iref) im
  s2 <- fold <$> traverse (go . _iref) be
  s3 <- fold <$> traverse (go . (^. theColObjId)) es
  return $
    s1 <> s2 <> s3
{-# INLINE foldCol #-}

-- traverse all collection entries
foldColEntries :: Monoid a
         => (ObjId -> Sem r a) -> p1 -> p2 -> p3 -> p4 -> ColEntries
         -> Sem r a
foldColEntries go _i _md _im _be es =
  fold <$> traverse (go . (^. theColObjId)) es
{-# INLINE foldColEntries #-}

-- traverse only the subcollections
foldColColEntries :: Monoid a
                  => (ObjId -> Sem r a) -> p1 -> p2 -> p3 -> p4 -> ColEntries
                  -> Sem r a
foldColColEntries go _i _md _im _be es =
  fold <$> traverse go (es ^.. traverse . theColColRef)
{-# INLINE foldColColEntries #-}

-- ----------------------------------------
