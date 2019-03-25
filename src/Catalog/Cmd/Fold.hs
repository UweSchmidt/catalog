module Catalog.Cmd.Fold
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Types
import           Data.ImgTree
import           Data.MetaData
import           Data.Prim

-- ----------------------------------------

type Act r = ObjId -> Cmd r

-- traverse an catalog object (@ObjId@) with all it's @ObjId@'s to
-- sub objects
-- for all object variants (IMG, DIR, COL and ROOT) a processing
-- funtion is given,
-- for the "inner" objects ROOt, DIR and COL the recursive call
-- of the fold is added as an extra param
-- so the processing functions can decide, when to make the recursive
-- call
-- illegal ObjIds (in an inconsistent catalog) are handled by @undefId@

foldMT' :: (         ObjId                               -> Cmd r)  -- ^ undef id
        -> (         ObjId -> ImgParts     -> MetaData   -> Cmd r)  -- ^ IMG
        -> (Act r -> ObjId -> DirEntries   -> TimeStamp  -> Cmd r)  -- ^ DIR
        -> (Act r -> ObjId -> ObjId        -> ObjId      -> Cmd r)  -- ^ ROOT
        -> (Act r -> ObjId -> MetaData     -> Maybe ImgRef
                           -> Maybe ImgRef -> [ColEntry] -> Cmd r)  -- ^ COL
        -> Act r
foldMT' undefId imgA dirA' rootA' colA' i0 = do
  go i0
  where
    dirA  = dirA'  go
    rootA = rootA' go
    colA  = colA'  go
    go i  = do
      -- trcObj i $ "foldMT"
      mn <- getTree (entryAt i)
      -- trc $ "foldMT: " ++ show mn
      case mn of
        Nothing ->
          undefId i
        Just n ->
          case n ^. nodeVal of
            IMG  pts md       -> imgA  i pts md
            DIR  es  ts       -> dirA  i es  ts
            ROOT dir col      -> rootA i dir col
            COL  md  im be es -> colA  i md  im be es


-- same as foldMT', but with defaut error handling

foldMT :: (         ObjId -> ImgParts     -> MetaData   -> Cmd r)  -- ^ IMG
       -> (Act r -> ObjId -> DirEntries   -> TimeStamp  -> Cmd r)  -- ^ DIR
       -> (Act r -> ObjId -> ObjId        -> ObjId      -> Cmd r)  -- ^ ROOT
       -> (Act r -> ObjId -> MetaData     -> Maybe ImgRef
                          -> Maybe ImgRef -> [ColEntry] -> Cmd r)  -- ^ COL
       -> Act r
foldMT = foldMT' undefId
  where
    undefId i = do
      warn  $ "foldMT: undefined obj id found: " ++ show i
      abort $ "foldMT: undefined obj id found: " ++ show i

-- same as foldMT', but with defaut error handling

foldMTU :: Monoid r
        => (         ObjId -> ImgParts     -> MetaData   -> Cmd r)  -- ^ IMG
        -> (Act r -> ObjId -> DirEntries   -> TimeStamp  -> Cmd r)  -- ^ DIR
        -> (Act r -> ObjId -> ObjId        -> ObjId      -> Cmd r)  -- ^ ROOT
        -> (Act r -> ObjId -> MetaData     -> Maybe ImgRef
                           -> Maybe ImgRef -> [ColEntry] -> Cmd r)  -- ^ COL
        -> Act r
foldMTU = foldMT' ignoreUndefId

-- ----------------------------------------
--
-- recurse into DIR and IMG entries
-- used when syncing with file system

foldImgDirs :: Monoid r
            => (         ObjId -> ImgParts   -> MetaData  -> Cmd r)  -- ^ IMG
            -> (Act r -> ObjId -> DirEntries -> TimeStamp -> Cmd r)  -- ^ DIR
            -> Act r
foldImgDirs imgA dirA =
  foldMT imgA dirA foldRootDir ignoreCol

-- ----------------------------------------
--
-- traverse DIR hierachy and process all IMG entries

foldImages :: Monoid r
           => (ObjId -> ImgParts -> MetaData -> Cmd r)
           -> Act r
foldImages imgA =
  foldImgDirs imgA foldDir

-- ----------------------------------------

-- recurse into COL hierachy and process collections

foldCollections :: Monoid r
                => (Act r -> ObjId -> MetaData     -> Maybe ImgRef
                                   -> Maybe ImgRef -> [ColEntry] -> Cmd r) -- ^ COL
                -> Act r

foldCollections colA =
  foldMT ignoreImg ignoreDir foldRootCol colA

-- ----------------------------------------
--
-- compute all ObjIds reachable from a starting ObjId, e.g. the root

allObjIds :: ObjId -> Cmd ObjIds
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

-- ----------------------------------------
--
-- compute all image ObjIds reachable from an ObjId, e.g. a collection

allImgObjIds :: ObjId -> Cmd ObjIds
allImgObjIds =
  foldMTU imgA foldDir foldRoot foldCol
  where
    imgA i _pts _md =
      return $ singleObjId i

-- ----------------------------------------
--
-- compute all subcollection ObjIds reachable from an ObjId, e.g. a collection

allColObjIds :: ObjId -> Cmd ObjIds
allColObjIds =
  foldMTU ignoreImg ignoreDir foldRootCol colA
  where
    colA go i md im be es = do
      s1 <- foldColEntries go i md im be es
      return $
        singleObjId i <> s1

-- ----------------------------------------
--
-- compute all dead ObjIds
-- for consistency check of ImgTree

allUndefObjIds :: ObjId -> Cmd ObjIds
allUndefObjIds =
  foldMT' undefId ignoreImg foldDir foldRoot foldCol
  where
    undefId i =
      return $ singleObjId i

-- ----------------------------------------

ignoreUndefId :: Monoid r => i -> Cmd r
ignoreUndefId _i = return mempty
{-# INLINE ignoreUndefId #-}

ignoreImg :: Monoid r =>  p1 -> p2 -> p3 -> Cmd r
ignoreImg _i _pts _md = return mempty
{-# INLINE ignoreImg #-}

-- --------------------
--
-- root folds

ignoreRoot :: Monoid r => p0 -> p1 -> p2 -> p3 -> Cmd r
ignoreRoot _go _i _dir _col = return mempty

foldRoot :: Monoid r => (ObjId -> Cmd r) -> p1 -> ObjId -> ObjId -> Cmd r
foldRoot go _i dir col = (<>) <$> go dir <*> go col
{-# INLINE foldRoot #-}

foldRootDir :: (ObjId -> Cmd r) -> p1 -> ObjId -> p3 -> Cmd r
foldRootDir go _i dir _col = go dir
{-# INLINE foldRootDir #-}

foldRootCol :: (ObjId -> Cmd r) -> p1 -> p2 -> ObjId -> Cmd r
foldRootCol go _i _dir col = go col
{-# INLINE foldRootCol #-}

-- --------------------
--
-- dir folds

ignoreDir :: Monoid r =>  p0 -> p1 -> p2 -> p3 -> Cmd r
ignoreDir _go _i _es _ts = return mempty
{-# INLINE ignoreDir #-}

foldDir :: Monoid r => (ObjId -> Cmd r) -> p1 -> DirEntries -> p3 -> Cmd r
foldDir go _i es _ts = mconcat <$> traverse go (es ^. isoDirEntries)
{-# INLINE foldDir #-}

-- --------------------
--
-- col folds

ignoreCol :: Monoid r =>  p0 -> p1 -> p2 -> p3 -> p4 -> p5 -> Cmd r
ignoreCol _go _i _md _im _be _es = return mempty
{-# INLINE ignoreCol #-}

foldCol :: Monoid r
        => (ObjId -> Cmd r) -> p1 -> p2
        -> Maybe ImgRef -> Maybe ImgRef -> [ColEntry]
        -> Cmd r
foldCol go _i _md im be es = do
  s1 <- fold <$> traverse (go . _iref) im
  s2 <- fold <$> traverse (go . _iref) be
  s3 <- fold <$> traverse (go . (^. theColObjId)) es
  return $
    s1 <> s2 <> s3
{-# INLINE foldCol #-}

-- traverse all collection entries
foldColEntries :: Monoid r
         => (ObjId -> Cmd r) -> p1 -> p2 -> p3 -> p4 -> [ColEntry]
         -> Cmd r
foldColEntries go _i _md _im _be es =
  fold <$> traverse (go . (^. theColObjId)) es
{-# INLINE foldColEntries #-}

-- traverse only the subcollections
foldColColEntries :: Monoid r
                  => (ObjId -> Cmd r) -> p1 -> p2 -> p3 -> p4 -> [ColEntry]
                  -> Cmd r
foldColColEntries go _i _md _im _be es =
  fold <$> traverse go (es ^.. traverse . theColColRef)
{-# INLINE foldColColEntries #-}

-- ----------------------------------------
