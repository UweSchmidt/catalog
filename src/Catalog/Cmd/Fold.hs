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

-- ----------------------------------------
--
-- recurse into DIR and IMG entries
-- used when syncing with file system

foldImgDirs :: Monoid r
            => (         ObjId -> ImgParts   -> MetaData  -> Cmd r)  -- ^ IMG
            -> (Act r -> ObjId -> DirEntries -> TimeStamp -> Cmd r)  -- ^ DIR
            -> Act r
foldImgDirs imgA dirA =
  foldMT imgA dirA rootA colA
  where
    rootA go _i dir _col        = go dir
    colA  _  _i _md _im _be _es = return mempty

-- ----------------------------------------
--
-- traverse DIR hierachy and process all IMG entries

foldImages :: Monoid r
           => (ObjId -> ImgParts -> MetaData -> Cmd r)
           -> Act r
foldImages imgA =
  foldImgDirs imgA dirA
  where
    dirA  go _i es _ts = mconcat <$> traverse go (es ^. isoDirEntries)

-- ----------------------------------------

-- recurse into COL hierachy and process collections

foldCollections :: Monoid r
                => (Act r -> ObjId -> MetaData     -> Maybe ImgRef
                                   -> Maybe ImgRef -> [ColEntry] -> Cmd r) -- ^ COL
                -> Act r

foldCollections colA =
  foldMT imgA dirA rootA colA
  where
    rootA go _i _dir col  = go col
    dirA  _  _  _es _ts   = return mempty
    imgA  _  _pts   _md   = return mempty

-- ----------------------------------------
--
-- compute all ObjIds reachable from a starting ObjId, e.g. the root

allObjIds :: ObjId -> Cmd ObjIds
allObjIds =
  foldMT' undefId imgA dirA rootA colA
  where
    undefId _i =
      return mempty

    imgA i _pts _md =
      return $ singleObjId i

    dirA go i es _ts = do
      s1 <- fold <$> traverse go (es ^. isoDirEntries)
      return (s0 <> s1)
        where
          s0 = singleObjId i

    colA go i _md im be es = do
      s1 <- fold <$> traverse (go . _iref) im
      s2 <- fold <$> traverse (go . _iref) be
      s3 <- fold <$> traverse (go . (^. theColObjId)) es
      return (mconcat [s0, s1, s2, s3])
        where
          s0 = singleObjId i

    rootA go i dir col = do
      s1 <- go dir
      s2 <- go col
      return (mconcat [s0, s1, s2])
        where
          s0 = singleObjId i

-- ----------------------------------------
--
-- compute all image ObjIds reachable from an ObjId, e.g. a collection

allImgObjIds :: ObjId -> Cmd ObjIds
allImgObjIds =
  foldMT' (const $ return mempty) imgA dirA rootA colA
  where
    imgA i _pts _md =
      return $ singleObjId i

    dirA go _i es _ts =
      mconcat <$> traverse go (es ^. isoDirEntries)

    rootA go _i dir col =
      (<>) <$> go dir <*> go col

    colA go _i _md im be es = do
      s1 <- fold <$> traverse (go . _iref) im
      s2 <- fold <$> traverse (go . _iref) be
      s3 <- fold <$> traverse (go . (^. theColObjId)) es
      return (mconcat [s1, s2, s3])


-- ----------------------------------------
--
-- compute all subcollection ObjIds reachable from an ObjId, e.g. a collection

allColObjIds :: ObjId -> Cmd ObjIds
allColObjIds =
  foldMT' (const $ return mempty) imgA dirA rootA colA
  where
    imgA _i _pts _md =
      return mempty

    dirA _go _i _es _ts =
      return mempty

    rootA go _i _dir col =
      go col

    colA go i _md _im _be es = do
      s1 <- fold <$> traverse (go . (^. theColObjId)) es
      return $ singleObjId i <> s1


-- ----------------------------------------
--
-- compute all dead ObjIds
-- for consistency check of ImgTree

allUndefObjIds :: ObjId -> Cmd ObjIds
allUndefObjIds =
  foldMT' undefId imgA dirA rootA colA
  where
    undefId i =
      return $ singleObjId i

    imgA _i _pts _md =
      return mempty

    dirA go _i es _ts =
      mconcat <$> traverse go (es ^. isoDirEntries)

    colA go _i _md im be es = do
      s1 <- fold <$> traverse (go . _iref) im
      s2 <- fold <$> traverse (go . _iref) be
      s3 <- fold <$> traverse (go . (^. theColObjId)) es
      return (mconcat [s1, s2, s3])

    rootA go _i dir col = do
      (<>) <$> go dir <*> go col

-- ----------------------------------------
