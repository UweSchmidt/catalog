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

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- {-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------

module Catalog.ImgStore
where

-- import Control.Monad.Trans.Except (Except, runExcept)

import Catalog.Effects

import Data.ImageStore
import Data.ImgTree
import Catalog.Journal
import Data.MetaData
import Data.Prim

import qualified Data.Set      as S
import qualified Data.Sequence as Seq
import qualified Data.Text     as T
import qualified Data.Text.IO  as T

------------------------------------------------------------------------------
--
-- basic image tree actions

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
-- smart constructors

-- make an image catalog node

mkCatEntry :: (ObjId -> Name -> Sem r ())   -- ^ journal action
           -> (ImgNode -> Bool)             -- ^ parent editable
           -> ImgNode                       -- ^ the catalog node value
           -> ObjId                         -- ^ parent node
           -> Name                          -- ^ the name of the node
           -> SemISEJ r ObjId               -- ^ the new ref
mkCatEntry journal' isN v i n =
  dt >>= go
  where
    go t = do
      (d, t') <- liftExcept $ mkNode isN n i v t
      -- theImgTree .= t'
      modify' (\ s -> s & theImgTree .~ t')
      journal' i n
      return d

mkJEntry :: (ObjId -> Name -> Journal) -> ObjId -> Name -> SemISJ r ()
mkJEntry km i n = journal $ km i n
{-# INLINE mkJEntry #-}

-- create a new empty DIR node
mkImgDir :: ObjId -> Name -> SemISEJ r ObjId
mkImgDir = mkCatEntry (mkJEntry MkDIR) isDIR emptyImgDir
{-# INLINE mkImgDir #-}

-- create a new empty COL node
mkImgCol :: ObjId -> Name -> SemISEJ r ObjId
mkImgCol = mkCatEntry (mkJEntry MkCOL) isCOL emptyImgCol
{-# INLINE mkImgCol #-}

-- create a new empty IMG node
mkImg :: ObjId -> Name -> SemISEJ r ObjId
mkImg = mkCatEntry (mkJEntry MkIMG) isDIR emptyImg
{-# INLINE mkImg #-}


-- remove an entry from catalog tree
rmImgNode :: ObjId -> SemISEJL r ()
rmImgNode i = do
  ex <- existsEntry i
  if ex
    then dt >>= go
    else log'warn $ "rmImgNode: ObjId doesn't exist: " <> i ^. isoText
  where
    go t = do
      -- journal output must be done first, before calling removeImgNode
      -- journalChange $ RmObj i
      journal $ RmObj i
      t' <- liftExcept $ removeImgNode i t
      modify' (\ s -> s & theImgTree .~ t')
      return ()

-- ----------------------------------------
--
-- simple file system like ops

-- create a new empty subcollection and append it to the colrefs

mkCollection :: Path -> SemISEJL r ObjId
mkCollection = mkCollection' $ flip (Seq.|>)

-- create a new empty subcollection and cons it to the colrefs
mkCollectionC :: Path -> SemISEJL r ObjId
mkCollectionC = mkCollection' (Seq.<|)

mkCollection' :: (ColEntry -> ColEntries -> ColEntries)
              -> Path
              -> SemISEJL r ObjId
mkCollection' merge target'path = do
  -- parent exists
  (parent'id, parent'node) <-
    getIdNode "mkCollection: parent doesn't exist" parent'path

  -- parent is a collection
  -- TODO exists check
  unless (isCOL parent'node) $
    throw $ msgPath parent'path "mkCollection: parent isn't a collection"

  -- check collection does not yet exist
  alreadyTherePath "mkCollection: target collection already exists" target'path

  -- create a new empty collection and append it to the parent collection
  col'id <- mkImgCol parent'id target'name
  adjustColEntries (merge $ mkColColRef col'id) parent'id
  return col'id
  where
    (parent'path, target'name) = target'path ^. viewBase

-- ----------------------------------------

adjustNodeVal :: Show a
              => (ObjId -> a -> Journal)
              -> Traversal' ImgNode a
              -> (a -> a)
              -> ObjId
              -> SemISEJL r ()
adjustNodeVal mkj theComp f i = do
  -- modify the image node
  modify' $ \ s ->
    s & theImgTree . entryAt i . traverse . nodeVal . theComp %~ f

  -- journal the changed result
  dt >>= journalAdjust
    where
      journalAdjust t =
        case t ^.. entryAt i . traverse . nodeVal . theComp of
          -- the expected case
          [new'v] ->
            journal $ mkj i new'v

          -- the error cases
          [] ->
            log'warn $ "adjustNodeVal: nothing changed, "
                       <> "component to be modified does not exist in "
                       <> i ^. isoText

          -- the critical case
          vs ->
            log'warn $ "adjustNodeVal: mulitple components have been changed in "
                       <> i ^. isoText
                       <> ": "
                       <> show vs ^. isoText

adjustImg :: (ImgParts -> ImgParts) -> ObjId -> SemISEJL r ()
adjustImg = adjustNodeVal AdjImgParts theParts
{-# INLINE adjustImg #-}

adjustDirEntries :: (DirEntries -> DirEntries) -> ObjId -> SemISEJL r ()
adjustDirEntries = adjustNodeVal AdjDirEntries theDirEntries
{-# INLINE adjustDirEntries #-}

adjustMetaData :: (MetaData -> MetaData) -> ObjId -> SemISEJL r ()
adjustMetaData = adjustNodeVal AdjMetaData theMetaData
{-# INLINE adjustMetaData #-}

adjustColImg :: (Maybe ImgRef -> Maybe ImgRef) -> ObjId -> SemISEJL r ()
adjustColImg = adjustNodeVal AdjColImg theColImg
{-# INLINE adjustColImg #-}

adjustColBlog :: (Maybe ImgRef -> Maybe ImgRef) -> ObjId -> SemISEJL r ()
adjustColBlog = adjustNodeVal AdjColBlog theColBlog
{-# INLINE adjustColBlog #-}

adjustColEntries :: (ColEntries -> ColEntries) -> ObjId -> SemISEJL r ()
adjustColEntries = adjustNodeVal AdjColEntries theColEntries
{-# INLINE adjustColEntries #-}

adjustColEntry :: (ColEntry -> ColEntry) -> Int -> ObjId -> SemISEJL r ()
adjustColEntry f i = adjustColEntries f'
  where
    f' :: ColEntries -> ColEntries
    f' = Seq.adjust f i
{-# INLINE adjustColEntry #-}

remColEntry :: Int -> ObjId -> SemISEJL r ()
remColEntry pos = adjustColEntries (Seq.deleteAt pos)
{-# INLINE remColEntry #-}

setSyncTime :: TimeStamp -> ObjId -> SemISEJL r ()
setSyncTime t i = do
  adjustNodeVal SetSyncTime theSyncTime (const t) i
{-# INLINE setSyncTime #-}

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
--
-- path to/from file path operations


buildImgPath0 :: ImgRef -> SemIS r TextPath
buildImgPath0 (ImgRef i n) = do
  p <- objid2path i
  return $ substPathName n p ^. isoText

buildImgPath :: ImgRef -> SemIS r TextPath
buildImgPath ir = addExt ".jpg" <$> buildImgPath0 ir

addExt :: Text -> TextPath -> TextPath
addExt ext fn
  | ext `T.isSuffixOf` fn = fn
  | otherwise             = fn <> ext


-- ----------------------------------------
--
-- journaling

journal :: Journal -> SemISJ r ()
journal jc = do
  jcp <- traverse objid2path jc
  consume @JournalP jcp

journalToStdout :: Member (Embed IO) r
                => InterpreterFor (Consume JournalP) r
journalToStdout = consumeIO $ T.putStrLn . ("journal: " <>) . (^. isoText) . show

journalToDevNull :: InterpreterFor (Consume JournalP) r
journalToDevNull = consumeNull

-- ----------------------------------------
