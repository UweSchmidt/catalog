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


------------------------------------------------------------------------------

module Catalog.ImgTree.Modify
  ( mkImgDir
  , mkImgCol
  , mkImg
  , rmImgNode
  , mkCollection
  , mkCollectionC
  , adjustImg
  , adjustDirEntries
  , adjustMetaData
  , adjustPartMetaData
  , adjustColImg
  , adjustColBlog
  , adjustColEntries
  , adjustColEntry
  , remColEntry
  , setSyncTime
  )
where

-- import Control.Monad.Trans.Except (Except, runExcept)

import Catalog.Effects
import Catalog.ImgTree.Access
import Catalog.Journal

import Data.ImageStore
import Data.ImgTree
import Data.Journal
import Data.MetaData
import Data.Prim

import qualified Data.Sequence as Seq

------------------------------------------------------------------------------
--
-- smart constructors
--
-- create a catalog node

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
            log'warn $ "adjustNodeVal: multiple components have been changed in "
                       <> i ^. isoText
                       <> ": "
                       <> toText vs

adjustImg :: (ImgParts -> ImgParts) -> ObjId -> SemISEJL r ()
adjustImg = adjustNodeVal AdjImgParts theParts
{-# INLINE adjustImg #-}

adjustDirEntries :: (DirEntries -> DirEntries) -> ObjId -> SemISEJL r ()
adjustDirEntries = adjustNodeVal AdjDirEntries theDirEntries
{-# INLINE adjustDirEntries #-}

adjustMetaData :: (MetaData -> MetaData) -> ObjId -> SemISEJL r ()
adjustMetaData = adjustNodeVal AdjMetaData theMetaData
{-# INLINE adjustMetaData #-}

adjustPartMetaData :: (MetaData -> MetaData) -> ImgRef -> SemISEJL r ()
adjustPartMetaData mf (ImgRef i nm) =
  adjustNodeVal (AdjPartMetaData nm) (theImgPart nm . theImgMeta) mf i

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
