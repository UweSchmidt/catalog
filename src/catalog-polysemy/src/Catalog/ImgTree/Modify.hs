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
       ( Sem
       , Eff'ISJ
       , Eff'ISEJ
       , Eff'ISEJL
       , liftExcept
       , log'warn
       , modify'
       , throw
       )
import Catalog.ImgTree.Access
       ( dt
       , existsEntry
       , getIdNode
       , alreadyTherePath
       )
import Catalog.Journal
       ( journal )

import Data.ImageStore
       ( theImgTree )

import Data.ImgTree
       ( ColEntries
       , ColEntryM
       , DirEntries
       , ImgNode
       , ImgParts
       , ImgRef
       , ImgRef'(ImgRef)
       , emptyImg
       , emptyImgCol
       , emptyImgDir
       , entryAt
       , isCOL
       , isDIR
       , mkColColRefM
       , mkNode
       , nodeVal
       , removeImgNode
       , theColBlog
       , theColEntries
       , theColImg
       , theDirEntries
       , theImgMeta
       , theImgPart
       , theMetaData
       , theParts
       , theSyncTime
       )
import Data.Journal
       ( Journal'(..)
       , Journal
       )
import Data.MetaData
       ( MetaData )

import Data.Prim
       ( IsoText(isoText)
       , Name
       , ObjId
       , Path
       , TimeStamp
       , Traversal'
       , (%~)
       , (&)
       , (.~)
       , (^.)
       , (^..)
       , msgPath
       , toText
       , unless
       , viewBase
       )

import qualified Data.Sequence as Seq

------------------------------------------------------------------------------
--
-- smart constructors
--
-- create a catalog node

mkCatEntry :: Eff'ISEJ r
           => (ObjId -> Name -> Sem r ())   -- ^ journal action
           -> (ImgNode -> Bool)             -- ^ parent editable
           -> ImgNode                       -- ^ the catalog node value
           -> ObjId                         -- ^ parent node
           -> Name                          -- ^ the name of the node
           -> Sem r ObjId                   -- ^ the new ref
mkCatEntry journal' isN v i n =
  dt >>= go
  where
    go t = do
      (d, t') <- liftExcept $ mkNode isN n i v t
      -- theImgTree .= t'
      modify' (\ s -> s & theImgTree .~ t')
      journal' i n
      return d

mkJEntry :: Eff'ISJ r
         => (ObjId -> Name -> Journal)
         ->  ObjId -> Name -> Sem r ()
mkJEntry km i n = journal $ km i n
{-# INLINE mkJEntry #-}

-- create a new empty DIR node
mkImgDir :: Eff'ISEJ r => ObjId -> Name -> Sem r ObjId
mkImgDir = mkCatEntry (mkJEntry MkDIR) isDIR emptyImgDir
{-# INLINE mkImgDir #-}

-- create a new empty COL node
mkImgCol :: Eff'ISEJ r => ObjId -> Name -> Sem r ObjId
mkImgCol = mkCatEntry (mkJEntry MkCOL) isCOL emptyImgCol
{-# INLINE mkImgCol #-}

-- create a new empty IMG node
mkImg :: Eff'ISEJ r => ObjId -> Name -> Sem r ObjId
mkImg = mkCatEntry (mkJEntry MkIMG) isDIR emptyImg
{-# INLINE mkImg #-}


-- remove an entry from catalog tree

rmImgNode :: Eff'ISEJL r => ObjId -> Sem r ()
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

mkCollection :: Eff'ISEJL r => Path -> Sem r ObjId
mkCollection = mkCollection' $ flip (Seq.|>)

-- create a new empty subcollection and cons it to the colrefs
mkCollectionC :: Eff'ISEJL r => Path -> Sem r ObjId
mkCollectionC = mkCollection' (Seq.<|)

mkCollection' :: Eff'ISEJL r
              => (ColEntryM -> ColEntries -> ColEntries)
              -> Path
              -> Sem r ObjId
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
  adjustColEntries (merge $ mkColColRefM col'id) parent'id
  return col'id
  where
    (parent'path, target'name) = target'path ^. viewBase

-- ----------------------------------------

adjustNodeVal :: (Eff'ISEJL r, Show a)
              => (ObjId -> a -> Journal)
              -> Traversal' ImgNode a
              -> (a -> a)
              -> ObjId
              -> Sem r ()
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

adjustImg :: Eff'ISEJL r
          => (ImgParts -> ImgParts) -> ObjId -> Sem r ()
adjustImg = adjustNodeVal AdjImgParts theParts
{-# INLINE adjustImg #-}

adjustDirEntries :: Eff'ISEJL r
                 => (DirEntries -> DirEntries) -> ObjId -> Sem r ()
adjustDirEntries = adjustNodeVal AdjDirEntries theDirEntries
{-# INLINE adjustDirEntries #-}

adjustMetaData :: Eff'ISEJL r
               => (MetaData -> MetaData) -> ObjId -> Sem r ()
adjustMetaData = adjustNodeVal AdjMetaData theMetaData
{-# INLINE adjustMetaData #-}

adjustPartMetaData :: Eff'ISEJL r
                   => (MetaData -> MetaData) -> ImgRef -> Sem r ()
adjustPartMetaData mf (ImgRef i nm) =
  adjustNodeVal (AdjPartMetaData nm) (theImgPart nm . theImgMeta) mf i

adjustColImg :: Eff'ISEJL r
             => (Maybe ImgRef -> Maybe ImgRef) -> ObjId -> Sem r ()
adjustColImg = adjustNodeVal AdjColImg theColImg
{-# INLINE adjustColImg #-}

adjustColBlog :: Eff'ISEJL r
              => (Maybe ImgRef -> Maybe ImgRef) -> ObjId -> Sem r ()
adjustColBlog = adjustNodeVal AdjColBlog theColBlog
{-# INLINE adjustColBlog #-}

adjustColEntries :: Eff'ISEJL r
                 => (ColEntries -> ColEntries) -> ObjId -> Sem r ()
adjustColEntries = adjustNodeVal AdjColEntries theColEntries
{-# INLINE adjustColEntries #-}

adjustColEntry :: Eff'ISEJL r
               => (ColEntryM -> ColEntryM) -> Int -> ObjId -> Sem r ()
adjustColEntry f i = adjustColEntries f'
  where
    f' :: ColEntries -> ColEntries
    f' = Seq.adjust f i
{-# INLINE adjustColEntry #-}

remColEntry :: Eff'ISEJL r => Int -> ObjId -> Sem r ()
remColEntry pos = adjustColEntries (Seq.deleteAt pos)
{-# INLINE remColEntry #-}

setSyncTime :: Eff'ISEJL r => TimeStamp -> ObjId -> Sem r ()
setSyncTime t i = do
  adjustNodeVal SetSyncTime theSyncTime (const t) i
{-# INLINE setSyncTime #-}

-- ----------------------------------------
