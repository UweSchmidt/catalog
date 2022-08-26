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
       , EffIStore
       , liftExcept
       , log'warn
       , modify'
       , throw
       )
import Catalog.ImgTree.Access

import Catalog.Journal
       ( journal )

import Data.ImageStore
       ( theImgTree )

import Data.ImgTree

import Data.Journal
       ( Journal'(..)
       , Journal
       )
import Data.MetaData
       ( MetaData
       , lookupCreate
       , parseDate
       , parseTime
       , isoDateInt
       )

import Data.Prim

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
mkCatEntry journal' isN v i n = withTF go
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
    then withTF go
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
              => (ColEntry -> ColEntries -> ColEntries)
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
  adjustColEntries (merge $ mkColColRef col'id) parent'id
  return col'id
  where
    (parent'path, target'name) = target'path ^. viewBase

-- ----------------------------------------
--
-- the only state modifying function

modifyNT' :: EffIStore r
          => Traversal' ImgTree a -> (ImgTree -> a -> a) -> Sem r ()
modifyNT' tr f =
  modify'(\ s ->
            s & theImgTree %~ (\ t ->
                                 t & tr %~ f t
                              )
         )
{-# INLINE modifyNT' #-}

-- state modification and journal output

adjustNodeVal' :: (Eff'ISEJL r, Show a)
              => (ObjId -> a -> Journal)
              -> Traversal' ImgNode a
              -> (ImgTree -> a -> a)
              -> ObjId
              -> Sem r ()
adjustNodeVal' mkj theComp f i = do
  -- modify the image node
  modifyNT' (entryAt i . nodeVal . theComp) f

  -- journal the changed result
  withTF $ traverseOf_
           (entryAt i . nodeVal . theComp)
           (journal . mkj i)

adjustColEntries' :: Eff'ISEJL r
                 => (ImgTree -> ColEntries -> ColEntries) -> ObjId -> Sem r ()
adjustColEntries' = adjustNodeVal' AdjColEntries theColEntries
{-# INLINE adjustColEntries' #-}

-- --------------------

adjustColBy' :: Eff'ISEJL r
             => (ImgTree -> ColEntries -> ColEntries)
             -> ColEntries
             -> ObjId
             -> Sem r ()
adjustColBy' sortCol cs parent'i =
  adjustColEntries' (sortMerge' cs sortCol) parent'i

adjustColByName' :: Eff'ISEJL r => ColEntries -> ObjId -> Sem r ()
adjustColByName' = adjustColBy' sortByName'

adjustColByDate' :: Eff'ISEJL r => ColEntries -> ObjId -> Sem r ()
adjustColByDate' = adjustColBy' sortByDate'

insertColByName :: Eff'ISEJL r => ObjId -> ObjId -> Sem r ()
insertColByName cref = adjustColByName' (Seq.singleton (mkColColRef cref))

insertColByAppend' :: Eff'ISEJL r => ObjId -> ObjId -> Sem r ()
insertColByAppend' i = adjustColEntries' $
                       const (<> Seq.singleton (mkColColRef i))

insertColByCons' :: Eff'ISEJL r => ObjId -> ObjId -> Sem r ()
insertColByCons' i = adjustColEntries' $
                       const (Seq.singleton (mkColColRef i) <>)

-- --------------------
--
-- pure functions

sortColEntries' :: forall a .
                   (ImgTree -> ColEntry -> a)
                -> (a -> a -> Ordering)
                -> ImgTree -> ColEntries -> ColEntries
sortColEntries' getVal cmpVal t = go
  where
    go = fmap fst . Seq.sortBy (cmpVal `on` snd) . fmap mkC
      where
        mkC :: ColEntry -> (ColEntry, a)
        mkC ce =  (ce, ) $ getVal t ce

sortByName' :: ImgTree -> ColEntries -> ColEntries
sortByName' = sortColEntries' getVal compare
  where
    getVal t =
      colEntry
      (\ j n1 -> (t, j) ^. theEntryName . to (\ n -> Right (n, n1)))
      (\ j    -> (t, j) ^. theEntryName . to         Left          )

sortByDate' :: ImgTree -> ColEntries -> ColEntries
sortByDate' = sortColEntries' getVal compare
  where
    getVal t =
      colEntry
      (\ j n1 -> (t, j) ^. theNode
                         . to (\ n -> n ^. theMetaData)
                         . to (Right . (,n1) . lookupCreate parseTime)
      )
      (\ j    -> (t, j) ^. theEntryName
                         . to  Left
      )

sortMerge' :: ColEntries
           -> (ImgTree -> ColEntries -> ColEntries)
           -> ImgTree -> ColEntries -> ColEntries
sortMerge' cs'new sortCol t cs =
  sortCol t $ mergeColEntries cs cs'new

-- --------------------

adjustImg :: Eff'ISEJL r
          => (ImgParts -> ImgParts) -> ObjId -> Sem r ()
adjustImg f = adjustNodeVal' AdjImgParts theParts (const f)
{-# INLINE adjustImg #-}

adjustDirEntries :: Eff'ISEJL r
                 => (DirEntries -> DirEntries) -> ObjId -> Sem r ()
adjustDirEntries f = adjustNodeVal' AdjDirEntries theDirEntries (const f)
{-# INLINE adjustDirEntries #-}

adjustMetaData :: Eff'ISEJL r
               => (MetaData -> MetaData) -> ObjId -> Sem r ()
adjustMetaData f = adjustNodeVal' AdjMetaData theMetaData (const f)
{-# INLINE adjustMetaData #-}

adjustPartMetaData :: Eff'ISEJL r
                   => (MetaData -> MetaData) -> ImgRef -> Sem r ()
adjustPartMetaData mf (ImgRef i nm) =
  adjustNodeVal' (AdjPartMetaData nm) (theImgPart nm . theImgMeta) (const mf) i

adjustColImg :: Eff'ISEJL r
             => (Maybe ImgRef -> Maybe ImgRef) -> ObjId -> Sem r ()
adjustColImg f = adjustNodeVal' AdjColImg theColImg (const f)
{-# INLINE adjustColImg #-}

adjustColBlog :: Eff'ISEJL r
              => (Maybe ImgRef -> Maybe ImgRef) -> ObjId -> Sem r ()
adjustColBlog f = adjustNodeVal' AdjColBlog theColBlog (const f)
{-# INLINE adjustColBlog #-}

adjustColEntries :: Eff'ISEJL r
                 => (ColEntries -> ColEntries) -> ObjId -> Sem r ()
-- adjustColEntries = adjustNodeVal AdjColEntries theColEntries
adjustColEntries f = adjustColEntries' (const f)
{-# INLINE adjustColEntries #-}

adjustColEntry :: Eff'ISEJL r
               => (ColEntry -> ColEntry) -> Int -> ObjId -> Sem r ()
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
  adjustNodeVal' SetSyncTime theSyncTime (const $ const t) i
{-# INLINE setSyncTime #-}


-- ----------------------------------------
