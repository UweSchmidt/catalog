{-# LANGUAGE
    ConstraintKinds,
    DataKinds,
    FlexibleContexts,
    GADTs,
    OverloadedStrings,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)

------------------------------------------------------------------------------

module Catalog.CopyRemove
  ( copyCollection
  , removeEntry
  , rmRec
  , dupColRec
  , removeEmptyColls
  , cleanupColByPath
  , cleanupAllCollections
  , cleanupCollections
  , selectCollections
  , filterCols
  , filterDirs
  , AdjustImgRef
  , AdjustColEnt
  , cleanupRefs'
  )
where

import Catalog.Effects
import Catalog.ImgTree.Fold
import Catalog.ImgTree.Access
import Catalog.ImgTree.Modify

-- import Data.ColEntrySet ( ColEntrySet
--                         , memberColEntrySet
--                         )
import Data.ImgTree
import Data.MetaData
import Data.Prim

import qualified Data.Set        as S
import qualified Data.Sequence   as Seq

-- ----------------------------------------

copyCollection :: Path -> Path -> SemISEJL r ()
copyCollection path'src path'dst = do

  -- find source and dst id
  -- abort when one of these is not there
  id'src <- fst <$> getIdNode "copyCollection: source not found"      path'src
  id'dst <- fst <$> getIdNode "copyCollection: destination not found" path'dst

  -- check whether an infinite tree would be build
  -- when copying a collection into one of its subcollections
  when (path'src `isPathPrefix` path'dst) $
    throw @Text $ "can't copy a parent collection into a subcollection"

  -- copyColRec id'src id'dst
  srcName   <- getImgName id'src
  dupColRec id'src id'dst srcName

-- ----------------------------------------
--
-- copy a collection src into a collection dstParent
-- with new collection name dstName

dupColRec :: ObjId -> ObjId -> Name -> SemISEJL r ()
dupColRec src dstParent dstName = do
  srcVal  <- getImgVal src
  srcPath <- objid2path src

  unless (isCOL srcVal) $
    throw $ msgPath srcPath "dupColRec: source isn't a collection "

  dstParentVal  <- getImgVal  dstParent
  dstParentPath <- objid2path dstParent

  unless (isCOL dstParentVal) $
    throw $ msgPath dstParentPath "dupColRec: target isn't a collection "

  let dstPath  = dstParentPath `snocPath` dstName
  let editPath = substPathPrefix srcPath dstPath

  void $ createColCopy dstPath src
  copyColEntries editPath src

-- ----------------------------------------
--
-- create a copy of a collection src'id at target'path

createColCopy :: Path -> ObjId -> SemISEJL r ObjId
createColCopy target'path src'id = do
  col'id <- mkCollection target'path

  -- copy collection attributes
  n <- getImgVal src'id
  adjustMetaData (const $ n ^. theColMetaData) col'id
  adjustColImg   (const $ join $ n ^? theColImg)      col'id
  adjustColBlog  (const $ join $ n ^? theColBlog)     col'id
  return col'id


-- create a copy of all collection entries at path
-- computed by path edit function pf and source path

copyColEntries :: (Path -> Path) -> ObjId -> SemISEJL r ()
copyColEntries pf =
      foldMT ignoreImg ignoreDir ignoreRoot colA
      where
        colA go i md im be cs = do
          dst'i  <- (mkObjId . pf) <$> objid2path i
          dst'cs <- mapM copy cs
          adjustColEntries (const dst'cs) dst'i
          adjustColImg     (const im    ) dst'i
          adjustColBlog    (const be    ) dst'i

          -- recurse into subcollections
          foldColEntries go i md im be cs
          where

            copy :: ColEntry -> SemISEJL r ColEntry
            copy ce =
              colEntry'
              (\ _ir -> return ce)
              (\ i'  -> do
                  copy'path <- pf <$> objid2path i'
                  mkColColRef <$> createColCopy copy'path i'
              )
              ce

-- ----------------------------------------

removeEntry :: Path -> SemISEJL r ()
removeEntry p = do
  i <- fst <$> getIdNode "removeEntry: entry not found " p
  rmRec i

rmRec :: ObjId -> SemISEJL r ()
rmRec = foldMT imgA dirA foldRoot colA
  where
    imgA i _p _md = rmImgNode i

    dirA go i es ts = do
      log'trc $ "dirA: " <> toText (i, es ^. isoDirEntries)
      void $ foldDir go i es ts            -- process subdirs first

      pe <- getImgParent i >>= getImgVal   -- remove dir node
      unless (isROOT pe) $                 -- if it's not the top dir
        rmImgNode i

    colA go i _md _im _be cs = do
      log'trc $ "colA: " <> toText cs
      let cs' = Seq.filter isColColRef cs      -- remove all images
      adjustColEntries (const cs') i       -- store remaining collections

      log'trc $ "colA: " <> toText cs'
      traverse_ go $
        cs' ^.. traverse . theColColRef    -- remove the remaining collections

      unlessM (isROOT <$> (getImgParent i >>= getImgVal)) $
        rmImgNode i                        -- remove node unless it's
                                           -- the top collection

-- ----------------------------------------
--
-- delete all empty subcollections in a given collection
-- used for generated collection "bycreatedate"

removeEmptyColls :: Path -> SemISEJL r ()
removeEmptyColls p = do
  i <- fst <$> getIdNode "removeEntry: entry not found " p
  rmEmptyRec i

rmEmptyRec :: ObjId -> SemISEJL r ()
rmEmptyRec i0 = foldCollections colA i0
  where
    colA go i md im be cs = do
      -- log'trc $ "rmEmptyRec: recurse into subdirs"
      void $ foldColEntries go i md im be cs

      cs' <- getImgVals i theColEntries
      if null cs' && i /= i0
        then do
             p <- objid2path i
             log'trc $ msgPath p "rmEmptyRec: remove empty collection "
             rmImgNode i
        else return ()

-- ----------------------------------------

-- traverse all collections and
-- remove entries of images not longer there
-- this is neccessary for consistent collections,
-- when a sync has been done
-- and some images have been deleted
--
-- after a sync run and before the byDate collections are
-- updated, removed images must also be removed in the collections
-- especially in the byDate collections

cleanupColByPath :: Path -> SemISEJL r ()
cleanupColByPath p = do
  log'verb $ msgPath p "cleanupColByPath: cleanup col: "
  lookupByPath p >>= maybe (return ()) (cleanupCollections . fst)

cleanupAllCollections :: SemISEJL r ()
cleanupAllCollections =
  getRootImgColId >>= cleanupCollections

cleanupCollections :: ObjId -> SemISEJL r ()
cleanupCollections i0 = do
  p <- objid2path i0
  log'trc $
    msgPath p "cleanupcollections: existence check of images referenced in "

  cleanup i0

  log'trc $
    msgPath p "cleanupcollections: cleanup finished in "
  where

    cleanup :: ObjId -> SemISEJL r ()
    cleanup i = do
      n <- getImgVal i
      case n of
        COL _md im be es -> do
          cleanupIm i im
          cleanupBe i be
          cleanupEs i es
        _ ->
          return ()
      where

        cleanupIm :: ObjId -> Maybe ImgRef -> SemISEJL r ()
        cleanupIm i' (Just ir) =
          unlessM (exImg ir) $
            adjustColImg (const Nothing) i'
        cleanupIm _ Nothing =
          return ()

        cleanupBe :: ObjId -> Maybe ImgRef -> SemISEJL r ()
        cleanupBe i' (Just ir) =
          unlessM (exImg ir) $
            adjustColBlog (const Nothing) i'
        cleanupBe _ Nothing =
          return ()

        cleanupEs :: ObjId -> ColEntries -> SemISEJL r ()
        cleanupEs i' es = do
          es' <- filterSeqM cleanupE es
          unless (length es' == length es) $
            adjustColEntries (const es') i'
          where
            cleanupE :: ColEntry -> SemISEJL r Bool
            cleanupE (ImgEnt ir) =
              exImg ir
            cleanupE (ColEnt j) = do
              -- recurse into subcollection and cleanup
              cleanup j
              j'not'empty <- (not . null) <$> getImgVals j theColEntries
              -- if collection is empty, remove it
              unless j'not'empty $
                rmRec j
              return j'not'empty

        exImg :: ImgRef -> SemISEJL r Bool
        exImg (ImgRef i' n') = do
          me <- getTreeAt i'
          let ex = case me of
                Just e
                  | isIMG (e ^. nodeVal) ->
                    let ns = e ^.. nodeVal . theParts . thePartNamesI in
                    n' `elem` ns
                _ ->
                  False
          unless ex $
            log'warn $
            "exImg: image ref found in a collection for a deleted image: "
            <> toText (i', n')

          return ex

-- ----------------------------------------
--
-- change or remove an ImgRef value within a collection
--
-- outer Maybe: Just ...  -> value has changed
--              Nothing   -> value not changed
-- inner Maybe: Just ...  -> the new value
--              Nothing   -> entry must be removed

type AdjustImgRef = Maybe ImgRef -> Maybe (Maybe ImgRef)
type AdjustColEnt = ColEntries   -> Maybe  ColEntries

cleanupRefs' :: forall r. AdjustImgRef -> AdjustColEnt -> ObjId -> SemISEJL r ()
cleanupRefs' adjIR adjCE i0 =
  foldCollections colA i0
  where
    colA go i _md im be es = do
      p <- objid2path i
      cleanupIm p
      cleanupBe p
      cleanupEs p
      cleanupSubCols
      removeEmptySubCols
      return ()
      where

        cleanupIm :: Path -> SemISEJL r ()
        cleanupIm p = case adjIR im of
          Nothing -> return ()
          Just new'im -> do
            log'trc $ msgPath p "cleanupRefs: col img changed: "
            log'trc $ "old: " <> toText im
            log'trc $ "new: " <> toText new'im
            adjustColImg (const new'im) i

        cleanupBe :: Path -> SemISEJL r ()
        cleanupBe p = case adjIR be of
          Nothing -> return ()
          Just new'be -> do
            log'trc $ msgPath p "cleanupRefs: col blog changed: "
            log'trc $ "old: " <> toText be
            log'trc $ "new: " <> toText new'be
            adjustColBlog (const new'be) i

        cleanupEs :: Path -> SemISEJL r ()
        cleanupEs p = case adjCE es of
          Nothing -> return ()
          Just new'es -> do
            log'trc $ msgPath p "cleanupRefs: col entries changed: "
            log'trc $ "old: " <> toText (es     ^. isoSeqList)
            log'trc $ "new: " <> toText (new'es ^. isoSeqList)
            adjustColEntries (const new'es) i

        cleanupSubCols :: SemISEJL r ()
        cleanupSubCols =
          traverse_ cleanupSubCol es
          where
            cleanupSubCol =
              colEntry' (const $ return ()) go

        removeEmptySubCols :: SemISEJL r ()
        removeEmptySubCols = do
          -- recompute colentries, maybe modified by calls of cleanupSubCol
          es1 <- getImgVals i theColEntries
          es2 <- emptySubCols es1
          mapM_ rmRec es2

        emptySubCols :: ColEntries -> SemISEJL r [ObjId]
        emptySubCols = foldM checkESC []
          where
            checkESC :: [ObjId] -> ColEntry -> SemISEJL r [ObjId]
            checkESC res =
              colEntry'
              (\ _i -> return res)
              (\ ci -> do cn <- getImgVal ci
                          return $
                            if isCOL cn
                               &&
                               null (cn ^. theColEntries)
                               &&
                               isRemovable (cn ^. theMetaData)
                            then ci : res
                            else      res
              )

-- ----------------------------------------

selectCollections :: ObjIds -> SemISEJL r ()
selectCollections cols = do
  subcols <- foldObjIds allColObjIds cols
  filterCols subcols                   -- remove all but these collections

  imgs <- getRootImgColId
          >>=
          allImgObjIds                 -- collect all image refs in remaining cols
  filterDirs imgs                      -- remove all images except these from dir

  return ()                            -- everything not selected is removed

filterDirs :: ObjIds -> SemISEJL r ()
filterDirs imgs =
  getRootId >>= foldMTU imgA dirA foldRootDir ignoreCol
  where
    imgA i _pts _md
      | i `S.member` imgs =                 -- image referenced
          return ()                         -- don't touch
      | otherwise =                         -- else
          rmImgNode i                       -- remove it

    dirA go i es ts = do
      void $ foldDir go i es ts             -- remove all images in sudirs

      e <- isempty <$> getImgVal i          -- dir now empty?
      when e $ do
        r <- isROOT                         -- and
             <$>
             (getImgParent i >>= getImgVal) -- dir not top level (child of root)?
        unless r $
          rmRec i                           -- remove dir

filterCols :: ObjIds -> SemISEJL r ()
filterCols cols =
  getRootId >>= foldMTU ignoreImg ignoreDir foldRootCol colA
  where
    colA go i md im be es
      | i `S.member` cols =                 -- collection selected?
          return ()                         -- don't touch it
      | otherwise = do
          a <- isAncestorCol i              -- ancestor of a selected col?
          if a
            then                            -- traverse subcollections
              foldColEntries go i md im be es
            else
              rmRec i                       -- not selected, remove

    isAncestorCol i =
      getAny <$> foldObjIds isAnc cols
      where
        isAnc c = Any <$> isPartOfTree c i

-- ----------------------------------------
