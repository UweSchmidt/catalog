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
       ( log'trc
       , log'verb
       , log'warn
       , throw
       , Sem
       , Eff'ISEJL
       )
import Catalog.ImgTree.Fold
       ( allColObjIds
       , allImgObjIds
       , foldColEntries
       , foldCollections
       , foldDir
       , foldMT
       , foldMTU
       , foldRoot
       , foldRootCol
       , foldRootDir
       , ignoreCol
       , ignoreDir
       , ignoreImg
       , ignoreRoot
       )
import Catalog.ImgTree.Access
       ( foldObjIds
       , getIdNode
       , getImgName
       , getImgParent
       , getImgVal
       , getImgVals
       , getRootId
       , getRootImgColId
       , getTreeAt
       , isPartOfTree
       , lookupByPath
       , objid2path
       )
import Catalog.ImgTree.Modify
       ( adjustColBlog
       , adjustColEntries
       , adjustColImg
       , adjustMetaData
       , mkCollection
       , rmImgNode
       )

-- import Data.ColEntrySet ( ColEntrySet
--                         , memberColEntrySet
--                         )
import Data.ImgTree
       ( colEntry'
       , isCOL
       , isColColRef
       , isIMG
       , isROOT
       , isoDirEntries
       , mkColColRef
       , theColBlog
       , theColColRef
       , theColEntries
       , theColImg
       , theColMetaData
       , theMetaData
       , thePartNamesI
       , theParts
       , nodeVal
       , ColEntry'(ColEnt, ImgEnt)
       , ImgNode'(COL)
       , ImgRef
       , ImgRef'(ImgRef)
       , ObjIds
       , ColEntries
       , ColEntry
       )
import Data.MetaData
       ( isRemovable )

import Data.Prim

import qualified Data.Set        as S
import qualified Data.Sequence   as Seq

-- ----------------------------------------

copyCollection :: Eff'ISEJL r => Path -> Path -> Sem r ()
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

dupColRec :: Eff'ISEJL r => ObjId -> ObjId -> Name -> Sem r ()
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

createColCopy :: Eff'ISEJL r => Path -> ObjId -> Sem r ObjId
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

copyColEntries :: Eff'ISEJL r => (Path -> Path) -> ObjId -> Sem r ()
copyColEntries pf =
      foldMT ignoreImg ignoreDir ignoreRoot colA
      where
        colA go i md im be cs = do
          dst'i  <- mkObjId . pf <$> objid2path i
          dst'cs <- mapM copy cs
          adjustColEntries (const dst'cs) dst'i
          adjustColImg     (const im    ) dst'i
          adjustColBlog    (const be    ) dst'i

          -- recurse into subcollections
          foldColEntries go i md im be cs
          where

            copy :: Eff'ISEJL r => ColEntry -> Sem r ColEntry
            copy ce =
              colEntry'
              (\ _ir -> return ce)
              (\ i'  -> do
                  copy'path <- pf <$> objid2path i'
                  mkColColRef <$> createColCopy copy'path i'
              )
              ce

-- ----------------------------------------

removeEntry :: Eff'ISEJL r => Path -> Sem r ()
removeEntry p = do
  i <- fst <$> getIdNode "removeEntry: entry not found " p
  rmRec i

rmRec :: Eff'ISEJL r => ObjId -> Sem r ()
rmRec = foldMT imgA dirA foldRoot colA
  where
    imgA i _p _md = rmImgNode i

    dirA go i es ts = do
      log'trc $ "remRec: remove dirs:\n" <> prettyJSONText [] (i, es ^. isoDirEntries)
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

removeEmptyColls :: Eff'ISEJL r => Path -> Sem r ()
removeEmptyColls p = do
  i <- fst <$> getIdNode "removeEntry: entry not found " p
  rmEmptyRec i

rmEmptyRec :: Eff'ISEJL r => ObjId -> Sem r ()
rmEmptyRec i0 = foldCollections colA i0
  where
    colA go i md im be cs = do
      -- log'trc $ "rmEmptyRec: recurse into subdirs"
      void $ foldColEntries go i md im be cs

      cs' <- getImgVals i theColEntries
      when (null cs' && i /= i0) $ do
        p <- objid2path i
        log'trc $ msgPath p "rmEmptyRec: remove empty collection "
        rmImgNode i

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

cleanupColByPath :: Eff'ISEJL r => Path -> Sem r ()
cleanupColByPath p = do
  log'verb $ msgPath p "cleanupColByPath: cleanup col: "
  lookupByPath p >>= maybe (return ()) (cleanupCollections . fst)

cleanupAllCollections :: Eff'ISEJL r => Sem r ()
cleanupAllCollections =
  getRootImgColId >>= cleanupCollections

cleanupCollections :: Eff'ISEJL r => ObjId -> Sem r ()
cleanupCollections i0 = do
  p <- objid2path i0
  log'trc $
    msgPath p "cleanupcollections: existence check of images referenced in "

  cleanup i0

  log'trc $
    msgPath p "cleanupcollections: cleanup finished in "
  where

    cleanup :: Eff'ISEJL r => ObjId -> Sem r ()
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

        cleanupIm :: Eff'ISEJL r => ObjId -> Maybe ImgRef -> Sem r ()
        cleanupIm i' (Just ir) =
          unlessM (exImg ir) $
            adjustColImg (const Nothing) i'
        cleanupIm _ Nothing =
          return ()

        cleanupBe :: Eff'ISEJL r => ObjId -> Maybe ImgRef -> Sem r ()
        cleanupBe i' (Just ir) =
          unlessM (exImg ir) $
            adjustColBlog (const Nothing) i'
        cleanupBe _ Nothing =
          return ()

        cleanupEs :: Eff'ISEJL r => ObjId -> ColEntries -> Sem r ()
        cleanupEs i' es = do
          es' <- filterSeqM cleanupE es
          unless (length es' == length es) $
            adjustColEntries (const es') i'
          where
            cleanupE :: Eff'ISEJL r => ColEntry -> Sem r Bool
            cleanupE (ImgEnt ir) =
              exImg ir
            cleanupE (ColEnt j) = do
              -- recurse into subcollection and cleanup
              cleanup j
              j'not'empty <- not . null <$> getImgVals j theColEntries
              -- if collection is empty, remove it
              unless j'not'empty $
                rmRec j
              return j'not'empty

        exImg :: Eff'ISEJL r => ImgRef -> Sem r Bool
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

cleanupRefs' :: forall r. Eff'ISEJL r
             =>  AdjustImgRef -> AdjustColEnt -> ObjId -> Sem r ()
cleanupRefs' adjIR adjCE =
  foldCollections colA
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

        cleanupIm :: Eff'ISEJL r => Path -> Sem r ()
        cleanupIm p = case adjIR im of
          Nothing -> return ()
          Just new'im -> do
            log'trc $ msgPath p "cleanupRefs: col img changed:\n"
            log'trc $ "old:\n" <> prettyJSONText [] im
            log'trc $ "new:\n" <> prettyJSONText [] new'im
            adjustColImg (const new'im) i

        cleanupBe :: Eff'ISEJL r => Path -> Sem r ()
        cleanupBe p = case adjIR be of
          Nothing -> return ()
          Just new'be -> do
            log'trc $ msgPath p "cleanupRefs: col blog changed:\n"
            log'trc $ "old:\n" <> prettyJSONText [] be
            log'trc $ "new:\n" <> prettyJSONText [] new'be
            adjustColBlog (const new'be) i

        cleanupEs :: Eff'ISEJL r => Path -> Sem r ()
        cleanupEs p = case adjCE es of
          Nothing -> return ()
          Just new'es -> do
            log'trc $ msgPath p "cleanupRefs: col entries changed:\n"
            log'trc $ "old:\n" <> prettyJSONText [] (es ^. isoSeqList)
            log'trc $ "new:\n" <> prettyJSONText [] (new'es ^. isoSeqList)
            adjustColEntries (const new'es) i

        cleanupSubCols :: Eff'ISEJL r => Sem r ()
        cleanupSubCols =
          traverse_ cleanupSubCol es
          where
            cleanupSubCol =
              colEntry' (const $ return ()) go

        removeEmptySubCols :: Eff'ISEJL r => Sem r ()
        removeEmptySubCols = do
          -- recompute colentries, maybe modified by calls of cleanupSubCol
          es1 <- getImgVals i theColEntries
          es2 <- emptySubCols es1
          mapM_ rmRec es2

        emptySubCols :: Eff'ISEJL r => ColEntries -> Sem r [ObjId]
        emptySubCols = foldM checkESC []
          where
            checkESC :: Eff'ISEJL r => [ObjId] -> ColEntry -> Sem r [ObjId]
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

selectCollections :: Eff'ISEJL r => ObjIds -> Sem r ()
selectCollections cols = do
  subcols <- foldObjIds allColObjIds cols
  filterCols subcols                   -- remove all but these collections

  imgs <- getRootImgColId
          >>=
          allImgObjIds                 -- collect all image refs in remaining cols
  filterDirs imgs                      -- remove all images except these from dir

  return ()                            -- everything not selected is removed

filterDirs :: Eff'ISEJL r => ObjIds -> Sem r ()
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

      e <- isEmpty <$> getImgVal i          -- dir now empty?
      when e $ do
        r <- isROOT                         -- and
             <$>
             (getImgParent i >>= getImgVal) -- dir not top level (child of root)?
        unless r $
          rmRec i                           -- remove dir

filterCols :: Eff'ISEJL r => ObjIds -> Sem r ()
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
