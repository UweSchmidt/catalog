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
       (-- foldColEntries
       -- ,
         foldDir
       , foldMT
       , foldRoot
 --      , ignoreDir
 --      , ignoreImg
--       , ignoreRoot
       )
import Catalog.ImgTree.Access
import Catalog.ImgTree.Modify

-- import Data.ColEntrySet ( ColEntrySet
--                         , memberColEntrySet
--                         )
import Data.ImgTree hiding (allImgObjIds)
import Data.MetaData
       ( isRemovable )

import Data.Prim

-- import qualified Data.Set        as S
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
  adjustMetaData (const $ n ^. theColMetaData)        col'id
  adjustColImg   (const $ join $ n ^? theColImg)      col'id
  adjustColBlog  (const $ join $ n ^? theColBlog)     col'id
  return col'id


-- create a copy of all collection entries at path
-- computed by path edit function pf and source path

{- old with foldMT
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
 -}

copyColEntries :: Eff'ISEJL r => (Path -> Path) -> ObjId -> Sem r ()
copyColEntries pf = go
  where
    go i = withTF $ \ t ->
      case (t, i) ^. theNode of
        n | isCOL n -> do
              -- copy col img
              traverseOf_ theColImg  (\ im -> adjustColImg  (const im) dst'i) n

              -- copy col blog
              traverseOf_ theColBlog (\ be -> adjustColBlog (const be) dst'i) n

              -- copy col entries
              -- create entries: copy ImgRefs, create empty cols for ColRefs
              dst'cs <- traverse copy (n ^. theColEntries)
              -- set col entries
              adjustColEntries (const dst'cs) dst'i
              -- recurse into subcollections
              traverseOf_ (theColEntries . traverse . theColColRef) go n

          | otherwise ->
              return ()
          where
            pf' i1 = pf (refPath i1 t)
            dst'i  = mkObjId (pf' i)

            copy :: Eff'ISEJL r => ColEntry -> Sem r ColEntry
            copy ce =
              colEntry'
              (\ _ir -> return ce)
              (\ i'  -> mkColColRef <$> createColCopy (pf' i') i')
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
      log'trc $ "dirA: " <> toText (i, es ^. isoDirEntries)
      void $ foldDir go i es ts            -- process subdirs first

      pe <- getImgParent i >>= getImgVal   -- remove dir node
      unless (isROOT pe) $                 -- if it's not the top dir
        rmImgNode i

    colA go i _md _im _be cs = do
      adjustColBlog (const Nothing) i
      adjustColImg  (const Nothing) i

      log'trc $ "colA: " <> toText cs
      let cs' = Seq.filter isColColRef cs  -- remove all images
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
rmEmptyRec i0 = go i0
  where
    go i = withTF $ \ t ->
      case (t, i) ^. theNode of
        n | isCOL n -> do
              -- rem empty subcollections
              traverseOf_ (theColEntries . traverse . theColColRef) go n
              -- rem entry, when afterwards the coll itself is empty
              remEmpty i
          | otherwise ->
              return ()

    remEmpty i = withTF $ \ t ->
      when ( hasn't (theNode . imgNodeRefs) (t, i)
             &&
             i /= i0
           ) $
      do
        log'trc $ msgPath (refPath i t) "rmEmptyRec: remove empty collection "
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
          e <- getTreeAt i'
          let ex = elemOf ( nodeVal
                          . theParts
                          . thePartNamesI
                          ) n' e
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
cleanupRefs' adjIR adjCE = go
  where
    go i = withTF $ \ t ->
      case (t, i) ^. theNode of
        n | isCOL n -> do
              traverseOf_ theColImg     cleanupIm n
              traverseOf_ theColBlog    cleanupBe n
              traverseOf_ theColEntries cleanupEs n
              traverseOf_ (theColEntries . traverse)
                          (colEntry' (const $ return ()) go)
                          n
              removeEmptySubCols i
              return ()

          | otherwise ->
              return ()

          where
            p  = refPath i t

            cleanupIm :: Eff'ISEJL r => Maybe ImgRef -> Sem r ()
            cleanupIm im = case adjIR im of
              Nothing -> return ()
              Just new'im -> do
                log'trc $ msgPath p "cleanupRefs: col img changed: "
                log'trc $ "old: " <> toText im
                log'trc $ "new: " <> toText new'im
                adjustColImg (const new'im) i

            cleanupBe :: Eff'ISEJL r => Maybe ImgRef -> Sem r ()
            cleanupBe be = case adjIR be of
              Nothing -> return ()
              Just new'be -> do
                log'trc $ msgPath p "cleanupRefs: col blog changed: "
                log'trc $ "old: " <> toText be
                log'trc $ "new: " <> toText new'be
                adjustColBlog (const new'be) i

            cleanupEs :: Eff'ISEJL r => ColEntries -> Sem r ()
            cleanupEs es = case adjCE es of
              Nothing -> return ()
              Just new'es -> do
                log'trc $ msgPath p "cleanupRefs: col entries changed: "
                log'trc $ "old: " <> toText (es     ^. isoSeqList)
                log'trc $ "new: " <> toText (new'es ^. isoSeqList)
                adjustColEntries (const new'es) i


removeEmptySubCols :: Eff'ISEJL r => ObjId -> Sem r ()
removeEmptySubCols i = withTF go
  where
    go t =
      traverseOf_
        (theNode . theColEntries . traverse . emptySubCol' t)
        rmRec
        (t, i)

emptySubCol' :: ImgTree -> Fold ColEntry ObjId
emptySubCol' t = folding go
  where
    go ce =
      ce ^.. theColColRef
           . filteredBy
             ( to (t,)
             . theNode
             . filtered
               ( \ n -> isCOL n
                        &&
                        hasn't (theColEntries . traverse) n
                        &&
                        isRemovable (n ^. theMetaData)
               )
             )

-- ----------------------------------------
