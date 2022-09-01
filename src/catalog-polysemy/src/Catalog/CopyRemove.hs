------------------------------------------------------------------------------

module Catalog.CopyRemove
  ( copyCollection
  , removeEntry
  , rmRec
  , dupColRec
  , removeEmptyColls
  , AdjustImgRef
  , AdjustColEnt
  , cleanupRefs'
  )
where

import Catalog.Effects
       ( log'trc
       , throw
       , Sem
       , Eff'ISEJL
       )

import Catalog.ImgTree.Access
import Catalog.ImgTree.Modify

import Data.ImgTree
import Data.MetaData
       ( isRemovable )

import Data.Prim

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

-- remove a subtree of entries (COL, DIR, IMG)

rmRec :: Eff'ISEJL r => ObjId -> Sem r ()
rmRec = go
  where
    go i = withTF $ \ t ->
      let en = (t, i) ^. theEntry
          n  = en ^. nodeVal
          pr = en ^. parentRef
          rm = unless (isROOT $ (t, pr) ^. theNode)
                 (rmImgNode i)
      in
        do
          log'trc $ "rmRec: " <> toText (i, n)

          case n of
            _ | isIMG n ->      -- this case is only reached from DIR entries
                  rm

              | isDIR n -> do
                  -- remove subdirs
                  traverseOf_ imgNodeRefs go n
                  rm

              | isCOL n -> do
                  -- clear col img ref
                  adjustColImg  (const Nothing) i
                  -- clear col block ref
                  adjustColBlog (const Nothing) i
                  -- remove all subcollection
                  traverseOf_ (theColEntries . traverse)
                              (colEntry' (const $ return ())
                                          go
                              ) n
                  -- clear all entries
                  adjustColEntries (const mempty) i
                  rm

              | isROOT n ->
                  traverseOf_ imgNodeRefs go n

              | otherwise ->
                  return ()



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
    emptySubCol' t = folding gogo
      where
        gogo ce =
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
