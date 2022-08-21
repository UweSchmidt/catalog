{-# LANGUAGE TupleSections #-}

-- ----------------------------------------

module Data.ImgTree.Invariants
where

import Data.Prim
import Data.ImgTree.ImgNode
import Data.ImgTree.RefTree
import Data.ImgTree.NavTree

import qualified Data.Map.Strict as M
import qualified Data.Set        as S

-- ----------------------------------------

noJunkInDirs :: Fold NavTree (ObjId, ObjId)
noJunkInDirs = allEntries
             . filteredBy (theNode . filtered isDIR)
             . folding f
  where
    f t@(_, r) =
      t ^.. theChildren
          . filteredBy (theNode . filtered (not . ((||) <$> isDIR <*> isIMG)))
          . _2
          . to (, r)

noJunkInColRefs :: Fold NavTree (ObjId, ObjId)
noJunkInColRefs = allEntries
                . filteredBy (theNode . filtered isCOL)
                . folding f
  where
    f t@(_, r) =
      t ^.. theNode
          . ( theColEntries . tr'ces
              <>
              theColImg     . tr'ib
              <>
              theColBlog    . tr'ib
            )
          . to (, r)
      where
        tr'ces   = traverse . filtered noRef    . theColObjId
        tr'ib    = traverse . filtered noImgRef . imgref

        isR      :: (ImgNode -> Bool) -> Fold ObjId ImgNode
        isR   p  = setCur t . theNode . filtered p

        noRef    :: ColEntry -> Bool
        noRef    = colEntry' noImgRef noColRef

        noImgRef :: ImgRef -> Bool
        noImgRef = hasn't (imgref . isR ((&&) <$> isIMG <*> (not . isNUL)))
                                          -- TODO: hack
        noColRef :: ObjId -> Bool
        noColRef = hasn't (         isR isCOL)

noJunkInPartNames :: Fold NavTree (ImgRef, ObjId)
noJunkInPartNames = allEntries
                . filteredBy (theNode . filtered isCOL)
                . folding f
  where
    f t@(_, r) =
      t ^.. theNode
          . colNodeImgRefs
          . folding g
          . to (,r)
      where
        g ir =
          ir ^.. filtered noName
          where
            inm = ir ^. imgname

            noName =
              hasn't ( imgref
                     . setCur t
                     . theNode
                     . theParts
                     . thePartNames
                     . filtered (== inm)
                     )
{-
allColrefJunk :: Fold NavTree (NavTree, ObjIds)
allColrefJunk =
  allEntries
  . filteredBy (theNode . filtered isCOL)
  . colrefJunk

colrefJunk :: Fold NavTree (NavTree, ObjIds)
colrefJunk = folding f
  where
    f t
      | has undefColNodeColRefs t = [(t, irs)]
      | otherwise                 = []
      where
        irs = foldMapOf undefColNodeColRefs S.singleton t

undefColNodeColRefs :: Fold NavTree ObjId
undefColNodeColRefs = folding f
  where
    f t =
      t ^.. theNode
          . colNodeColRefs
          . filtered
            ( has ( setCur t
                  . theNode
                  . filtered (not . isCOL)
                  )
            )

allImgrefJunk :: Fold NavTree (NavTree, Set ImgRef)
allImgrefJunk =
  allEntries
  . filteredBy (theNode . filtered isCOL)
  . imgrefJunk

imgrefJunk :: Fold NavTree (NavTree, Set ImgRef)
imgrefJunk = folding f
  where
    f :: NavTree -> [(NavTree, Set ImgRef)]
    f t
      | has undefColNodeImgRefs t = [(t, irs)]
      | otherwise                 = []
      where
        irs = foldMapOf undefColNodeImgRefs S.singleton t

-- select all imgrefs in the COL node of a tree
undefColNodeImgRefs :: Fold NavTree ImgRef
undefColNodeImgRefs = folding f
  where
    f t =
      t ^.. theNode
          . colNodeImgRefs
          . filtered
            (\ ir ->
               has ( imgref
                   . setCur t
                   . theNode
                   . filtered ( not
                              . ( (&&)
                                  <$> isIMG
                                  <*> has (theImgPart (ir ^. imgname))
                                )
                              )
                   ) ir
            )
-- -}

-- --------------------
--
-- result: ((ref, correct parentref), current wrong parentref)

uplinkCheck :: Fold NavTree ((ObjId, ObjId), ObjId)
uplinkCheck = allTrees                 -- iterate over all trees
            . folding f
  where
    f :: NavTree -> [((ObjId, ObjId), ObjId)]
    f tp@(_, p) =                       -- the parent tree
      tp ^.. theChildren
           . folding
             (\ t@(_, r) ->             -- the child tree
                t ^.. filteredBy
                      ( theNode
                      . filtered (\ n -> not (isIMG n && parentIsCol))
                      )
                    . theEntry
                    . parentRef
                    . filtered (/= p)
                    . to ((r, p),)
            )
      where
        parentIsCol = has (theNode . filtered isCOL) tp

-- refs in map not occuring in tree

allOrphanObjIds    :: Fold NavTree ObjId
allOrphanObjIds    = folding f
  where
    f t =
      t ^.. _1
          . entries
          . to M.keys
          . folded
          . filtered (not . (`S.member` ids))
      where
        ids = allObjIds t

-- ----------------------------------------
{-
--
-- remove undefined ObjIds
-- from a COL od IMG node

cleanupUndefRefs :: (ObjId -> DirEntries -> r)
                 -> (ObjId -> ColEntries -> r)
                 -> (ObjId               -> r)
                 -> (ObjId               -> r)
                 -> Fold NavTree ObjId
                 -> Fold NavTree r
cleanupUndefRefs
  setDirEntries
  setColEntries
  clearColImg
  clearColBlog
  undefsF
  = folding f
  where
    f t0 = t0 ^.. allTrees . folding editNode
      where
        undefs = foldMapOf undefsF S.singleton t0

        editNode t@(_, r) =
          dirEdit <> colEdit
          where
            n  :: ImgNode
            n  = t ^. theNode

            dirEdit
              | anyOf (theDirEntries . traverse)
                      (`S.member` undefs)
                      n =
              -- undefined references found
                [setDirEntries r $ filterDirEntries noJunk des]

              | otherwise = []
              where
                des    = n ^. theDirEntries
                noJunk = not . (`S.member` undefs)

            colEdit = colImgEdit <> colBlogEdit <> colEntryEdit

            colEntryEdit
              | anyOf (theColEntries . traverse . theColObjId)
                      (`S.member` undefs)
                      n =
                -- undefined references found
                [setColEntries r $ filterColEntries noJunk ces]

              | otherwise = []
              where
                ces = n ^. theColEntries
                noJunk = has ( theColObjId
                             . filtered (not . (`S.member` undefs))
                             )

            colImgEdit  = colRefEdit theColImg  clearColImg
            colBlogEdit = colRefEdit theColBlog clearColBlog

            colRefEdit theColRef cmd
              | anyOf (theColRef . traverse . imgref)
                      (`S.member` undefs)
                      n =
                [cmd r]
              | otherwise = []
-- -}
-- -----------------------------------------
{-
cleanupImgrefJunk :: (ObjId -> ColEntries -> r)
                  -> (ObjId               -> r)
                  -> (ObjId               -> r)
                  -> Fold (NavTree, Set ImgRef) r
cleanupImgrefJunk
  setColEntries
  clearColImg
  clearColBlog = folding f
  where
    f (t, irs) =
      t ^. theNode . to (editImg <> editBlog <> editEntries)
      where
        r = t ^. _2

        editRef theRef cmd n
          | has ( theRef
                . traverse
                . filtered (`S.member` irs)
                ) n   = [cmd r]
          | otherwise = []

        editBlog    = editRef theColBlog clearColBlog
        editImg     = editRef theColImg  clearColImg

        editEntries n =
          [setColEntries r $ filterColEntries noJunk ces]
          where
            ces    = n ^. theColEntries

            noJunk = colEntry'
                     (not . (`S.member` irs))
                     (const True)

-- -----------------------------------------

cleanupColrefJunk :: (ObjId -> ColEntries -> r)
                  -> Fold (NavTree, ObjIds) r
cleanupColrefJunk
  setColEntries = folding f
  where
    f (t, crs) =
      t ^. theNode . to editEntries
      where
        r = t ^. _2

        editEntries n =
          [setColEntries r $ filterColEntries noJunk ces]
          where
            ces    = n ^. theColEntries

            noJunk = colEntry'
                     (const True)
                     (not . (`S.member` crs))
-- -}
-- -----------------------------------------
