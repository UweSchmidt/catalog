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
        noImgRef = hasn't (imgref . isR isIMG)

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

-- --------------------
--
-- result: ((ref, correct parentref), currently wrong parentref)

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
                    . filtered (not . isRootRef p)
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

-- -----------------------------------------
