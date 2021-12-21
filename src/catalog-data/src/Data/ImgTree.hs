{-# LANGUAGE FlexibleContexts #-}

module Data.ImgTree
       ( module Data.RefTree
       , module Data.ImgNode
       , module Data.ImgTree
       )
       where

import Control.Monad.Except
import Data.ImgNode
import Data.Prim
import Data.RefTree

import Data.Map as M

-- ----------------------------------------

type ImgTree    = DirTree ImgNode' ObjId
type UplNode    = UpLink  ImgNode' ObjId
type ImgNode    = ImgNode'    ObjId
type ColEntry   = ColEntry'   ObjId
type ColEntries = ColEntries' ObjId
type DirEntries = DirEntries' ObjId

type ImgNodeP   = ImgNode' Path
type ImgTreeP   = DirTree ImgNode' Path

-- ----------------------------------------

-- the tree for the image hierachy

mkEmptyImgRoot :: Name           -- ^ name of root node
               -> Name           -- ^ name of topmost image dir
               -> Name           -- ^ name of topmost collection
               -> Except String
                         ImgTree -- ^ an empty catalog, no images, no collections
mkEmptyImgRoot rootName imgName colName =
  do (_r1,t1) <- mkDirNode mkObjId isROOT addImgArchive imgName r emptyImgDir t0
     (_r2,t2) <- mkDirNode mkObjId isROOT addImgCol     colName r emptyImgCol t1
     return t2
  where
    t0 = mkDirRoot mkObjId rootName emptyImgRoot
    r  = t0 ^. rootRef

    addImgArchive r' n = n & theRootImgDir .~ r'
    addImgCol     r' n = n & theRootImgCol .~ r'

mkImgRoot :: Name -> ImgNode -> ImgTree
mkImgRoot = mkDirRoot mkObjId
{-# INLINE mkImgRoot #-}

mkNode :: (ImgNode -> Bool)
       -> Name                    -- ^ name of the node
       -> ObjId                   -- ^ parent node of the tree
       -> ImgNode                 -- ^ node value
       -> ImgTree                 -- ^ the tree into which the node is added
       -> Except String
                 (ObjId, ImgTree) -- ^ new ref of node and extended tree
mkNode isN =
  mkDirNode mkObjId isN addChildRef
{-# INLINE mkNode #-}

lookupImgPath :: Path -> ImgTree -> Maybe (ObjId, ImgNode)
lookupImgPath =
  lookupDirPath mkObjId
{-# INLINE lookupImgPath #-}

-- | remove an image node or a dir node without entries
removeImgNode :: ObjId
              -> ImgTree
              -> Except String ImgTree
removeImgNode =
  remDirNode isempty removeChildRef
{-# INLINE removeImgNode #-}

addChildRef :: ObjId -> ImgNode -> ImgNode
addChildRef r n =
  n & theDirEntries %~ addDirEntry r
{-# INLINE addChildRef #-}

-- | remove a child from an image dir or collection node
removeChildRef :: ObjId -> ImgNode -> ImgNode
removeChildRef r n =
  n & theDirEntries %~ delDirEntry r
    & theColEntries %~ delColEntry r
{-# INLINE removeChildRef #-}

-- ----------------------------------------

keysImgTree :: ImgTree -> [ObjId]
keysImgTree t = t ^. entries . to M.keys

-- ----------------------------------------
