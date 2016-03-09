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

import qualified Data.Set as S

-- ----------------------------------------

type ImgTree  = DirTree ImgNode' ObjId
type ImgNode  = ImgNode'  ObjId
type ColEntry = ColEntry' ObjId

-- ----------------------------------------

-- the tree for the image hierachy

mkEmptyImgRoot :: (MonadError String m) =>
                  Name -> Name -> Name -> m ImgTree
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

mkNode ::  (MonadError String m) =>
           (ImgNode -> Bool) ->
           Name ->                       -- name of the node
           ObjId ->                      -- parent node
           ImgNode ->                    -- node value
           ImgTree -> m (ObjId, ImgTree) -- new ref and modified tree
mkNode isN = mkDirNode mkObjId isN addChildRef

lookupImgPath :: Path -> ImgTree -> Maybe (ObjId, ImgNode)
lookupImgPath = lookupDirPath mkObjId

-- | remove an image node or a dir node without entries
removeImgNode :: (MonadError String m) =>
                 ObjId ->
                 ImgTree -> m ImgTree
removeImgNode = remDirNode isempty removeChildRef

addChildRef :: ObjId -> ImgNode -> ImgNode
addChildRef r n = n & theDirEntries %~ S.insert r

-- | remove a child from an image dir node
removeChildRef :: ObjId -> ImgNode -> ImgNode
removeChildRef r n = n & theDirEntries %~ S.delete r

-- ----------------------------------------
