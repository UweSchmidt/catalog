{-# LANGUAGE TupleSections #-}

-- ----------------------------------------

module Data.ImgTree.NavTree
where

import Data.Prim
import Data.ImgTree.ImgNode
import Data.ImgTree.RefTree
import Data.ImgTree.ImgTree

import Text.SimpleParser (matchGlobPattern)

import qualified Data.Map.Strict as M
import qualified Data.Set        as S

-- ----------------------------------------
--
-- a navigatable ImgTree consists of a pair (ImgTree, ObjId)
-- where ObjId is the current position (the curser)

type NavTree    = (ImgTree, ObjId)

type NameFilter = [Name -> Bool]

-- ----------------------------------------
--
-- basic constructors and predicates

mkNavTree :: ImgTree -> NavTree
mkNavTree t = (t, t ^. rootRef)

-- ----------------------------------------
--
-- Optics for NavTree(s)


-- conversion ImgTree <-> NavTree

isoNavTree :: Iso' ImgTree NavTree
isoNavTree = iso mkNavTree fst

setCur :: NavTree -> Getter ObjId NavTree
setCur (t, _) = to $ (t,)

-- --------------------
-- basic optics

-- get uplink node of current ObjId

theEntry :: Lens' NavTree UplNode
theEntry =
  lensNavTree (\ ref -> entries . emAt ref)

-- get/insert/remove entry in tree map
--
-- indexing is a total function

emAt :: ObjId -> Lens' (M.Map ObjId UplNode) UplNode
emAt ref = lens g s
  where
    g em = M.findWithDefault (emptyUplNode ref) ref em
    s em v
      | isempty v = M.delete ref   em
      | otherwise = M.insert ref v em

-- get ImgNode of current ObjId

theNode :: Getter NavTree ImgNode
theNode = theEntry . nodeVal

-- get path for current ObjId

thePath :: Getter NavTree Path
thePath = to f
  where
    f :: NavTree -> Path
    f nt@(t, ref)
      | ref == p  = mkPath n
      | otherwise = f (t, p) `snocPath` n
      where
        p = e  ^. parentRef
        n = e  ^. nodeName
        e = nt ^. theEntry

-- get Path for current ObjId

objId2Path :: Fold NavTree Path
objId2Path = folding f
  where
    f (t, ref)
      | isempty p = Nothing
      | otherwise = Just p
      where
        p = refPath ref t

-- --------------------
-- basic Fold(s)


-- navigate from a ROOT to the COL or DIR child

aRootRef ::  Traversal' ImgNode ObjId ->  Fold NavTree NavTree
aRootRef rf = foldingTree visit
  where
    visit t ref = t ^.. entries . emAt ref . nodeVal . rf

theRootCol :: Fold NavTree NavTree
theRootCol = aRootRef theRootImgCol

theRootDir :: Fold NavTree NavTree
theRootDir = aRootRef theRootImgDir

-- don't fold over the hierachy
-- but over the map containing the entries

allEntries :: Fold NavTree NavTree
allEntries =
  folding $ \ (t, _r) -> t ^.. entries . to M.keys . folded . to (t,)

-- select all entries of current ObjId and sub-ObjIds

allTrees :: Fold NavTree NavTree
allTrees = foldingTree visit
  where
    visit t ref =
      ref : nt ^. theNode
                . imgNodeRefs
                . to (visit t)
      where
        nt = (t, ref)

-- select all entries of all sub-ObjIds

allSubTrees :: Fold NavTree NavTree
allSubTrees = dropping 1 allTrees

-- select all childen of current ObjId

theChildren :: Fold NavTree NavTree
theChildren = foldingTree (curry visit)
  where
    visit nt = nt ^.. theNode . imgNodeRefs

-- select ancestors of current ObjId including the current entry

theAncestors :: Fold NavTree NavTree
theAncestors = foldingTree visit
  where
    visit t ref
      | ref == pref = [ref]
      | otherwise   = ref : visit t pref
      where
        pref = (t, ref) ^. theEntry . parentRef

-- --------------------
--
-- more complex selectors

-- select a tree by a path,
-- starting at current ObjId

treeByPath :: Path -> Fold NavTree NavTree
treeByPath = subTreesByNames . subTree
  where
    subTree :: Path -> NameFilter
    subTree p = p ^.. folded . to (==)


-- select trees by a path (given as Text) containing
-- glob style wildcards

treesByWildcard :: Text -> Fold NavTree NavTree
treesByWildcard = subTreesByNames . globTree
  where
    globTree :: Text -> NameFilter
    globTree pat = patPath ^.. folded . to match
      where
        patPath :: Path
        patPath = isoText # pat

        match :: Name -> Name -> Bool
        match p n = matchGlobPattern (p ^. isoString) (n ^. isoString)


-- select subtrees of a given depth

treesByDepth :: Int -> Fold NavTree NavTree
treesByDepth = subTreesByNames . levelTree
  where
    levelTree n = replicate n (const True)

-- the working horse:
-- select subtrees by a list of name predicates
-- whitch must match the names in a path

subTreesByNames :: NameFilter -> Fold NavTree NavTree
subTreesByNames nf0 = foldingTree (visit nf0)
  where
    visit :: NameFilter -> ImgTree -> ObjId -> [ObjId]
    visit []        _ ref = [ref]
    visit (p : nf1) t ref =
      (t, ref) ^. theEntry
                . nodeNameVal         -- select name and img node
                . filtered (p . fst)  -- filter by name predicate
                . _2                  -- select img node
                . imgNodeRefs         -- select refs contained in img node
                . to (visit nf1 t)    -- recurse


-- select trees by node predicate
-- @recurse@ controls, which subtrees are traversed
-- @include@ controls, which subtrees are part of the result

treesByNode :: (ImgNode -> Bool)
            -> (ImgNode -> Bool)
            -> Fold NavTree NavTree
treesByNode recurse include = foldingTree visit
  where
    visit t ref =
      (t, ref) ^. theNode . to f1
      where
        f1 n
          | include n = ref : down n
          | otherwise =       down n

        down n
          | recurse n = n ^. imgNodeRefs . to (visit t)
          | otherwise = mempty

-- --------------------
--
-- examples of compound queries

-- select all nodes representing images

allImgNodes :: Fold NavTree ImgNode
allImgNodes = treesByNode (const True) isIMG . theNode

-- select all ObjIds of images contained in dir nodes

allDirImgRefs :: Fold NavTree ObjId
allDirImgRefs = treesByNode ((||) <$> isDIR <*> isROOT) isIMG . _2

-- compute all paths for collection nodes

allColPaths :: Fold NavTree Path
allColPaths = treesByNode ((||) <$> isCOL <*> isROOT) isCOL . thePath


-- select all refs to sub collections of a collection node

childColRefs :: Fold NavTree NavTree
childColRefs = foldingTree (curry visit)
  where
    visit nt =
      nt ^.. theNode . theColEntries . traverse . theColColRef

-- select all names of all subcollections
-- useful for checking name clashes, when creating new collections

childColNames :: Fold NavTree Name
childColNames = childColRefs . theEntry . nodeName

-- ----------------------------------------

-- helper for constructing optics
--
-- construct a selector Fold from a
-- basic Tree selection function

foldingTree :: (ImgTree -> ObjId -> [ObjId])
            -> Fold NavTree NavTree
foldingTree visit = folding (uncurry f)
  where
    f t ref = (t,) <$> visit t ref

lensNavTree :: (ObjId -> Lens' ImgTree a)
         -> Lens' NavTree a
lensNavTree lf = lens to' fr'
  where
    to' (t, r)   = t ^. lf r
    fr' (t, r) v = (t & lf r .~ v, r)

filteredByNode :: (ImgNode -> Bool) -> Fold NavTree NavTree
filteredByNode p = filteredBy (theNode . filtered p)

{-# INLINE foldingTree    #-}
{-# INLINE lensNavTree    #-}
{-# INLINE filteredByNode #-}

-- ----------------------------------------
--
-- compute sets of ObjId(s)

allObjIds          :: NavTree -> ObjIds
allObjIds          = foldToObjIds (allTrees . _2)

allDefinedObjIds   :: NavTree -> ObjIds
allDefinedObjIds   = foldToObjIds (_1 . entries . to M.keys . folded)

allUndefinedObjIds :: NavTree -> ObjIds
allUndefinedObjIds = allObjIdsBy isempty

allImgObjIds       :: NavTree -> ObjIds
allImgObjIds       = allObjIdsBy isIMG

allColObjIds       :: NavTree -> ObjIds
allColObjIds       = allObjIdsBy isCOL

allDirObjIds       :: NavTree -> ObjIds
allDirObjIds       = allObjIdsBy isDIR

setTo :: (a -> Set b) -> Fold a b
setTo f = folding (S.toAscList . f)

-- fold helpers

foldToObjIds :: Fold NavTree ObjId -> NavTree -> ObjIds
foldToObjIds fd = foldMapOf fd singleObjId

allObjIdsBy :: (ImgNode -> Bool) -> NavTree -> ObjIds
allObjIdsBy p = foldToObjIds (allTrees . filteredByNode p . _2)

{-# INLINE allObjIds #-}
{-# INLINE allDefinedObjIds #-}
{-# INLINE allUndefinedObjIds #-}
{-# INLINE allImgObjIds #-}
{-# INLINE allColObjIds #-}
{-# INLINE allDirObjIds #-}
{-# INLINE foldToObjIds #-}
{-# INLINE allObjIdsBy #-}

-- ----------------------------------------
