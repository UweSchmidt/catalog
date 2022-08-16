{-# LANGUAGE TupleSections #-}

-- ----------------------------------------

module Data.NavTree
where

import Data.Prim
import Data.ImgNode
import Data.RefTree
import Data.ImgTree

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

curEntry :: Lens' NavTree UplNode
curEntry =
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

curNode :: Getter NavTree ImgNode
curNode = curEntry . nodeVal

-- get path for current ObjId

curPath :: Getter NavTree Path
curPath = to f
  where
    f :: NavTree -> Path
    f nt@(t, ref)
      | ref == p  = mkPath n
      | otherwise = f (t, p) `snocPath` n
      where
        p = e  ^. parentRef
        n = e  ^. nodeName
        e = nt ^. curEntry

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

curTrees :: Fold NavTree NavTree
curTrees = foldingTree visit
  where
    visit t ref =
      ref : nt ^. curNode
                . imgNodeRefs
                . to (visit t)
      where
        nt = (t, ref)

-- select all entries of all sub-ObjIds

curSubTrees :: Fold NavTree NavTree
curSubTrees = dropping 1 curTrees

-- select all childen of current ObjId

curChildren :: Fold NavTree NavTree
curChildren = foldingTree (curry visit)
  where
    visit nt = nt ^.. curNode . imgNodeRefs

-- select ancestors of current ObjId including the current entry

curAncestors :: Fold NavTree NavTree
curAncestors = foldingTree visit
  where
    visit t ref
      | ref == pref = [ref]
      | otherwise   = ref : visit t pref
      where
        pref = (t, ref) ^. curEntry . parentRef

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
      (t, ref) ^. curEntry
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
      (t, ref) ^. curNode . to f1
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
allImgNodes = treesByNode (const True) isIMG . curNode

-- select all ObjIds of images contained in dir nodes

allDirImgRefs :: Fold NavTree ObjId
allDirImgRefs = treesByNode ((||) <$> isDIR <*> isROOT) isIMG . _2

-- compute all paths for collection nodes

allColPaths :: Fold NavTree Path
allColPaths = treesByNode ((||) <$> isCOL <*> isROOT) isCOL . curPath


-- select all refs to sub collections of a collection node

childColRefs :: Fold NavTree NavTree
childColRefs = foldingTree (curry visit)
  where
    visit nt =
      nt ^.. curNode . theColEntries . traverse . theColColRef

-- select all names of all subcollections
-- useful for checking name clashes, when creating new collections

childColNames :: Fold NavTree Name
childColNames = childColRefs . curEntry . nodeName

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
filteredByNode p = filteredBy (curNode . filtered p)

{-# INLINE foldingTree    #-}
{-# INLINE lensNavTree    #-}
{-# INLINE filteredByNode #-}

-- ----------------------------------------
--
-- compute sets of ObjId(s)

allObjIds          :: NavTree -> ObjIds
allObjIds          = foldToObjIds (curTrees . _2)

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
allObjIdsBy p = foldToObjIds (curTrees . filteredByNode p . _2)

{-# INLINE allObjIds #-}
{-# INLINE allDefinedObjIds #-}
{-# INLINE allUndefinedObjIds #-}
{-# INLINE allImgObjIds #-}
{-# INLINE allColObjIds #-}
{-# INLINE allDirObjIds #-}
{-# INLINE foldToObjIds #-}
{-# INLINE allObjIdsBy #-}

-- ----------------------------------------

type Assertion r = NavTree -> Maybe r
                   -- Nothing : o.k.
                   -- Just r  : not o.k. with error value

hasRootNode      :: Assertion ImgNode
hasRootNode t    = t ^? curNode . filtered (not . isROOT)

hasRootColNode   :: Assertion ImgNode
hasRootColNode t = t ^? theRootCol . curNode . filtered (not . isCOL)

hasRootDirNode   :: Assertion ImgNode
hasRootDirNode t = t ^? theRootDir . curNode . filtered (not . isDIR)

noUndefinedIds   :: Assertion ObjIds
noUndefinedIds t = (allUndefinedObjIds t) ^. isoMaybe

noJunkInDirs :: Fold NavTree NavTree
noJunkInDirs = folding f
  where
    f t = t ^.. allEntries
              . filteredBy ( curNode
                           . filtered isDIR   -- all DIR nodes
                           )
              . filteredBy ( curChildren
                           . curNode
                           . filtered (not . ((||) <$> isDIR <*> isIMG))
                           )

allColrefJunk :: Fold NavTree (NavTree, ObjIds)
allColrefJunk =
  allEntries
  . filteredBy (curNode . filtered isCOL)
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
      t ^.. curNode
          . colNodeColRefs
          . filtered
            ( has ( setCur t
                  . curNode
                  . filtered (not . isCOL)
                  )
            )

allImgrefJunk :: Fold NavTree (NavTree, Set ImgRef)
allImgrefJunk =
  allEntries
  . filteredBy (curNode . filtered isCOL)
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
      t ^.. curNode
          . colNodeImgRefs
          . filtered
            (\ ir ->
               has ( imgref
                   . setCur t
                   . curNode
                   . filtered ( not
                              . ( (&&)
                                  <$> isIMG
                                  <*> has (theImgPart (ir ^. imgname))
                                )
                              )
                   ) ir
            )

uplinkCheck :: Fold NavTree (ObjId, ObjId)
uplinkCheck = folding f
  where
    f t =
      t ^.. curTrees
          . folding
            (\ t'@(_, r) ->
               t' ^.. curEntry
                    . filteredBy (nodeVal . filtered (not . isROOT))
                    . parentRef
                    . filtered
                      ( not . elemOf ( setCur t
                                     . curNode
                                     . imgNodeRefs
                                     ) r
                      )
                    . to (r,)
            )

-- ----------------------------------------
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
    f t0 = t0 ^.. curTrees . folding editNode
      where
        undefs = foldMapOf undefsF S.singleton t0

        editNode t@(_, r) =
          dirEdit <> colEdit
          where
            n  :: ImgNode
            n  = t ^. curNode

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

-- -----------------------------------------

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
      t ^. curNode . to (editImg <> editBlog <> editEntries)
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
      t ^. curNode . to editEntries
      where
        r = t ^. _2

        editEntries n =
          [setColEntries r $ filterColEntries noJunk ces]
          where
            ces    = n ^. theColEntries

            noJunk = colEntry'
                     (const True)
                     (not . (`S.member` crs))

-- -----------------------------------------

cleanupUplinks :: (ObjId -> ObjId -> r)
               -> Fold (ObjId, ObjId) r
cleanupUplinks repairUplink = folding f
  where
    f (ref, pref) = [repairUplink ref pref]

-- -----------------------------------------
{-
cleanupColEntries :: (ObjId -> ColEntries -> m ())
                  -> (ObjId               -> m ())
                  -> (ObjId               -> m ())
                  -> [(ObjId, ColEntry)]
                  -> NavTree
                  -> [m ()]
cleanupColEntries
  setColEntries
  clearColImg
  clearColBlog
  undefs
  t0@(t', r')
  = t0 ^.. to allColObjIds
         . folded
         . filtered (`S.member` oids)
         . folding editCol
  where
    oids :: ObjIds
    oids = undefs ^. traverse . _1 . to singleObjId

    ces :: Set ColEntry
    ces = undefs ^. traverse . _2 . to S.singleton

    editCol :: ObjId -> [m()]
    editCol r = r ^.. to (t',)
                    . curNode
                    . folding (colImgEdit <> colBlogEdit <> colEntEdit)
      where
        colImgEdit :: ImgNode -> [m ()]
        colImgEdit  = colRefEdit theColImg  clearColImg
        colBlogEdit = colRefEdit theColBlog clearColBlog

        colRefEdit theColRef cmd n
          | anyOf (theColRef . traverse)
                  (`S.member` ces)
                  n =
            [cmd r]
          | otherwise = []

        colEntEdit :: ImgNode -> [m()]
        colEntEdit r = undefined
-- -}

-- -----------------------------------------
