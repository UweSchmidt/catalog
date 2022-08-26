{-# LANGUAGE TupleSections #-}
module Data.ImgTree.RefTree
       ( RefTree
       , UpLink(..)
       , DirTree
       , rootRef
--       , emptyUplNode
       , entries
       , entryAt
       , emAt
--       , theNode'
       , parentRef
       , nodeName
       , nodeVal
       , nodeNameVal
       , refPath
       , refObjIdPath
       , refInTree
       , mkDirRoot
       , isRootRef
       , upTree
       , isDirRoot
       , mkDirNode
       , remDirNode
       , lookupDirPath
       , mapRefTree
       )
where

import Control.Monad.Except
       ( Except
       , throwError
       )
import Data.Prim

import qualified Data.Aeson      as J
import qualified Data.Map.Strict as M

-- ----------------------------------------

-- A RefTree has a root reference and a map of refs to nodes
--
-- kind signature
-- type RefTree :: (* -> *) -> * -> *

data RefTree node ref = RT !ref !(Map ref (node ref))

deriving
  instance (Show ref, Show (node ref)) =>
           Show (RefTree node ref)

instance (ToJSON (node ref), ToJSON ref) =>
         ToJSON (RefTree node ref)
  where
    toJSON (RT r m) = J.object
      [ "rootRef"   J..= r
      , "entries"   J..= M.toList m  -- entries are ordered in .json doc
      ]                              -- better readability with pathid's

instance (Ord ref, FromJSON (node ref), FromJSON ref) =>
         FromJSON (RefTree node ref)
  where
    parseJSON = J.withObject "RefTree" $ \ o ->
      RT
      <$> o J..: "rootRef"
      <*> (M.fromList <$> o J..: "entries")

rootRef :: Lens' (RefTree node ref) ref
rootRef k (RT r m) = (`RT` m) <$> k r
{-# INLINE rootRef #-}

entries :: Lens' (RefTree node ref) (Map ref (node ref))
entries k (RT r m) = RT r <$> k m
{-# INLINE entries #-}

-- get/insert/remove entry in tree map
--
-- indexing is a total function

emAt :: ( Monoid (node ref), IsEmpty (node ref)
        , Ord ref
        )
     => ref -> Lens' (Map ref (node ref)) (node ref)
emAt ref = lens g s
  where
    g em = M.findWithDefault mempty ref em
    s em v
      | isempty v = M.delete ref   em
      | otherwise = M.insert ref v em
{-# INLINE emAt #-}

entryAt :: ( Monoid (node ref), IsEmpty (node ref)
           , Ord ref
           )
        => ref -> Lens' (RefTree node ref) (node ref)
entryAt r = entries . emAt r
{-# INLINE entryAt #-}

-- theNode' :: (Ord ref) => ref -> Traversal' (RefTree node ref) (node ref)
-- theNode' r = entryAt r . traverse
-- {-# INLINE theNode' #-}

-- ----------------------------------------

-- exchange the keys of a reftree
--
-- almost a functor, f must be an injective function

mapRefTree :: (Functor node, Ord ref')
           => (ref -> ref')      -- ^ the injective function mapping the refs
           -> RefTree node ref   -- ^ the input tree
           -> RefTree node ref'  -- ^ result tree
mapRefTree f (RT r t) =
  RT (f r) ( M.foldrWithKey'
             (\ k !v !acc -> M.insert (f k) (fmap f v) acc)
             M.empty
             t
           )

-- ----------------------------------------

-- An UpLink takes a node and adds two components,
-- .1 a ref to the parent node,
-- .2 second a name.
--
-- The root node has the root itself as parent ref
--
-- kind signature
-- type UpLink :: (* -> *) -> * -> *

data UpLink node ref = UL !ref !Name !(node ref)

deriving instance (Show ref, Show (node ref)) => Show (UpLink  node ref)

instance Functor node =>
         Functor (UpLink node)
  where
    fmap f (UL x n t) = UL (f x) n (fmap f t)
    {-# INLINE fmap #-}

instance (ToJSON (node ref), ToJSON ref) =>
         ToJSON (UpLink node ref)
  where
    toJSON (UL r n v) = J.object
      [ "parentRef" J..= r
      , "nodeName"  J..= n
      , "nodeVal"   J..= v
      ]

instance (FromJSON (node ref), FromJSON ref) =>
         FromJSON (UpLink node ref)
  where
    parseJSON = J.withObject "UpLink" $ \ o ->
      UL
      <$> o J..: "parentRef"
      <*> o J..: "nodeName"
      <*> o J..: "nodeVal"

instance IsEmpty (node ref) =>
         IsEmpty (UpLink node ref)
  where
    isempty (UL _r _n v) = isempty v
    {-# INLINE isempty #-}

instance IsEmpty (node ref) =>
         Semigroup (UpLink node ref)
  where
    un1 <> un2
      | isempty un1 = un2
      | otherwise   = un1
    {-# INLINE (<>) #-}

instance ( Monoid ref
         , Monoid (node ref), IsEmpty (node ref)
         ) =>
         Monoid (UpLink node ref)
  where
    mempty = UL mempty mempty mempty
    {-# INLINE mempty #-}

-- lenses

parentRef :: Lens' (UpLink node ref) ref
parentRef k (UL r n v) = fmap (\ new -> UL new n v) (k r)
{-# INLINE parentRef #-}

nodeName :: Lens' (UpLink node ref) Name
nodeName k (UL r n v) = fmap (\ new -> UL r new v) (k n)
{-# INLINE nodeName #-}

nodeVal :: Lens' (UpLink node ref) (node ref)
nodeVal k (UL r n v) = UL r n <$> k v
{-# INLINE nodeVal #-}

nodeNameVal :: Lens' (UpLink node ref) (Name, node ref)
nodeNameVal k (UL r n v) = fmap (\ (newn, newv) -> UL r newn newv) (k (n, v))
{-# INLINE nodeNameVal #-}

-- ----------------------------------------
--
-- a dir tree is a ref tree with a ref to
-- the parent node,
-- this enables navigation towards root (unix: ..)
-- and each node gets a name, not only a value
--
-- kind signature
-- type DirTree :: (* -> *) -> * -> *

type DirTree  node ref = RefTree (UpLink node) ref

-- ----------------------------------------
--
-- access and modification

-- transform a ref into the path from root to node

{- old version
refPath :: forall ref node
           . ( Ord ref, Monoid ref
             , Monoid (node ref), IsEmpty (node ref)
             )
        => ref -> DirTree node ref -> Path
refPath r00 t =
  -- in case of an error (a ref was not found)
  -- the empty path is returned
  fromMaybe mempty $ refPath' r00
  where
    refPath' ::  (Monoid ref, Monoid (node ref)) => ref -> Maybe Path
    refPath' r0 = do
      n0 <- t ^? entryAt r0
      path r0 n0
      where

        -- with the new Path impl
        -- conversion to Path is more efficient
        -- by using snocPath instead of consPath (old version)

        path :: Monoid ref => ref -> UpLink node ref -> Maybe Path
        path r n
          | par == r   = return (n ^. nodeName . to mkPath)

          | otherwise  = do
              n'     <- t ^? entryAt par
              res'   <- path par n'
              return (res' `snocPath` n ^. nodeName)

          where
            par = n ^. parentRef
{-# INLINE refPath #-}
-- -}
-- {- new version
refPath :: ( Eq ref, Ord ref, IsEmpty ref, Monoid ref
           , Monoid (node ref), IsEmpty (node ref)
           )
        => ref -> DirTree node ref -> Path
refPath r0 t
  = go r0
  where
    go ref =
      case upTree t ref of
        Nothing -> mkPath n
        Just p  -> go p `snocPath` n
      where
        n = t ^. entryAt ref . nodeName
{-# INLINE refPath #-}
-- -}

-- compute all ancestors of a ref
-- list is never empty,
-- head is the ref itself, last is the root
{- old
refObjIdPath :: ( Ord ref, Show ref, Monoid ref
                , Monoid (node ref), IsEmpty (node ref)
                )
             => ref -> DirTree node ref -> [ref]
refObjIdPath r0 t =
  parentIds $ Just r0
  where
    parentIds Nothing  = []
    parentIds (Just r)
      | isRoot mp = [r]
      | otherwise = r : parentIds mp
      where
        mp     = t ^? entryAt r . parentRef

        isRoot Nothing  = True
        isRoot (Just p) = p == r
-- -}

refObjIdPath :: ( Ord ref, Show ref, IsEmpty ref, Monoid ref
                , Monoid (node ref), IsEmpty (node ref)
                )
             => ref -> DirTree node ref -> [ref]
refObjIdPath r0 t =
  go r0
  where
    go ref
      = case upTree t ref of
          Nothing -> ref : []
          Just p  -> ref : go p

{-# INLINE refObjIdPath #-}

-- test whether a ref @r@ is a part of the tree given by ref @p@
refInTree :: ( Ord ref, Show ref, IsEmpty ref, Monoid ref
             , Monoid (node ref), IsEmpty (node ref)
             )
          => ref -> ref -> DirTree node ref -> Bool
refInTree r p t =
  r `elem` tail (refObjIdPath p t)

-- --------------------
--
-- | create the root of a DirTree.
--
-- It's the only node where the parent ref is mempty (old: equals the ref).
-- The generation of the reference is supplied by a conversion from path to ref

mkDirRoot :: Monoid ref
          => (Path -> ref) -> Name -> node ref -> DirTree node ref
mkDirRoot genRef n v =
--  RT r (M.singleton r (UL mempty n v))   -- null value
    RT r (M.singleton r (UL r n v))        -- cyclic reference
  where
    r = genRef $ mkPath n
{-# INLINE mkDirRoot #-}

-- --------------------

upTree :: ( Ord ref, IsEmpty ref, Monoid ref
          , Monoid (node ref), IsEmpty (node ref)
          )
       => DirTree node ref -> ref -> Maybe ref
upTree t r
  | isRootRef p r = Nothing
  | otherwise     = Just p
  where
    p = t ^. entryAt r . parentRef

-- the only function, which knows how
-- the root is represented

isRootRef :: (Eq ref, IsEmpty ref) => ref -> ref -> Bool
isRootRef parent ref =
  isempty parent  -- new
  ||
  parent == ref          -- old

isDirRoot :: ( Ord ref, Show ref, IsEmpty ref, Monoid ref
             , Monoid (node ref), IsEmpty (node ref)
             )
          => ref -> DirTree node ref -> Bool
isDirRoot r t = isNothing $ upTree t r
{-# INLINE isDirRoot #-}

-- --------------------
--
-- lookup a (ref, nodeval) by a path
--
-- check whether a corresponding node is there
-- and return ref and node value

lookupDirPath :: ( Ord ref, Show ref, Monoid ref
                 , Monoid (node ref), IsEmpty (node ref)
                 )
              => (Path -> ref)         -- ^ conversion path to ref
              -> Path                  -- ^ the path
              -> DirTree node ref      -- ^ the tree
              -> Maybe (ref, node ref) -- ^ the ref and the node value
lookupDirPath genRef p t =
  (i',) <$> t ^? entryAt i' . filtered isn'tempty . nodeVal
  where
    i' = genRef p
{-# INLINE lookupDirPath #-}



-- | create a new entry with a new ref and modify the parent node
-- to store the new ref in its node value

mkDirNode :: ( Ord ref, Show ref, IsEmpty ref, Monoid ref
             , Monoid (node ref), IsEmpty (node ref)
             )
          => (Path -> ref)                   -- ^ ref generator
          -> (node ref -> Bool)              -- ^ parent node editable?
          -> (ref -> node ref -> node ref)   -- ^ edit value of parent node
          -> Name                            -- ^ name of the node
          -> ref                             -- ^ parent node
          -> node ref                        -- ^ node value
          -> DirTree node ref                -- ^ tree
          -> Except String
                    (ref, DirTree node ref)  -- ^ new ref and modified tree

mkDirNode genRef isParentDir updateParent n p v t = do
  -- check entry already there
  when (has (entryAt r . filtered isn'tempty) t) $
    throwError $ "mkDirNode: entry already exists: " ++ show rp

  -- check parent, must be a dir
  -- unless (t ^. theNodeVal p . to isParentDir) $
  unless (has (entryAt p . nodeVal . filtered isParentDir) t) $
    throwError $ "mkDirNode: parent node not a dir: " ++ show pp

  return
    -- insert new node with name and uplink
    -- and add ref into parent
    ( r
    , t & entryAt r           .~ UL p n v
        & entryAt p . nodeVal %~ updateParent r
    )
    where
      pp = refPath p t        -- get path of parent,
      rp = pp `snocPath` n    -- append name and
      r  = genRef rp          -- make reference

remDirNode :: ( Ord ref, Show ref, Show ref, IsEmpty ref, Monoid ref
              , Monoid (node ref), IsEmpty (node ref)
              )
           => (node ref -> Bool)             -- ^ node removable?
           -> (ref -> node ref -> node ref)  -- ^ update the parent node
           -> ref                            -- ^ node to be removed
           -> DirTree node ref               -- ^ input tree
           -> Except String
                     (DirTree node ref)      -- ^ new tree

remDirNode removable updateParent r t = do
  -- check whether node exists?
  unless (has (entryAt r . filtered isn'tempty) t) $
    throwError $ "remDirNode: ref doesn't exist: " <> show r

  -- root can't be removed
  when (r `isDirRoot` t) $
    throwError "remDirNode: root ref can't be removed"

  -- node allowed to be removed (application specific check)?
  unless (has (entryAt r . nodeVal . filtered removable) t) $
    throwError $ "remDirNode: node value not removable, entry: "
                 <> show (refPath r t)

  case t ^. entryAt r of
    n | isn'tempty n ->
          return
          --  ^ remove ref from parent
          --  ^ remove node from map
          (t & entryAt (n ^. parentRef) . nodeVal %~ updateParent r
             & entryAt r                          .~ mempty
          )

      | otherwise ->
          throwError $ "remDirNode: parent node doesn't exist " <> show r

-- ----------------------------------------
