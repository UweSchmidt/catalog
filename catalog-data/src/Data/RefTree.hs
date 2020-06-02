{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.RefTree
       ( RefTree
       , UpLink
       , DirTree
       , rootRef
       , entries
       , entryAt
       , theNode'
       , parentRef
       , nodeName
       , nodeVal
       , refPath
       , refObjIdPath
       , refInTree
       , mkDirRoot
       , isDirRoot
       , mkDirNode
       , remDirNode
       , lookupDirPath
       , mapRefTree
       )
where

import           Control.Monad.Except
import           Data.Prim
import qualified Data.Aeson      as J
import qualified Data.Map.Strict as M

-- ----------------------------------------

-- A RefTree has a root reference and a map of refs to nodes

data RefTree node ref = RT !ref !(Map ref (node ref))

deriving instance (Show ref, Show (n ref)) => Show (RefTree n ref)

instance (ToJSON (node ref), ToJSON ref) => ToJSON (RefTree node ref)
  where
    toJSON (RT r m) = J.object
      [ "rootRef"   J..= r
      , "entries"   J..= M.toList m
      ]

instance (Ord ref, FromJSON (node ref), FromJSON ref) => FromJSON (RefTree node ref)
  where
    parseJSON = J.withObject "RefTree" $ \ o ->
      RT
      <$> o J..: "rootRef"
      <*> (M.fromList <$> o J..: "entries")

rootRef :: Lens' (RefTree node ref) ref
rootRef k (RT r m) = (\ new -> RT new m) <$> k r
{-# INLINE rootRef #-}

entries :: Lens' (RefTree node ref) (Map ref (node ref))
entries k (RT r m) = (\ new -> RT r new) <$> k m
{-# INLINE entries #-}

entryAt :: (Ord ref) => ref -> Lens' (RefTree node ref) (Maybe (node ref))
entryAt r = entries . at r
{-# INLINE entryAt #-}

theNode' :: (Ord ref) => ref -> Traversal' (RefTree node ref) (node ref)
theNode' r = entryAt r . traverse
{-# INLINE theNode' #-}

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

data UpLink node ref = UL !ref !Name !(node ref)

deriving instance (Show ref, Show (node ref)) => Show (UpLink  node ref)

instance Functor node => Functor (UpLink node) where
  fmap f (UL x n t) = UL (f x) n (fmap f t)

instance (ToJSON (node ref), ToJSON ref) => ToJSON (UpLink node ref)
  where
    toJSON (UL r n v) = J.object
      [ "parentRef" J..= r
      , "nodeName"  J..= n
      , "nodeVal"   J..= v
      ]

instance (FromJSON (node ref), FromJSON ref) => FromJSON (UpLink node ref)
  where
    parseJSON = J.withObject "UpLink" $ \ o ->
      UL
      <$> o J..: "parentRef"
      <*> o J..: "nodeName"
      <*> o J..: "nodeVal"

parentRef :: Lens' (UpLink node ref) ref
parentRef k (UL r n v) = fmap (\ new -> UL new n v) (k r)
{-# INLINE parentRef #-}

nodeName :: Lens' (UpLink node ref) Name
nodeName k (UL r n v) = fmap (\ new -> UL r new v) (k n)
{-# INLINE nodeName #-}

nodeVal :: Lens' (UpLink node ref) (node ref)
nodeVal k (UL r n v) = fmap (\ new -> UL r n new) (k v)
{-# INLINE nodeVal #-}

-- ----------------------------------------
--
-- a dir tree is a ref tree with a ref to
-- the parent node,
-- this enables navigation towards root (unix: ..)
-- and each node gets a name, not only a value

type DirTree node ref = RefTree (UpLink node) ref

-- lenses

-- ----------------------------------------
--
-- access and modification

-- transform a ref into the path from root to node

refPath :: forall ref node . (Ord ref) => ref -> DirTree node ref -> Path
refPath r00 t =
  fromMaybe mempty $ refPath' r00
  where
    refPath' :: ref -> Maybe Path
    refPath' r0 = do
      let theName'   r = t ^? entryAt r . traverse . nodeName
      let theParent' r = t ^? entryAt r . traverse . parentRef

      let path ref acc = do
            par <- theParent' ref
            if par == ref
              then return acc
              else do
                   acc' <- (`consPath` acc) <$> theName' par
                   path par acc'

      acc0 <- mkPath <$> theName' r0
      path r0 acc0

{-# INLINE refPath #-}

-- compute all ancestors of a ref
-- list is never empty,
-- head is the ref itself, last is the root

refObjIdPath :: (Ord ref, Show ref) => ref -> DirTree node ref -> [ref]
refObjIdPath r0 t =
  parentIds $ Just r0
  where
    parentIds Nothing  = []
    parentIds (Just r)
      | isRoot mp = r : []
      | otherwise = r : parentIds mp
      where
        mp     = t ^? entryAt r . traverse . parentRef

        isRoot Nothing  = True
        isRoot (Just p) = p == r

-- test whether a ref @r@ is a part of the tree given by ref @p@
refInTree :: (Ord ref, Show ref) => ref -> ref -> DirTree node ref -> Bool
refInTree r p t =
  r `elem` tail (refObjIdPath p t)

-- | create the root of a DirTree.
--
-- It's the only node where the parent ref equals the ref.
-- The generation of the reference is supplied by a conversion from path to ref

mkDirRoot :: (Path -> ref) -> Name -> node ref -> DirTree node ref
mkDirRoot genRef n v
  = RT r (M.singleton r (UL r n v))
  where
    r = genRef $ mkPath n

isDirRoot :: (Ord ref, Show ref) => ref -> DirTree node ref -> Bool
isDirRoot r t =
  t ^? entryAt r . traverse . parentRef == Just r
{-# INLINE isDirRoot #-}

-- lookup a (ref, nodeval) by a path
--
-- check whether a corresponding node is there
-- and return ref and node value

lookupDirPath :: (Ord ref)
              => (Path -> ref)         -- ^ conversion path to ref
              -> Path                  -- ^ the path
              -> DirTree node ref      -- ^ the tree
              -> Maybe (ref, node ref) -- ^ the ref and the node value
lookupDirPath genRef p t =
   (\ v -> (i', v)) <$> (t ^? entryAt i' . _Just . nodeVal)
  where
    i' = genRef p
{-# INLINE lookupDirPath #-}



-- | create a new entry with a new ref and modify the parent node
-- to store the new ref in its node value

mkDirNode :: (Ord ref, Show ref)
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
  when (has (entryAt r . _Just) t) $
    throwError $ "mkDirNode: entry already exists: " ++ show rp

  -- check parent, must be a dir
  -- unless (t ^. theNodeVal p . to isParentDir) $
  unless (has (entryAt p . traverse . nodeVal . filtered isParentDir) t) $
    throwError $ "mkDirNode: parent node not a dir: " ++ show pp

  return
    -- insert new node with name and uplink
    -- and add ref into parent
    ( r
    , t & entryAt r                      .~ Just (UL p n v)
        & entryAt p . traverse . nodeVal %~ updateParent r
    )
    where
      pp = refPath p t        -- get path of parent,
      rp = pp `snocPath` n    -- append name and
      r  = genRef rp          -- make reference

remDirNode :: (Ord ref, Show ref)
           => (node ref -> Bool)             -- ^ node removable?
           -> (ref -> node ref -> node ref)  -- ^ update the parent node
           -> ref                            -- ^ node to be removed
           -> DirTree node ref               -- ^ input tree
           -> Except String
                     (DirTree node ref)      -- ^ new tree

remDirNode removable updateParent r t = do
  -- check whether node exists?
  unless (has (entryAt r) t) $
    throwError $ "remDirNode: ref doesn't exist: " <> show r

  -- root can't be removed
  when (r `isDirRoot` t) $
    throwError "remDirNode: root ref can't be removed"

  -- node allowed to be removed (application specific check)?
  unless (has (entryAt r . traverse . nodeVal . filtered removable) t) $
    throwError $ "remDirNode: node value not removable, entry: "
                 <> show (refPath r t)

  case t ^? entryAt r . traverse . parentRef of
    Just p ->
      return
        -- ^ remove ref from parent
        -- ^ remove node from map
        (t & entryAt p . traverse . nodeVal %~ updateParent r
           & entryAt r                      .~ Nothing
        )
    Nothing ->
      throwError $ "remDirNode: parent node doesn't exist " <> show r

-- ----------------------------------------
