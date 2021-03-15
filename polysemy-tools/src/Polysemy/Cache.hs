{-# LANGUAGE
    DataKinds,
    FlexibleContexts,
    GADTs,
    OverloadedStrings,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TemplateHaskell,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)

{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Cache
  ( -- Effect
    Cache(..)

    -- * Actions
  , lookupCache
  , putCache
  , getCache

    -- * Interpretations
  , runCache

    -- * aux types and functions
  , CacheTable
  , emptyCache
  )
where

import Polysemy
-- import Polysemy.Error
-- import Polysemy.EmbedExc
import Polysemy.State

import qualified Data.Map as M

------------------------------------------------------------------------------

type CacheTable k v = M.Map k v

data Cache k v m a where
  PutCache    :: [(k, v)] -> Cache k v m ()
  GetCache    :: Cache k v m [(k, v)]
  LookupCache :: k -> Cache k v m (Maybe v)

makeSem ''Cache

------------------------------------------------------------------------------

emptyCache :: M.Map k v
emptyCache = M.empty

runCache' :: forall k v r a
           . ( Ord k
             , Member (State (CacheTable k v)) r
             )
          => (k -> Sem r (Maybe v))
          -> Sem (Cache k v : r) a
          -> Sem r a
runCache' f =
  interpret $
  \ c -> case c of
    PutCache kvs -> do
      put $ M.fromList kvs

    GetCache -> do
      M.toList <$> get

    LookupCache k -> do
      m <- get
      case M.lookup k m of
        Just v -> return (Just v)
        Nothing -> do
          mv <- f k
          maybe
            (return ())
            (\ v -> put (M.insert k v m))
            mv
          return mv

runCache :: Ord k
         => (k -> Sem r (Maybe v))
         -> Sem (Cache k v : r) a -> Sem r a
runCache f =
  evalState emptyCache
  . runCache' (\ k -> raise (f k))  -- lift f into (State (CacheTable k v) : r)
  . raiseUnder

------------------------------------------------------------------------------
