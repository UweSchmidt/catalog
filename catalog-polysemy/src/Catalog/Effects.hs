{-# LANGUAGE
    ConstraintKinds,
    DataKinds,
    FlexibleContexts,
    GADTs,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- {-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------

module Catalog.Effects
  ( module Polysemy
  , module Polysemy.Consume
  , module Polysemy.Error
  , module Polysemy.FileSystem
  , module Polysemy.Logging
  , module Polysemy.Reader
  , module Polysemy.State

    -- effect constraints
  , EffCatEnv
  , EffError
  , EffFileSys
  , EffIStore
  , EffJournal
  , EffLogging

    -- * action types
  , SemCE
  , SemE
  , SemIS
  , SemISE
  , SemISJ
  , SemISEJ
  , SemISEL
  , SemISEJL
  , SemISEJLFS

  , SemMB

    -- * lifting functions
  , liftExcept

    -- * Maybe monad on top of Sem r
  , pureMB
  , failMB
  , liftMB
  , bindMB
  , filterMB
  )
where

import Control.Monad.Trans.Except (Except, runExcept)

import Polysemy
import Polysemy.Consume
import Polysemy.Error
import Polysemy.FileSystem
import Polysemy.Logging
import Polysemy.Reader
import Polysemy.State

import Data.ImageStore (ImgStore)
import Data.Journal    (JournalP)
import Data.Prim       (Text, isoText, (^.))
import Catalog.CatEnv  (CatEnv)

------------------------------------------------------------------------------

type EffCatEnv  r = Member (Reader CatEnv)    r
type EffError   r = Member (Error Text)       r
type EffFileSys r = Member FileSystem         r
type EffIStore  r = Member (State ImgStore)   r
type EffJournal r = Member (Consume JournalP) r
type EffLogging r = Member Logging            r

type SemCE    r a = ( EffCatEnv r
                    ) => Sem r a

type SemE     r a = ( EffError r
                    ) => Sem r a

type SemIS    r a = ( EffIStore r
                    ) => Sem r a

type SemISE   r a = ( EffIStore r
                    , EffError  r
                    ) => Sem r a

type SemISJ   r a = ( EffIStore  r
                    , EffJournal r
                    ) => Sem r a

type SemISEJ  r a = ( EffIStore  r
                    , EffError   r
                    , EffJournal r
                    ) => Sem r a

type SemISEL  r a = ( EffIStore  r
                    , EffError   r
                    , EffLogging r
                    ) => Sem r a

type SemISEJL r a = ( EffIStore  r
                    , EffError   r
                    , EffJournal r
                    , EffLogging r
                    ) => Sem r a

type SemISEJLFS r a = ( EffIStore  r
                      , EffError   r
                      , EffJournal r
                      , EffLogging r
                      , EffFileSys r
                      ) => Sem r a

type SemMB r a = Sem r (Maybe a)

-- ----------------------------------------
--
-- basic Cmd combinators

liftExcept :: Except String a -> SemE r a
liftExcept cmd =
  case runExcept cmd of
    Left  msg -> throw $ msg ^. isoText
    Right res -> return res

-- ----------------------------------------

pureMB :: a -> Sem r (Maybe a)
pureMB x = pure (Just x)

failMB :: Sem r (Maybe a)
failMB = pure Nothing

liftMB :: Maybe a -> Sem r (Maybe a)
liftMB mx = pure mx

bindMB :: Sem r (Maybe a) -> (a -> Sem r (Maybe b)) -> Sem r (Maybe b)
bindMB m f = do
  ma <- m
  case ma of
    Nothing -> return Nothing
    Just x  -> f x

filterMB :: (a -> Sem r Bool) -> Sem r (Maybe a) -> Sem r (Maybe a)
filterMB mp ma = do
  ma `bindMB`
    (\ x -> do b <- mp x
               return $
                 if b
                 then Just x
                 else Nothing
    )

------------------------------------------------------------------------------
