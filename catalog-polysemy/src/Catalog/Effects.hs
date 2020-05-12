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

    -- action types
  , SemE
  , SemIS
  , SemISE
  , SemISJ
  , SemISEJ
  , SemISEJL

  -- lifting functions
  , liftExcept
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
import Catalog.Journal (JournalP)
import Data.Prim       (Text, isoText, (^.))
import Catalog.CatEnv  (CatEnv)

------------------------------------------------------------------------------

type EffCatEnv  r = Member (Reader CatEnv)    r
type EffError   r = Member (Error Text)       r
type EffFileSys r = Member FileSystem         r
type EffIStore  r = Member (State ImgStore)   r
type EffJournal r = Member (Consume JournalP) r
type EffLogging r = Member Logging            r

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

type SemISEJL r a = ( EffIStore  r
                    , EffError   r
                    , EffJournal r
                    , EffLogging r
                    ) => Sem r a

-- ----------------------------------------
--
-- basic Cmd combinators

liftExcept :: Except String a -> SemE r a
liftExcept cmd =
  case runExcept cmd of
    Left  msg -> throw $ msg ^. isoText
    Right res -> return res

-- ----------------------------------------
