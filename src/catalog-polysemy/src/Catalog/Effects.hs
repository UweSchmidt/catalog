------------------------------------------------------------------------------

module Catalog.Effects
  ( module Polysemy
  , module Polysemy.Consume
  , module Polysemy.Error
  , module Polysemy.FileSystem
  , module Polysemy.Logging
  , module Polysemy.NonDet
  , module Polysemy.Reader
  , module Polysemy.State
  , module Polysemy.Time

    -- * effect constraints
  , EffCatEnv
  , EffError
  , EffFileSys
  , EffIStore
  , EffJournal
  , EffLogging
  , EffNonDet
  , EffTime
  , EffExecProg
  , EffUndoHist

  , Eff'ISE     -- combined effect constraints
  , Eff'ISJ
  , Eff'ISEJ
  , Eff'ISEL
  , Eff'ISEJL
  , Eff'ISEJLT
  , Eff'ISEJLFS
  , Eff'ALL

    -- * lifting functions
  , liftExcept
  , liftMaybe
  , pureMaybe
  , runMaybe
  , runMaybeEmpty
  )
where

import Control.Monad.Trans.Except (Except, runExcept)

import Polysemy
import Polysemy.Consume
import Polysemy.Error
import Polysemy.FileSystem
import Polysemy.Logging
import Polysemy.NonDet
import Polysemy.Reader
import Polysemy.State
import Polysemy.Time

import Data.ImageStore
       ( ImgStore )

import Data.Journal
       ( JournalP )

import Data.Prim
       ( Alternative(empty)
       , Text
       , fromMaybe
       , (^.)
       , IsoText(isoText)
       )

import Catalog.CatEnv
       ( CatEnv )
import Catalog.History
       ( UndoListCmd )

import Polysemy.ExecProg
       ( ExecProg )

------------------------------------------------------------------------------

type EffCatEnv   r = Member (Reader CatEnv)    r
type EffError    r = Member (Error Text)       r
type EffFileSys  r = Member FileSystem         r
type EffIStore   r = Member (State ImgStore)   r
type EffJournal  r = Member (Consume JournalP) r
type EffLogging  r = Member Logging            r
type EffNonDet   r = Member NonDet             r
type EffTime     r = Member Time               r
type EffExecProg r = Member ExecProg           r
type EffUndoHist r = Member UndoListCmd        r

type Eff'ISE     r = ( EffIStore  r
                     , EffError   r
                     )

type Eff'ISJ     r = ( EffIStore  r
                     , EffJournal r
                     )

type Eff'ISEL    r = ( EffIStore  r
                     , EffError   r
                     , EffLogging r
                     )
type Eff'ISEJ    r = ( EffIStore  r
                     , EffError   r
                     , EffJournal r
                     )

type Eff'ISEJL   r = ( EffIStore  r
                     , EffError   r
                     , EffJournal r
                     , EffLogging r
                     )

type Eff'ISEJLFS  r = ( EffIStore  r
                      , EffError   r
                      , EffJournal r
                      , EffLogging r
                      , EffFileSys r
                      )

type Eff'ISEJLT  r = ( EffIStore  r
                     , EffError   r
                     , EffJournal r
                     , EffLogging r
                     , EffTime r
                     )

type Eff'ALL  r  = ( EffCatEnv   r
                   , EffError    r
                   , EffFileSys  r
                   , EffIStore   r
                   , EffJournal  r
                   , EffLogging  r
                   , EffTime     r
                   , EffExecProg r
                   , EffUndoHist r
                   )

-- ----------------------------------------
--
-- basic Cmd combinators

liftExcept :: EffError r => Except String a -> Sem r a
liftExcept cmd =
  case runExcept cmd of
    Left  msg -> throw $ msg ^. isoText
    Right res -> return res

-- ----------------------------------------

liftMaybe :: Member NonDet r => Sem r (Maybe a) -> Sem r a
liftMaybe cmd = cmd >>= maybe empty return
{-# INLINE liftMaybe #-}

pureMaybe :: Member NonDet r => Maybe a -> Sem r a
pureMaybe = maybe empty return
{-# INLINE pureMaybe #-}

runMaybe :: Sem (NonDet ': r) a -> Sem r (Maybe a)
runMaybe = runNonDetMaybe
{-# INLINE runMaybe #-}

runMaybeEmpty :: (Monoid a) => Sem (NonDet ': r) a -> Sem r a
runMaybeEmpty cmd = fromMaybe mempty <$> runMaybe cmd

------------------------------------------------------------------------------
