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

  , Eff'ISE     -- combined effect constraints
  , Eff'ISEL
  , Eff'ISEJL
  , Eff'ISEJLT
  , Eff'ISEJLFS
  , Eff'ALL

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
  , SemISEJLT

  , SemMB

    -- * lifting functions
  , liftExcept
  , liftMaybe
  , pureMaybe
  , runMaybe
  , runMaybeEmpty

    -- * Maybe monad on top of Sem r (TODO: refactor with NonDet effect)
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
import Polysemy.NonDet
import Polysemy.Reader
import Polysemy.State
import Polysemy.Time

import Data.ImageStore (ImgStore)
import Data.Journal    (JournalP)
import Data.Prim

import Catalog.CatEnv  (CatEnv)

import Polysemy.ExecProg (ExecProg)

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

type Eff'ISE     r = ( EffIStore  r
                     , EffError   r
                     )

type Eff'ISEL    r = ( EffIStore  r
                     , EffError   r
                     , EffLogging r
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
                   )


type SemCE      r a = ( EffCatEnv r
                      ) => Sem r a

type SemE       r a = ( EffError r
                      ) => Sem r a

type SemIS      r a = ( EffIStore r
                      ) => Sem r a

type SemISE     r a = ( EffIStore r
                      , EffError  r
                      ) => Sem r a

type SemISJ     r a = ( EffIStore  r
                      , EffJournal r
                      ) => Sem r a

type SemISEJ    r a = ( EffIStore  r
                      , EffError   r
                      , EffJournal r
                      ) => Sem r a

type SemISEL    r a = ( EffIStore  r
                      , EffError   r
                      , EffLogging r
                      ) => Sem r a

type SemISEJL   r a = ( EffIStore  r
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

type SemISEJLT  r a = ( EffIStore  r
                      , EffError   r
                      , EffJournal r
                      , EffLogging r
                      , EffTime r
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
--
-- TODO: really neccesary ???

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
