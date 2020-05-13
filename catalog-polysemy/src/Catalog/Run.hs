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

module Catalog.Run
where

import Polysemy.NonDet

import Catalog.CatEnv
import Catalog.Effects
import Catalog.Journal (journalToStdout, journalToDevNull)

import Data.Prim
import Data.Journal    (JournalP)
import Data.ImageStore (ImgStore, emptyImgStore)

------------------------------------------------------------------------------

type CatApp a = Sem '[ FileSystem
                     , Time
                     , Reader CatEnv
                     , Consume JournalP
                     , Logging
                     , Consume LogMsg
                     , Error Text
                     , State ImgStore
                     , Embed IO
                     ] a

runApp :: AppEnv -> CatApp a -> IO a
runApp env cmd = do
  let runJournal
        | env ^. appEnvJournal = journalToStdout
        | otherwise            = journalToDevNull

  (_imgStore, Right res) <-
    runM
    . runState        @ImgStore emptyImgStore
    . runError        @Text
    . logToStdErr
    . logWithLevel    (env ^. appEnvLogLevel)
    . runJournal
    . runReader       @CatEnv (env ^. appEnvCat)
    . posixTime       ioExcToText
    . basicFileSystem ioExcToText
    $ cmd

  return res

type Test a = Sem '[ NonDet, Embed IO] a

runTest :: Test a -> IO (Maybe a)
runTest cmd = do
  runM
  . runNonDetMaybe
  $ cmd

c1 :: Test ()
c1 = embed $ putStrLn "emil"

c2 :: Test Int
c2 = do
  c1
  return 42

c3 :: Int -> Test Int
c3 i = do
  if i == 44
    then return i
    else empty

c4 :: Test (Maybe Int)
c4 = c1 >> c2 >>= return . Just

liftMaybe :: Member NonDet r => Sem r (Maybe Int) -> Sem r Int
liftMaybe cmd = do
  mx <- cmd
  maybe empty (return . (1+)) mx

------------------------------------------------------------------------------
