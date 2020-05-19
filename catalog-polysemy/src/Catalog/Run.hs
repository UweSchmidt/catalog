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
import Polysemy.State.RunTMVar
import Catalog.CatEnv
import Catalog.Effects
import Catalog.Journal (journalToStdout, journalToDevNull)

import Data.Prim
import Data.Journal    (JournalP)
import Data.ImageStore (ImgStore, emptyImgStore)

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar
import qualified Control.Exception as Ex

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

runApp :: (Sem '[Embed IO] (Either Text a) -> IO (Either Text a))
       -> (forall r a . Member (Embed IO) r => Sem (State ImgStore ': r) a -> Sem r a)
       -> AppEnv
       -> CatApp a
       -> IO (Either Text a)
runApp runM' runState' env cmd = do
  let runJournal
        | env ^. appEnvJournal = journalToStdout
        | otherwise            = journalToDevNull

  res <-
    runM'
    . runState'
    . runError        @Text
    . logToStdErr
    . logWithLevel    (env ^. appEnvLogLevel)
    . runJournal
    . runReader       @CatEnv (env ^. appEnvCat)
    . posixTime       ioExcToText
    . basicFileSystem ioExcToText
    $ cmd

  return res

----------------------------------------
--
-- run on server side: catalog reading command

runRead :: TMVar ImgStore
        -> AppEnv
        -> CatApp a
        -> IO (Either Text a)
runRead var env =
  runM
  . evalStateTMVar  var
  . runError        @Text
  . runLogEnvFS     env

{-# INLINE runRead #-}

--------------------
--
-- run on server side: catalog modifying command

runMody :: TMVar ImgStore
        -> TMVar ImgStore
        -> AppEnv
        -> CatApp a
        -> IO (Either Text a)
runMody rvar mvar env =
  runM' mvar
  . modifyStateTMVar rvar mvar
  . runError        @Text
  . runLogEnvFS     env

{-# INLINE runMody #-}

-- a save version of runM, which restores the TMVar
-- with the initial state, if an exception has been thrown
-- in the IO monad and not been caught in the cmd

runM' :: TMVar s -> Sem '[Embed IO] a -> IO a
runM' var cmd =
  bracketOnErrorTMVar (runM cmd)
  where
    -- bracketOnErrorTMVar :: TMVar s -> IO a -> IO a
    bracketOnErrorTMVar cmd' = do
      Ex.bracketOnError
        (atomically $ readTMVar var)
        (\ v -> atomically $ do
                  em <- isEmptyTMVar var
                  when em (putTMVar var v)
        )
        (\ _ -> cmd')

--------------------
--
-- common run parts

runLogEnvFS :: (EffError r, EffIStore r, Member (Embed IO) r)
            => AppEnv
            -> Sem (FileSystem : Time : Reader CatEnv
                    : Consume JournalP : Logging
                    : Consume LogMsg : r) a
            -> Sem r a
runLogEnvFS env =
  logToStdErr
  . logWithLevel    (env ^. appEnvLogLevel)
  . runJournal      (env ^. appEnvJournal)
  . runReader       @CatEnv (env ^. appEnvCat)
  . posixTime       ioExcToText
  . basicFileSystem ioExcToText

{-# INLINE runLogEnvFS #-}

runJournal :: (Member (Embed IO) r)
           => Bool -> Sem (Consume JournalP : r) a -> Sem r a
runJournal withJournal
  | withJournal = journalToStdout
  | otherwise   = journalToDevNull

{-# INLINE runJournal #-}

----------------------------------------

main' :: IO ()
main' = do
  rvar  <- newTMVarIO emptyImgStore
  mvar  <- newTMVarIO emptyImgStore

  let env  = defaultAppEnv

  let runRC = runRead rvar env
  let runMC = runMody rvar mvar env

  Right _ <- runRC $
             return ()

  Right x <- runMC $
             return ()

  return ()

------------------------------------------------------------------------

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
