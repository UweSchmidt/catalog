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
--
-- the preliminary main module for testing
-- should be moved to an app

module Catalog.Run
where

-- polysemy and polysemy-tools
import Polysemy.NonDet
import Polysemy.State.RunTMVar
import System.ExecProg

-- catalog-polysemy
import Catalog.CatEnv
import Catalog.Effects
import Catalog.Effects.CatCmd
import Catalog.Effects.CatCmd.Interpreter
import Catalog.Journal (journalToStdout, journalToDevNull)

-- catalog
import Data.Prim
import Data.Journal    (JournalP)
import Data.ImageStore (ImgStore, emptyImgStore)

-- libs
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TChan
import Control.Monad.IO.Class (liftIO)
import qualified Control.Exception as Ex

-- servant
import Servant(throwError)
import Servant.Server

------------------------------------------------------------------------------

type CatApp a = Sem '[ CatCmd
                     , ExecProg
                     , FileSystem
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
       -> (forall r b . Member (Embed IO) r => Sem (State ImgStore ': r) b -> Sem r b)
       -> AppEnv
       -> CatApp a
       -> IO (Either Text a)
runApp runM'' runState' env cmd = do
  let runJournal'
        | env ^. appEnvJournal = journalToStdout
        | otherwise            = journalToDevNull

  res <-
    runM''
    . runState'
    . runError        @Text
    . logToStdErr
    . logWithLevel    (env ^. appEnvLogLevel)
    . runJournal'
    . runReader       @CatEnv (env ^. appEnvCat)
    . posixTime       ioExcToText
    . basicFileSystem ioExcToText
    . systemProcess   ioExcToText
    . evalCatCmd
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
                  when em (putTMVar var v)   -- refill empty TMVar
        )
        (\ _ -> cmd')

--------------------

runBG :: TMVar ImgStore
      -> TChan Job
      -> AppEnv
      -> CatApp a
      -> IO ()
runBG var qu env =
  runM
  . evalStateTChan var qu
  . runError       @Text
  . runLogEnvFS    env

--------------------
--
-- common run parts

runLogEnvFS :: (EffError r, EffIStore r, Member (Embed IO) r)
            => AppEnv
            -> Sem (CatCmd
                    : ExecProg
                    : FileSystem
                    : Time
                    : Reader CatEnv
                    : Consume JournalP
                    : Logging
                    : Consume LogMsg
                    : r
                   ) a
            -> Sem r a
runLogEnvFS env =
  logToStdErr
  . logWithLevel    (env ^. appEnvLogLevel)
  . runJournal      (env ^. appEnvJournal)
  . runReader       @CatEnv (env ^. appEnvCat)
  . posixTime       ioExcToText
  . basicFileSystem ioExcToText
  . systemProcess   ioExcToText
  . evalCatCmd

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
  qu    <- createJobQueue

  let env  = defaultAppEnv

  let runRC :: CatApp a -> Handler a
      runRC = ioeither2Handler . runRead rvar env

  let runMC :: CatApp a -> Handler a
      runMC = ioeither2Handler . runMody rvar mvar env

  let runBQ :: CatApp a -> Handler ()
      runBQ = liftIO . runBG rvar qu env

  Right _ <- runHandler $ runRC $
             return ()

  Right _ <- runHandler $ runMC $
             return ()

  _       <- runHandler $ runBQ $
             return ()
  return ()

ioeither2Handler :: IO (Either Text a) -> Handler a
ioeither2Handler cmd = do
  res <- liftIO cmd
  either raise500 return res
  where
    raise500 :: Text -> Handler a
    raise500 msg =
      throwError $ err500 { errBody = msg ^. isoString . from isoString }


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
