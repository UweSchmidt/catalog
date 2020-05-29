{-# LANGUAGE
    ConstraintKinds,
    DataKinds,
    FlexibleContexts,
    GADTs,
    OverloadedStrings,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)

------------------------------------------------------------------------------
--
-- the preliminary main module for testing
-- should be moved to an app

module Catalog.Run
  ( CatApp
  , runApp
  , runRead
  , runMody
  , runBG
  , r
  )
where

-- polysemy and polysemy-tools
import Polysemy
import Polysemy.Consume.BGQueue
import Polysemy.State.RunTMVar
import Polysemy.ExecProg

-- catalog-polysemy
import Catalog.CatEnv
import Catalog.Effects
import Catalog.Effects.CatCmd
import Catalog.Effects.CatCmd.Interpreter
import Catalog.Journal              (journalToStdout, journalToDevNull)

-- catalog
import Data.Prim
import Data.Journal                 (JournalP)
import Data.ImageStore              (ImgStore, emptyImgStore)

-- libs
import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TChan
import System.IO                    (stderr, hFlush)

import qualified Control.Exception as Ex
import qualified Data.Text         as T ()
import qualified Data.Text.IO      as T

------------------------------------------------------------------------------

type CatApp a = Sem '[ CatCmd
                     , ExecProg
                     , FileSystem
                     , Time
                     , Reader CatEnv
                     , Consume JournalP
                     , Logging
                     , Error Text
                     , State ImgStore
                     , Embed IO
                     ] a

----------------------------------------
--
-- run a single command

runApp :: ImgStore -> AppEnv -> CatApp a -> IO (ImgStore, Either Text a)
runApp ims env =
  runM
  . runState ims
  . runError @Text
  . logToStdErr
  . logWithLevel (env ^. appEnvLogLevel)
  . runLogEnvFS env

r :: CatApp a -> IO (ImgStore, Either Text a)
r = runApp emptyImgStore defaultAppEnv

----------------------------------------
--
-- run on server side: catalog reading command

runRead :: TMVar ImgStore
        -> TChan Job
        -> AppEnv
        -> CatApp a
        -> IO (Either Text a)
runRead var logQ env =
  runM
  . evalStateTMVar  var
  . runError        @Text
  . runLogging      logQ (env ^. appEnvLogLevel)
  . runLogEnvFS     env

{-# INLINE runRead #-}

--------------------
--
-- run on server side: catalog modifying command

runMody :: TMVar ImgStore
        -> TMVar ImgStore
        -> TChan Job
        -> AppEnv
        -> CatApp a
        -> IO (Either Text a)
runMody rvar mvar logQ env =
  runM' mvar
  . modifyStateTMVar rvar mvar
  . runError        @Text
  . runLogging      logQ (env ^. appEnvLogLevel)
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
      -> TChan Job
      -> AppEnv
      -> CatApp a
      -> IO ()
runBG var qu logQ env =
  runM
  . evalStateTChan var qu
  . runError       @Text
  . runLogging     logQ (env ^. appEnvLogLevel)
  . runLogEnvFS    env

--------------------
--
-- common run parts

runLogEnvFS :: (EffError r, EffIStore r, EffLogging r, Member (Embed IO) r)
            => AppEnv
            -> Sem (CatCmd
                    : ExecProg
                    : FileSystem
                    : Time
                    : Reader CatEnv
                    : Consume JournalP
                    : r
                   ) a
            -> Sem  r a
runLogEnvFS env =
  runJournal        (env ^. appEnvJournal)
  . runReader       @CatEnv (env ^. appEnvCat)
  . runOS
  . evalCatCmd

{-# INLINE runLogEnvFS #-}


runLogging :: (Member (Embed IO) r)
           => BGQueue
           -> LogLevel
           -> Sem (Logging ': r) a
           -> Sem r a
runLogging logQ lev =
  writeToBGQueue logQ
     (\ t -> do T.hPutStrLn stderr $ logMsgToText t
                hFlush stderr
     )
  . logWithLevel lev

{-# INLINE runLogging #-}


runJournal :: (Member (Embed IO) r)
           => Bool
           -> Sem (Consume JournalP ': r) a
           -> Sem r a
runJournal withJournal
  | withJournal = journalToStdout
  | otherwise   = journalToDevNull

{-# INLINE runJournal #-}

runOS :: ( Member (Embed IO) r
         , Member (Error Text) r
         , Member Logging r
         )
       => Sem (ExecProg : FileSystem : Time : r) a
       -> Sem r a
runOS =
  posixTime         ioExcToText
  . basicFileSystem ioExcToText
  . systemProcess   ioExcToText


{-# INLINE runOS #-}

------------------------------------------------------------------------------
