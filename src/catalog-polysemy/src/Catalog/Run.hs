------------------------------------------------------------------------------

module Catalog.Run
  ( CatApp
  , JournalHandle
  , runApp
  , runRead
  , runMody
  , runBG
  , runLogQ
  , r
  , openJournal
  )
where

-- polysemy and polysemy-tools
import Polysemy
       ( Embed
       , Member
       , Sem
       , runM
       )
import Polysemy.Consume.BGQueue
       ( writeToBGQueue
       , BGQueue
       )
import Polysemy.State
       ( State
       , runStateIORef
       , runState
       )
import Polysemy.State.RunTMVar
       ( createJobQueue
       , evalStateTChan
       , evalStateTMVar
       , modifyStateTMVar
       , Job
       )
import Polysemy.ExecProg
       ( ExecProg
       , systemProcess
       )
import Polysemy.Logging
       ( LogMsg(logMsgToText)
       , LogLevel
       , Logging
       , logWithLevel
       , logToStdErr
       , logToBGQueue
       , log'info
       )
import Polysemy.FileSystem
       ( TextPath )

-- catalog-polysemy
import Catalog.CatEnv
       ( CatEnv
       , AppEnv
       , appEnvCat
       , appEnvLogLevel
       , appEnvJournal
       , defaultAppEnv
       )
import Catalog.Effects
       ( Consume
       , Time
       , FileSystem
       , Reader
       , EffLogging
       , EffIStore
       , EffError
       , Error
       , posixTime
       , basicFileSystem
       , ioExcToText
       , runReader
       , runError
       )
import Catalog.Effects.CatCmd
       ( CatCmd )

import Catalog.Effects.CatCmd.Interpreter
       ( evalCatCmd )

import Catalog.History
       ( UndoHistory
       , UndoListCmd
       , undoListNoop
       , undoListWithState
       )
import Catalog.Journal
       ( journalToBGQueue
       , journalToHandle
       , journalToDevNull
       )

-- catalog

import Data.Prim
import Data.ImgTree  -- for testing
import Data.NavTree  -- for testing

import Data.Journal
       ( JournalP )

import Data.ImageStore
       ( ImgStore
       , emptyImgStore
       )

-- libs
import Control.Concurrent.STM
       ( atomically )

import Control.Concurrent.STM.TMVar
       ( TMVar
       , isEmptyTMVar
       , putTMVar
       , readTMVar
       )
import Control.Concurrent.STM.TChan
       ( TChan )

import Data.IORef
       ( IORef )

import System.IO
       ( Handle
       , hFlush
       , stdout
       , stderr
       , IOMode(WriteMode)
       , openFile
       )


import qualified Control.Exception as Ex
import qualified Data.Text         as T ()
import qualified Data.Text.IO      as T

------------------------------------------------------------------------------

type CatApp a = Sem '[ CatCmd
                     , UndoListCmd
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
-- run a single command, for testing

runApp :: ImgStore -> AppEnv -> CatApp a -> IO (ImgStore, Either Text a)
runApp ims env cmd = do
  logQ <- createJobQueue
  jh   <- openJournal (env ^. appEnvJournal)
  runM
    . runState ims
    . runError @Text
    . logToStdErr
    . logWithLevel (env ^. appEnvLogLevel)
    . runLogEnvFS jh logQ (env ^. appEnvCat)
    . undoListNoop
    . evalCatCmd
    $ cmd

-- --------------------
--
-- for simple testing in ghci

r :: CatApp a -> IO (ImgStore, Either Text a)
r = runApp emptyImgStore defaultAppEnv

----------------------------------------
--
-- run on server side: catalog reading command

runRead :: TMVar ImgStore
        -> BGQueue
        -> AppEnv
        -> CatApp a
        -> IO (Either Text a)
runRead var logQ env =
  runM
  . evalStateTMVar  var
  . runError        @Text
  . runLogging      logQ (env ^. appEnvLogLevel)
  . runLogEnvFS     Nothing logQ (env ^. appEnvCat)
  . undoListNoop
  . evalCatCmd

{-# INLINE runRead #-}

--------------------
--
-- run on server side: catalog modifying command

runMody :: JournalHandle
        -> IORef UndoHistory
        -> TMVar ImgStore
        -> TMVar ImgStore
        -> BGQueue
        -> AppEnv
        -> CatApp a
        -> IO (Either Text a)
runMody jh hist rvar mvar logQ env =
  runM' mvar
  . modifyStateTMVar rvar mvar
  . runError        @Text
  . runLogging      logQ (env ^. appEnvLogLevel)
  . runLogEnvFS     jh logQ (env ^. appEnvCat)
  . runStateIORef   hist
  . undoListWithState
  . evalCatCmd

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
        (const cmd')

--------------------

runBG :: JournalHandle
      -> TMVar ImgStore
      -> TChan Job
      -> TChan Job
      -> AppEnv
      -> CatApp a
      -> IO ()
runBG jh var qu logQ env =
  runM
  . evalStateTChan var qu
  . runError       @Text
  . runLogging     logQ (env ^. appEnvLogLevel)
  . runLogEnvFS    jh logQ (env ^. appEnvCat)
  . undoListNoop
  . evalCatCmd

--------------------

runLogQ :: LogLevel
        -> BGQueue
        -> Text -- Sem '[Consume LogMsg, Embed IO] ()
        -> IO ()
runLogQ lev qu t =
  runM
  . logToBGQueue qu
  . logWithLevel lev
  $ log'info t

{-# INLINE runLogQ #-}

--------------------
--
-- common run parts

runLogEnvFS :: (EffError r, EffIStore r, EffLogging r, Member (Embed IO) r)
             => JournalHandle
             -> BGQueue
             -> CatEnv
             -> Sem (ExecProg
                     : FileSystem
                     : Time
                     : Reader CatEnv
                     : Consume JournalP
                     : r
                    ) a
             -> Sem  r a
runLogEnvFS jHandle logQ catEnv =
  runJournal jHandle logQ
  . runReader catEnv
  . runOS

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


type JournalHandle = Maybe (Either Handle Handle)

runJournal :: (Member (Embed IO) r)
           => JournalHandle
           -> BGQueue
           -> Sem (Consume JournalP ': r) a
           -> Sem r a
runJournal jh logQ =
  case jh of
    Nothing
      -> journalToDevNull
    Just (Left std)
      -> journalToBGQueue logQ std
    Just (Right hdl)
      -> journalToHandle hdl

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

openJournal :: Maybe TextPath -> IO JournalHandle
openJournal tp = case tp of
  Nothing
    -> return Nothing
  Just p
    | p == "1"
      -> return $ Just (Left stdout)
    | p == "2"
      -> return $ Just (Left stderr)
    | otherwise
      -> Just . Right <$> openFile (p ^. isoString) WriteMode

------------------------------------------------------------------------------
