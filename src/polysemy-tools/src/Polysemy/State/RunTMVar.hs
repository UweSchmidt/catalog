----------------------------------------

module Polysemy.State.RunTMVar
  ( Job
  , runStateTMVar
  , evalStateTMVar
  , modifyStateTMVar
  , evalStateTChan
  , createJobQueue
  )
where

import Polysemy
       ( Member
       , Sem
       , Embed
       , runM
       , embed
       )
import Polysemy.State
       ( runState
       , State
       )

import Control.Concurrent.Async
       ( async
       , link
       )
import Control.Concurrent.STM
       ( atomically )

import Control.Concurrent.STM.TMVar
       ( TMVar
       , readTMVar
       , putTMVar
       , swapTMVar
       , takeTMVar
       )
import Control.Concurrent.STM.TChan
       ( TChan
       , newTChanIO
       , readTChan
       , writeTChan
       )
import Control.Monad
       ( forever
       , join
       )

----------------------------------------
--
-- run a statful cmd on the contents of a TMVar

runStateTMVar
    :: forall s r a
     . Member (Embed IO) r
    => TMVar s
    -> Sem (State s ': r) a
    -> Sem r a

runStateTMVar ref stateCmd = do
  s0 <- embed $ atomically $ takeTMVar ref
  (!s1, res) <- runState s0 stateCmd
  embed $ atomically $ putTMVar ref s1
  return res

{-# INLINE runStateTMVar #-}

-- run a stateful action with initial state read from TMVar
-- and discard the final state
evalStateTMVar
    :: forall s r a
     . Member (Embed IO) r
    => TMVar s
    -> Sem (State s ': r) a
    -> Sem r a

evalStateTMVar ref stateCmd = do
  s0 <- embed $ atomically $ readTMVar ref
  snd <$> runState s0 stateCmd

{-# INLINE evalStateTMVar #-}

-- allow non blocking concurrent read actions on a state
-- but synchronize the modifiying actions
--
-- there are 2 vars holding the state,
-- one for reading, one for the state changes
--
-- the read var never blocks, when accessed
-- the mody var synchronizes the modifiying actions
-- and updates the read var with the new state
--
modifyStateTMVar
    :: forall s r a
     . Member (Embed IO) r
    => TMVar s
    -> TMVar s
    -> Sem (State s ': r) a
    -> Sem r a
modifyStateTMVar rvar mvar stateCmd = do
  s0 <- embed $ atomically $
          takeTMVar mvar  -- take the mody state

  (s1, res) <- runState s0 stateCmd
  embed $ atomically $
    swapTMVar rvar s1         -- put new state into read var
    >>
    putTMVar mvar s1          -- put new state back into mody var

  return res

{-# INLINE modifyStateTMVar #-}

-- run a stateful action as background job
--
-- the initial state is read from a TMVar
-- the final state is discarded

evalStateTChan
    :: forall s a
     . TMVar s
    -> TChan Job
    -> Sem (State s ': Embed IO ': '[]) a
    -> Sem (Embed IO ': '[]) ()

evalStateTChan ref queue cmd = do
  embed $ atomically $ writeTChan queue runc
  where
    runc :: Job
    runc = do
      store <- atomically $ readTMVar ref
      _ <- runM . runState store $ cmd
      return ()

type Job = IO ()

createJobQueue :: IO (TChan Job)
createJobQueue = do
  qu <- newTChanIO
  bgq <- async $ jobQueue qu
  link bgq
  return qu

jobQueue :: TChan Job -> IO ()
jobQueue q =
  forever $
  join (atomically $ readTChan q)

{-# INLINE evalStateTChan #-}

------------------------------------------------------------------------
