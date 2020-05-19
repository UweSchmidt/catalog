{-# LANGUAGE
    DataKinds,
    FlexibleContexts,
    GADTs,
    OverloadedStrings,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TemplateHaskell,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)

module Polysemy.State.RunTMVar
where

import Polysemy
import Polysemy.State

import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TMVar

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
  (s1, res) <- runState s0 stateCmd
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
  res <- snd <$> runState s0 stateCmd
  return res

{-# INLINE evalStateTMVar #-}

-- allow non blocking concurrent read actions on a state
-- but synchronize the modifiying actions
--
-- ther are 2 vars holding the state,
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

------------------------------------------------------------------------
