{-# LANGUAGE
    DataKinds,
    FlexibleContexts,
    GADTs,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeFamilies,
    TypeOperators
#-} -- default extensions (only for emacs)

------------------------------------------------------------------------------

module Polysemy.Consume.BGQueue
  ( -- * Interpretation of Consume
    writeToBGQueue       -- write an IO() cmd into a TChan queue

    -- * Background queue generation
  , startBGQueue

    -- * Types
  , BGQueue
  , BGJob
  )
where

import Polysemy
import Polysemy.Consume

import Control.Monad           ( forever
                               )
import Control.Concurrent.STM  ( atomically
                               , TChan
                               , newTChanIO
                               , readTChan
                               , writeTChan
                               )
import Control.Concurrent.Async( async
                               , link
                               )

------------------------------------------------------------------------------
--
-- | Consume values, by mapping the values to IO () actions
-- writing the actions to a queue and performing
-- the actions in a background thread
-- the queue is implemented as a STM TChan

type BGJob   = IO ()
type BGQueue = TChan BGJob

writeToBGQueue :: Member (Embed IO) r
               => BGQueue
               -> (v -> BGJob)
               -> InterpreterFor (Consume v) r
writeToBGQueue qu cmd =
  consumeIO (atomically . writeTChan qu . cmd)

{-# INLINE writeToBGQueue #-}

startBGQueue :: IO BGQueue
startBGQueue = do
  qu <- newTChanIO
  bg <- async $ process qu
  link bg
  return qu
  where
    process qu = forever $ do
      cmd <- atomically $ readTChan qu
      cmd

------------------------------------------------------------------------------
