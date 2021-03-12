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

{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Delay
  ( -- Effect
    Delay (..)

    -- * Actions
  , delayExec

    -- * Interpretations
  , delayedExec

    -- * aux types and functions
  )
where

import Polysemy
import Polysemy.Error
import Polysemy.EmbedExc
import Polysemy.Time

import Control.Monad                ( when )
import Control.Concurrent           ( threadDelay )
import Control.Concurrent.STM       ( atomically )
import Control.Concurrent.STM.TMVar

{- imports for test

import Data.Text                    ( Text )
import qualified Data.Text          as T

import System.IO
-- -}

------------------------------------------------------------------------------

data Delay m a where
  -- delay the execution of cmd
  -- if last cmd executed with delay was more recently
  -- than the given time span in seconds
  --
  -- example: query an external http resource
  -- with limited capacity, e.g.
  -- open street map location service

  DelayExec  :: Int -> m a -> Delay m a

makeSem ''Delay

------------------------------------------------------------------------------

delayedExec' :: forall r a
              . ( Member (Embed IO) r
                , Member Time r
                )
             => TMVar Int
             -> Sem (Delay : r) a -> Sem r a
delayedExec' lastTime =
  interpretH $
  \ c -> case c of
    DelayExec sec cmd -> do
      lst <- embed $ atomically $ takeTMVar lastTime
      now <- fromEnum <$> currentTime
      -- delay thread when last action was recently executed
      embed $ do
        let nxt = lst + sec
        when (nxt > now) $ do
          threadDelay ((nxt - now) * 1000000) -- sec -> microsec

      -- run current action
      mm <- runT cmd

      -- enable next action to run
      now2 <- fromEnum <$> currentTime
      embed $ do
        atomically $ putTMVar lastTime now2

      raise $ delayedExec' lastTime mm


delayedExec'' :: forall r a
               . ( Member (Embed IO) r
                 , Member Time r
                 )
              => Sem (Delay : r) a -> Sem r a
delayedExec'' cmd = do
  refLast <- embed @ IO $ newTMVarIO 0
  delayedExec' refLast cmd


delayedExec :: forall exc r a
             . ( Member (Embed IO) r
               , Member (Error exc) r
               )
            => (IOException -> exc)
            -> Sem (Delay : r) a -> Sem r a
delayedExec ef =
  posixTime ef      -- add Time effect
  . delayedExec''   -- add Delay effect
  . raiseUnder      -- remove Time effect

  ------------------------------------------------------------------------------

{- test delayed cmd exec

test :: IO ()
test = do
  res <- runM
         . runError @ Text
         . delayedExec ioExcToText
         $ do cmd
              embed $ threadDelay (2 * 1000000)
              embed $ putStrLn "second run"
              cmd

  either (putStrLn . T.unpack) return res
  where
    cmd = sequence_ $ map (\ i -> delayExec 1 $ embed $ do
                              putStrLn $ show i ++ ". time"
                              hFlush stdout
                          ) [1::Int .. 10]
-- -}
------------------------------------------------------------------------------
