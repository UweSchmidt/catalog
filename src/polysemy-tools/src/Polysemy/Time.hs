{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------

module Polysemy.Time
  ( -- Effect
    Time (..)

    -- * Actions
  , currentTime

  , -- * Interpretations
    posixTime

    -- * aux types and functions
  , EpochTime
  )
where

import Polysemy
       ( InterpreterFor
       , Member
       , Embed
       , makeSem
       , interpret
       )
import Polysemy.Error
       ( Error )

import Polysemy.EmbedExc
       ( IOException
       , embedExc
       )

import System.Posix
       ( EpochTime )

import qualified System.Posix as X

------------------------------------------------------------------------------

data Time m a where
  CurrentTime  :: Time m EpochTime

makeSem ''Time

------------------------------------------------------------------------------
--
-- | posix time functions
--

posixTime :: ( Member (Embed IO) r
             , Member (Error exc) r
             )
          => (IOException -> exc)
          -> InterpreterFor Time r
posixTime ef = do
  interpret $
    \ case
      CurrentTime -> embedExc ef X.epochTime

{-# INLINE posixTime #-}

------------------------------------------------------------------------------
