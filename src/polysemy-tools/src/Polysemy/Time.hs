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
import Polysemy.Error
import Polysemy.EmbedExc

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
    \ c -> case c of
             CurrentTime -> embedExc ef $ X.epochTime
{-# INLINE posixTime #-}

------------------------------------------------------------------------------
