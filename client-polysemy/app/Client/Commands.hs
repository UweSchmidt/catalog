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
{-# LANGUAGE OverloadedStrings #-}

module Client.Commands
  ( -- * Effects
    CCommand (..)

    -- * Actions
  , ccLs

    -- * aux types
  , Path
  )
where

import Polysemy

import Data.Prim (Path)

------------------------------------------------------------------------------

data CCommand m a where
  CcLs :: Path -> CCommand m [Path]

makeSem ''CCommand

------------------------------------------------------------------------------
