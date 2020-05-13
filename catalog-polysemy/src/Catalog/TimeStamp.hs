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

------------------------------------------------------------------------------

module Catalog.TimeStamp
  ( whatTimeIsIt
  , lastModified
  )
where

import Catalog.Effects

import Data.Prim

import qualified Data.Text  as T

------------------------------------------------------------------------------
--
-- read the wall clock

whatTimeIsIt :: EffFileSys r => Sem r TimeStamp
whatTimeIsIt = (isoEpochTime #) <$> currentTime

lastModified :: EffFileSys r => TextPath -> Sem r TimeStamp
lastModified p = (isoEpochTime #) <$> getModiTime p

-- ----------------------------------------
