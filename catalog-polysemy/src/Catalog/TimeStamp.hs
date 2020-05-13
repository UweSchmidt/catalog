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

------------------------------------------------------------------------------
--
-- read the wall clock

whatTimeIsIt :: EffTime r => Sem r TimeStamp
whatTimeIsIt = (isoEpochTime #) <$> currentTime

lastModified :: EffFileSys r => TextPath -> Sem r TimeStamp
lastModified p = (isoEpochTime #) <$> getModiTime p

-- ----------------------------------------
