------------------------------------------------------------------------------

module Catalog.TimeStamp
  ( whatTimeIsIt
  , lastModified
  , nowAsIso8601
  )
where

import Catalog.Effects
       ( Sem
       , EffFileSys
       , EffTime
       , TextPath
       , currentTime
       , getModiTime
       )

import Data.Prim
       ( Text
       , TimeStamp
       , IsoText(isoText)
       , iso8601TimeStamp
       , (^.)
       , (#)
       , isoEpochTime
       )

------------------------------------------------------------------------------
--
-- read the wall clock

whatTimeIsIt :: EffTime r => Sem r TimeStamp
whatTimeIsIt = (isoEpochTime #) <$> currentTime

lastModified :: EffFileSys r => TextPath -> Sem r TimeStamp
lastModified p = (isoEpochTime #) <$> getModiTime p

nowAsIso8601 :: EffTime r => Sem r Text
nowAsIso8601 = do
  (^. isoText) . iso8601TimeStamp <$> whatTimeIsIt

-- ----------------------------------------
