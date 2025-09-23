------------------------------------------------------------------------------

module Client.Version
  ( module Client.Version
  , module Catalog.Version
  )
where

import Data.Prim.Prelude
       ( (^.)
       , IsoString(isoString)
       )

import Catalog.Version

appname :: String
appname = "client-polysemy"

userAgent :: String
userAgent = appname <> " - " <> version ^. isoString <> " (" <> date ^. isoString <> ")"

------------------------------------------------------------------------------
