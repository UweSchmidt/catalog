{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

------------------------------------------------------------------------------

module Client.Version
  ( module Client.Version
  , module Catalog.Version
  )
where

import Catalog.Version

appname :: String
appname = "client-polysemy"

userAgent :: String
userAgent = appname <> " - " <> version <> " (" <> date <> ")"
