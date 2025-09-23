{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------

module Catalog.CatEnv
where

import Polysemy.FileSystem
       ( TextPath )

import Polysemy.Logging
       ( LogLevel (..) )

import Catalog.Version
       ( date,
         version,
       )

import Data.Prim.Prelude
       ( Text
       , ToJSON(..)
       , makeLenses
       )

import qualified Data.Aeson as J

------------------------------------------------------------------------------

data CatEnv = CatEnv
  { _catMountPath   :: !TextPath
  , _catJsonArchive :: !TextPath
  , _catGPSCache    :: !TextPath
  , _catJournal     :: !(Maybe TextPath)
  , _catForceMDU    :: !Bool        -- force metadata update
  , _catSaveBothIx  :: !Bool
  , _catNoSync      :: !Bool
  , _catFontName    :: !Text
  , _catHost        :: !Text
  , _catPort        :: !Int
  , _catLogLevel    :: !LogLevel
  , _catVersion     :: !Text
  , _catVersDate    :: !Text
  , _catStart       :: !Text
  }

$(makeLenses ''CatEnv)

defaultCatEnv :: CatEnv
defaultCatEnv = CatEnv
  { _catMountPath   = "."
  , _catJsonArchive = "photos.pathid.json"
  , _catGPSCache    = "gps-cache.json"
  , _catJournal     = Nothing
  , _catForceMDU    = False
  , _catSaveBothIx  = False
  , _catNoSync      = False
  , _catFontName    = mempty
  , _catHost        = mempty
  , _catPort        = 3001
  , _catLogLevel    = LogErr
  , _catVersion     = version
  , _catVersDate    = date
  , _catStart       = "unknown"
  }

instance ToJSON CatEnv where
  toJSON ce = J.object
    [ "catMountPath"   J..= _catMountPath ce
    , "catJsonArchive" J..= _catJsonArchive ce
    , "catGPSCache"    J..= _catGPSCache ce
    , "catJournal"     J..= _catJournal ce
    , "catForceMDU"    J..= _catForceMDU ce
    , "catSaveBothIx"  J..= _catSaveBothIx ce
    , "catNoSync"      J..= _catNoSync ce
    , "catFontName"    J..= _catFontName ce
    , "catHost"        J..= _catHost ce
    , "catPort"        J..= _catPort ce
    , "catLogLevel"    J..= (show $ _catLogLevel ce)
    , "catVersion"     J..= _catVersion ce
    , "catVersDate"    J..= _catVersDate ce
    , "catStart"       J..= _catStart ce

    ]

------------------------------------------------------------------------------
