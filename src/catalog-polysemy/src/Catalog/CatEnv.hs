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

import Data.Prim
       ( Text
       , makeLenses
       )

------------------------------------------------------------------------------

type AppEnv = CatEnv

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

------------------------------------------------------------------------------
