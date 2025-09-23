{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------

module Catalog.CatEnv
where

import Polysemy.FileSystem
       ( TextPath )

import Polysemy.Logging
       ( LogLevel (..) )

import Data.Prim
       ( Text
       , makeLenses
       )

------------------------------------------------------------------------------

data AppEnv = AppEnv
  { _appEnvCat      :: !CatEnv
  }

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
  }

$(makeLenses ''CatEnv)
$(makeLenses ''AppEnv)

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
                }

defaultAppEnv :: AppEnv
defaultAppEnv = AppEnv defaultCatEnv

------------------------------------------------------------------------------
