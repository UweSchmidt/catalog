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
  { _appEnvLogLevel :: !LogLevel
  , _appEnvJournal  :: !(Maybe TextPath)
  , _appEnvPort     :: !Int
  , _appEnvCat      :: !CatEnv
  }

data CatEnv = CatEnv
  { _catMountPath   :: !TextPath
  , _catJsonArchive :: !TextPath
  , _catGPSCache    :: !TextPath
  , _catForceMDU    :: !Bool        -- force metadata update
  , _catSaveBothIx  :: !Bool
  , _catNoSync      :: !Bool
  , _catFontName    :: !Text
  , _catHost        :: !Text
  , _catPort        :: !Int
  }

$(makeLenses ''CatEnv)
$(makeLenses ''AppEnv)

defaultCatEnv :: CatEnv
defaultCatEnv = CatEnv
                { _catMountPath   = "."
                , _catJsonArchive = "photos.pathid.json"
                , _catGPSCache    = "gps-cache.json"
                , _catForceMDU    = False
                , _catSaveBothIx  = False
                , _catNoSync      = False
                , _catFontName    = mempty
                , _catHost        = mempty
                , _catPort        = 3001
                }

defaultAppEnv :: AppEnv
defaultAppEnv = AppEnv LogErr Nothing 3001 defaultCatEnv

------------------------------------------------------------------------------
