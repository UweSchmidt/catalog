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

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- {-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------

module Catalog.CatEnv
where

import Polysemy.FileSystem (TextPath)
import Polysemy.Logging    (LogLevel (..))

import Data.Prim

------------------------------------------------------------------------------

data AppEnv = AppEnv
  { _appEnvLogLevel :: LogLevel
  , _appEnvJournal  :: Maybe TextPath
  , _appEnvPort     :: Int
  , _appEnvCat      :: CatEnv
  }

data CatEnv = CatEnv
  { _catMountPath   :: TextPath
  , _catJsonArchive :: TextPath
  , _catForceMDU    :: Bool        -- force metadata update
  , _catSaveBothIx  :: Bool
  , _catFontName    :: Text
  }

$(makeLenses ''CatEnv)
$(makeLenses ''AppEnv)

defaultCatEnv :: CatEnv
defaultCatEnv = CatEnv
                { _catMountPath   = "."
                , _catJsonArchive = "photos.pathid.json"
                , _catForceMDU    = False
                , _catSaveBothIx  = False
                , _catFontName    = mempty
                }

defaultAppEnv :: AppEnv
defaultAppEnv = AppEnv LogErr Nothing 3001 defaultCatEnv

------------------------------------------------------------------------------
