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
  , _appEnvJournal  :: Bool
  , _appEnvCat      :: CatEnv
  }

data CatEnv = CatEnv
  { _catMountPath :: TextPath
  , _catForceMDU  :: Bool        -- force metadata update
  }

$(makeLenses ''CatEnv)
$(makeLenses ''AppEnv)

defaultCatEnv :: CatEnv
defaultCatEnv = CatEnv "." False

defaultAppEnv :: AppEnv
defaultAppEnv = AppEnv LogErr True defaultCatEnv

------------------------------------------------------------------------------
