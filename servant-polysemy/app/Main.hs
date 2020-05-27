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

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- {-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
--
-- the preliminary main module for testing
-- should be moved to an app

module Main
where

-- polysemy and polysemy-tools
import Polysemy.State.RunTMVar
import System.ExecProg

-- catalog-polysemy
import Catalog.CatalogIO
import Catalog.CatEnv
import Catalog.Effects
import Catalog.Effects.CatCmd
import Catalog.Effects.CatCmd.Interpreter
import Catalog.Journal (journalToStdout, journalToDevNull)
import Catalog.Run

-- catalog
import Data.Prim
import Data.Journal    (JournalP)
import Data.ImageStore (ImgStore, emptyImgStore)

-- libs
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TChan
import Control.Monad.IO.Class (liftIO)
import qualified Control.Exception as Ex

-- servant
import Servant
import Servant.Server ()

import Network.Wai.Handler.Warp( setPort
                               , setLogger
                               , defaultSettings
                               , runSettings
                               )
import Network.Wai.Logger      ( withStdoutLogger )

import System.Exit             ( die )

------------------------------------------------------------------------------

catalogServer :: CatEnv
              -> (forall a . CatApp a -> Handler a)
              -> (forall a . CatApp a -> Handler a)
              -> (forall a . CatApp a -> Handler ())
              -> Server CatalogAPI
catalogServer env runReadC _runModyC _runBGC =
  bootstrap
  where
    mountPath :: FilePath
    mountPath = env ^. catMountPath . isoString

    static p = do
      serveDirectoryWebApp (mountPath ++ p)

    bootstrap         = static ps'bootstrap

    runR1 :: forall a . (Path -> CatApp a) -> [Text] -> Handler a
    runR1 cmd' = runReadC  . cmd' . listToPath


type CatalogAPI  =
  "bootstrap" :> Raw

----------------------------------------

main :: IO ()
main = do
  let env  = defaultAppEnv

  rvar  <- newTMVarIO emptyImgStore
  mvar  <- newTMVarIO emptyImgStore
  qu    <- createJobQueue

  let runRC :: CatApp a -> Handler a
      runRC = ioeither2Handler . runRead rvar env

  let runMC :: CatApp a -> Handler a
      runMC = ioeither2Handler . runMody rvar mvar env

  let runBQ :: CatApp a -> Handler ()
      runBQ = liftIO . runBG rvar qu env

  runMody rvar mvar env initImgStore >>=
    either (die . (^. isoString) ) return

  -- start servant server
  withStdoutLogger $ \logger -> do
    let settings =
          defaultSettings & setPort   (env ^. appEnvPort)
                          & setLogger logger

    runSettings settings $
      serve (Proxy :: Proxy CatalogAPI) $
      catalogServer (env ^. appEnvCat) runRC runMC runBQ


ioeither2Handler :: IO (Either Text a) -> Handler a
ioeither2Handler cmd = do
  res <- liftIO cmd
  either raise500 return res
  where
    raise500 :: Text -> Handler a
    raise500 msg =
      throwError $ err500 { errBody = msg ^. isoString . from isoString }

------------------------------------------------------------------------------
