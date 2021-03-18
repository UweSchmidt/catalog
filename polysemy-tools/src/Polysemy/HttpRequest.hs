{-# LANGUAGE
    DataKinds,
    FlexibleContexts,
    GADTs,
    OverloadedStrings,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TemplateHaskell,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Polysemy.HttpRequest
  ( -- Effect
    HttpRequest (..)

    -- * Actions
  , httpRequest

    -- * Interpretations
  , basicHttpRequests
  , simpleHttpRequests

    -- * aux types and functions
  , ResponseLBS
  , newBasicManager
  , newBasicTlsManager
  , httpExcToText
  )
where

import Polysemy
import Polysemy.Error


import Network.HTTP.Client
       ( Request(..)
       , Response(..)
       , Manager
       , HttpException
       , httpLbs
       , defaultManagerSettings
       -- , defaultRequest
       , newManager
       -- , responseTimeoutNone
       )
import Network.HTTP.Client.TLS
       ( newTlsManager )

import Data.Text
       ( Text )

import qualified Control.Exception    as X
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T

------------------------------------------------------------------------------

type ResponseLBS = Response LBS.ByteString

data HttpRequest m a where
  HttpRequest :: Request -> HttpRequest m ResponseLBS

makeSem ''HttpRequest

------------------------------------------------------------------------------
--
-- | most basic form of HTTP request with Network.HTTP.Client
--
-- mapping to IO with lazy ByteString's a response body
-- and propagation of HttpExceptions


basicHttpRequests :: forall exc r
                   . ( Member (Embed IO) r
                     , Member (Error exc) r
                     )
                  => (HttpException -> exc)
                  -> Manager
                  -> Manager
                  -> InterpreterFor HttpRequest r
basicHttpRequests ef manager managerTls =
  interpret $
  \ c -> case c of
    HttpRequest req -> do
      r <- embed $ X.try (httpLbs req $
                           if secure req
                           then managerTls
                           else manager
                         )
      case r of
        Left  e -> throw @ exc (ef e)
        Right a -> pure a

      -- shorter in polysemy >= 1.3
      -- r <- fromExceptionVia ef (httpLbs req manager)


newBasicManager :: IO Manager
newBasicManager =
  newManager defaultManagerSettings

newBasicTlsManager :: IO Manager
newBasicTlsManager =
  newTlsManager

httpExcToText :: HttpException -> Text
httpExcToText = T.pack . show

simpleHttpRequests :: forall r
                   . ( Member (Embed IO) r
                     , Member (Error Text) r
                     )
                  => InterpreterFor HttpRequest r
simpleHttpRequests cmd = do
  manHTTP  <- embed @ IO $ newBasicManager
  manHTTPS <- embed @ IO $ newBasicTlsManager
  basicHttpRequests httpExcToText manHTTP manHTTPS cmd

------------------------------------------------------------------------------
