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

module GPS.Effects.GeoLocCmd
  ( -- * Effects
    GeoLocCmd (..)

    -- * Actions
  , glAddress

    -- * Interpreter
  , nominatimHttps

    -- * reexported catalog types
  , GPSposDec
  , gpsLat
  , gpsLong
  )
where

import Polysemy
import Polysemy.EmbedExc
import Polysemy.Error
import Polysemy.HttpRequest
import Polysemy.HttpRequest.SimpleRequests
import Polysemy.Logging
import Polysemy.Reader

import Data.Prim

import Data.MetaData
       ( MetaKey )

import Data.Prim.GPS
       ( GPSposDec
       , gpsLat
       , gpsLong
       )

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

import Network.HTTP.Types.Header

import Text.Printf
       ( printf )

import Client.Options
       ( userAgent)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson            as J

------------------------------------------------------------------------------

newtype GeoAddrList = GAS [GeoAddress]

data GeoAddress = GA
  { _display_name :: Text
  , _geo_address  :: GeoAddress1
  }

data GeoAddress1 = GA1
  { _house_number :: Text
  , _road         :: Text
  , _suburb       :: Text
  , _city         :: Text
  , _county       :: Text
  , _state        :: Text
  , _postcode     :: Text
  , _country      :: Text
  , _country_code :: Text
  }

instance FromJSON GeoAddrList where
  parseJSON = J.withObject "GeoAddrList" $ \ o ->
    GAS <$> o J..: "features"

instance FromJSON GeoAddress where
  parseJSON = J.withObject "GeoAddress" $ \ o ->
    GA
    <$> o J..: "display_name"
    <*> o J..: "address"

instance FromJSON GeoAddress1 where
  parseJSON = J.withObject "GeoAddress1" $ \ o ->
    GA1
    <$> (o J..:? "house_number" J..!= mempty)
    <*> (o J..:? "road"         J..!= mempty)
    <*> (o J..:? "suburb"       J..!= mempty)
    <*> (o J..:? "city"         J..!= mempty)
    <*> (o J..:? "country"      J..!= mempty)
    <*> (o J..:? "state"        J..!= mempty)
    <*> (o J..:? "postcode"     J..!= mempty)
    <*> (o J..:? "country"      J..!= mempty)
    <*> (o J..:? "country_code" J..!= mempty)


------------------------------------------------------------------------------

data GeoLocCmd m a where
  GlAddress    :: GPSposDec -> GeoLocCmd m GeoAddress

makeSem ''GeoLocCmd

------------------------------------------------------------------------------

nominatimHttps :: forall r a
                . ( Member (Embed IO) r
                  , Member (Error Text) r
                  , Member (Reader Request) r
                  , Member HttpRequest r
                  , Member Logging r
                  )
               => Sem (GeoLocCmd : r) a -> Sem r a
nominatimHttps = do
  interpret $
    \ c -> case c of
      GlAddress loc -> do
        req <- embedExcText $ nominatimRequest loc  -- create request
        lbs <- local (const req) execReq            -- set request and exec
        jsonDecode lbs                              -- decode response

nominatimRequest :: GPSposDec -> IO Request
nominatimRequest gps = do
  req <- parseRequest $
         "https://nominatim.openstreetmap.org/reverse?"
         ++
         showPosDec gps
         ++
         "&zoom=18&format=geojson&extratags=1"
  return $ addHeader uah req
  where
    showPosDec :: GPSposDec -> String
    showPosDec loc =
      printf "lat=%.9f&lon=%.9f" (loc ^. gpsLat) (loc ^. gpsLong)

    uah :: Header
    uah = (hUserAgent, BS.pack userAgent)

    addHeader :: Header -> Request -> Request
    addHeader hd req = req {requestHeaders = hd : requestHeaders req}

------------------------------------------------------------------------------
