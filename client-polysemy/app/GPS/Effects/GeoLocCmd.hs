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
    Cache(..)

    -- * Actions
  , lookupGeoCache
  , putGeoCache
  , getGeoCache

    -- * Interpreter
  , runReverseGeoLoc

    -- * aux types and functions
  , nominatimRequest
  , locTestRequest
  )
where

import Polysemy
import Polysemy.Cache
import Polysemy.Delay
import Polysemy.EmbedExc
import Polysemy.Error
import Polysemy.HttpRequest
import Polysemy.HttpRequest.SimpleRequests
import Polysemy.Logging
import Polysemy.Reader

import Data.Prim

import Network.HTTP.Client
       ( Request(..) )

import Network.HTTP.Types.Header

import Text.Printf
       ( printf )

import Client.Version
       ( userAgent )

import qualified Data.ByteString.Lazy     as LS
import qualified Data.ByteString.Char8    as BS
import qualified Data.Aeson               as J
import qualified Data.Aeson.Encode.Pretty as J

------------------------------------------------------------------------------

lookupGeoCache :: (Member (Cache GPSposDec GeoAddrList) r)
               => GPSposDec -> Sem r (Maybe GeoAddrList)
lookupGeoCache = lookupCache


putGeoCache :: ( Member (Error Text) r
               , Member (Cache GPSposDec GeoAddrList) r)
            => LS.ByteString
            -> Sem r ()
putGeoCache lbs = do
  case J.decode lbs of
    Nothing -> throw "json decode Error"
    Just cs -> putCache cs

getGeoCache :: ( Member (Error Text) r
               , Member (Cache GPSposDec GeoAddrList) r)
            => Sem r LS.ByteString
getGeoCache = J.encodePretty' conf <$> getCache
  where
    conf = J.defConfig
           { J.confIndent  = J.Spaces 2 }

------------------------------------------------------------------------------

data GeoLocCmd m a where
  GeoLocAddress :: GPSposDec -> GeoLocCmd m (Maybe GeoAddrList)

makeSem ''GeoLocCmd

------------------------------------------------------------------------------

-- currently the only interpreter for getting
-- locations mapped to addresses
--
-- nominatim uses open street map data
-- it allows only low bandwidth usage,
-- max. a single request per second
-- and results must be cached to
-- to avoid repeated request with same coordinates
--
-- runReverseGeoLoc is the interpreter
-- supporting cached lookup
-- nominatimhttps adds delayed requests
-- nominatimhttps' does the real request
-- and decodes JSON data of response


runReverseGeoLoc :: forall r a
                . ( Member (Embed IO) r
                  , Member (Error Text) r
                  , Member Logging r
                  , Member (Reader Request) r
                  , Member HttpRequest r
                  )
               => Sem (Cache GPSposDec GeoAddrList : r) a -> Sem r a
runReverseGeoLoc =
  nominatimHttps               -- add GeoLocCmd effect
  . runCache geoLocAddress     -- add Cache effect, use GeoLocCmd effect
  . raiseUnder                 -- hide GeoLocCmd effect
                               -- only the Cache commands are needed

nominatimHttps :: forall r a
                . ( Member (Embed IO) r
                  , Member (Error Text) r
                  , Member Logging r
                  , Member (Reader Request) r
                  , Member HttpRequest r
                  )
               => Sem (GeoLocCmd : r) a -> Sem r a
nominatimHttps =
  delayedExec ioExcToText                  -- add Delay effect
  . nominatimHttps' nominatimRequest       -- add GeoLocCmd effect
--  . nominatimHttps' locTestRequest         -- add GeoLocCmd effect
  . raiseUnder                             -- hide Delay effect


nominatimHttps' :: forall r a
                 . ( Member (Embed IO) r
                   , Member (Error Text) r
                   , Member Logging r
                   , Member (Reader Request) r
                   , Member HttpRequest r
                   , Member Delay r
                   )
                => (GPSposDec -> IO Request)
                -> Sem (GeoLocCmd : r) a -> Sem r a
nominatimHttps' req0 = do
  interpret $
    \ c -> case c of
      GeoLocAddress loc -> delayExec timeBetweenRequests $
        ( do
            log'trc $ untext [ "geoLocAddress: read address for: "
                             , isoString . prismString # loc
                             ]

            req <- embedExcText $ req0 loc              -- create request
            lbs <- local (const req) execReq            -- set request and exec

            log'trc $ untext [ "geoLocAddress: result: "
                             , lbsToText lbs
                             ]
            -- decode response
            (Just . toGeoAddrList <$> jsonDecode lbs)
              `catch`
              (\ msg ->
                  do
                    log'warn $ untext ["geoLocAddress: ", msg]
                    return $ Just []
              )
        )
        `catch`
        ( \ msg ->
            do
              log'warn $ untext ["geoLocAddress: nominatim server err:", msg]
              return Nothing
        )
  where
    timeBetweenRequests :: Int
    timeBetweenRequests = 2   -- seconds

-- create request for nominatim server of open street map
-- location to address translation

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

locTestRequest :: GPSposDec -> IO Request
locTestRequest _gps = do
  parseRequest $
    "http://localhost:3001/assets/javascript/geotest.js"

------------------------------------------------------------------------------
--
-- aux types for JSON geo response format "geojson"

newtype GeoAddrList' = GAL' [GeoAddress']
newtype GeoAddress'  = GA'  GeoAddress

instance FromJSON GeoAddrList' where
  parseJSON = J.withObject "GeoAddrList'" $ \ o ->
    GAL' <$> o J..: "features"

instance FromJSON GeoAddress' where
  parseJSON = J.withObject "GeoAddress'" $ \ o ->
    GA' <$> o J..: "properties"

toGeoAddrList :: GeoAddrList' -> GeoAddrList
toGeoAddrList (GAL' xs) = map (\ (GA' x) -> x) xs

------------------------------------------------------------------------------