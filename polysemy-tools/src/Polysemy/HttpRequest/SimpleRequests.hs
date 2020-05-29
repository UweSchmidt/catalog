{-# LANGUAGE ConstraintKinds #-}
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

------------------------------------------------------------------------

module Polysemy.HttpRequest.SimpleRequests
  (
    -- * common HTTP request variants
    basicReq
  , getReq
  , postReq
  , simpleJSONReq
  , argJSONReq

    -- * aux types
  , HostPort
  , HttpEffects
  )
where

import Polysemy
import Polysemy.Error
import Polysemy.HttpRequest
import Polysemy.Logging
import Polysemy.Reader

import Network.HTTP.Client
       ( Request(..)
       , RequestBody(..)
       , Response(..)
       , defaultRequest
       , responseTimeoutNone
       )

import Network.HTTP.Types.Header
       ( hContentType )

import Network.HTTP.Types.Status
       ( statusCode
       , statusMessage
       )

import Data.Aeson
       ( ToJSON
       , FromJSON
       , decode
       , encode
       )
import Data.Text
       ( Text )


import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LCS
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as CS
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T

------------------------------------------------------------------------------


type HostPort = (Text, Int)

type HttpEffects r =
  (Members '[Reader HostPort, HttpRequest, Error Text, Logging] r)

getReq :: HttpEffects r
       => Text
       -> Sem r LBS.ByteString
getReq = basicReq "GET" id

--------------------

postReq :: HttpEffects r
         => (Request -> Request)
         -> Text
         -> Sem r LBS.ByteString
postReq = basicReq "POST"

--------------------

simpleJSONReq :: ( FromJSON a
                 , HttpEffects r
                 )
              => Text
              -> Sem r a
simpleJSONReq path' =
  postReq id path'
  >>= jsonDecode

--------------------

argJSONReq :: ( ToJSON a
              , FromJSON b
              , HttpEffects r
              )
           => a -> Text
           -> Sem r b
argJSONReq arg' path' =
  postReq (setReqBodyJSON $ encode arg') path'
  >>= jsonDecode
  where
    setReqBodyJSON :: LBS.ByteString -> Request -> Request
    setReqBodyJSON body' req =
      req { requestBody =
              RequestBodyLBS body'
          , requestHeaders =
              (hContentType, "application/json; charset=utf-8") :
              requestHeaders req
          }


--------------------

basicReq :: HttpEffects r
         => Text
         -> (Request -> Request)
         -> Text
         -> Sem r LBS.ByteString

basicReq method' setBody path' = do
  (host', port') <- ask
  let request = setBody $ defaultRequest
        { method = T.encodeUtf8 method'
        , host   = T.encodeUtf8 host'
        , port   = port'
        , path   = T.encodeUtf8 path'
        , responseTimeout = responseTimeoutNone
        }
  let ppReq = T.unwords
        [ T.pack . show $ method'
        , "http://"
          <> host'
          <> ":"
          <> (T.pack . show $ port')
          <> path'
        ]

  log'verb ppReq

  response <- httpRequest request

  let rbody  = responseBody   response
  let status = responseStatus response
  let scode  = statusCode     status
  let smsg   = T.unwords
               [ T.pack $ show scode
               , bsToText $ statusMessage status
                 -- no decodeUtf8, avoid decodeUtf8 errors
               ]

  log'verb smsg

  if scode /= 200
    then do let msg
                  | scode == 500 = lbsToText $ rbody
                  | otherwise    = smsg
            abortWith msg

    else return rbody

------------------------------------------------------------------------------

jsonDecode :: ( FromJSON a
              , Member (Error Text) r
              , Member Logging r
              )
           => LBS.ByteString
           -> Sem r a
jsonDecode lbs =
  maybe
    (abortWith "JSON decode error")
    return
    (decode lbs)

------------------------------------------------------------------------------
--
-- no decodeUtf8, avoid decodeUtf8 errors

lbsToText :: LBS.ByteString -> Text
lbsToText = T.pack . LCS.unpack

bsToText :: BS.ByteString -> Text
bsToText = T.pack . CS.unpack

------------------------------------------------------------------------------
