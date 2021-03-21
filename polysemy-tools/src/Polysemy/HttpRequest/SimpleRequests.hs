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
    execReq
  , basicReq
  , getReq
  , postReq
  , simpleJSONReq
  , argJSONReq

    -- * reexports and aux types and functions
  , HttpEffects
  , Request(..)
  , RequestBody(..)
  , Response(..)
  , parseRequest

  , jsonDecode
  , lbsToText
  , bsToText

  , module Network.HTTP.Types
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
       , parseRequest
       )

import Network.HTTP.Types
{-
       ( Method
       , hContentType
       , methodGet
       , methodPost
       , statusCode
       , statusMessage
       )
-}
-- import Network.HTTP.Types.Status

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


type HttpEffects r =
  (Members '[Reader Request, HttpRequest, Error Text, Logging] r)

--------------------

execReq :: HttpEffects r
       => Sem r LBS.ByteString
execReq = basicReq' id id id

--------------------

getReq :: HttpEffects r
       => Text
       -> Sem r LBS.ByteString
getReq = basicReq methodGet id

--------------------

postReq :: HttpEffects r
         => (Request -> Request)
         -> Text
         -> Sem r LBS.ByteString
postReq = basicReq methodPost

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
         => Method
         -> (Request -> Request)
         -> Text
         -> Sem r LBS.ByteString
basicReq method' setBody path' =
  basicReq' (const method') setBody (const $ T.encodeUtf8 path')

basicReq' :: HttpEffects r
          => (Method -> Method)
          -> (Request -> Request)
          -> (BS.ByteString -> BS.ByteString)
          -> Sem r LBS.ByteString

basicReq' setMethod setBody setPath = do
  req <- ask
  let request = setBody $ req
        { method = setMethod $ method req
        , path   = setPath   $ path   req
        }

  let ppBody b = case b of
        RequestBodyLBS lbs -> " " <> (T.pack . LCS.unpack $ lbs)
        _others            -> mempty

  let totxt = T.pack . CS.unpack

  let ppReq = T.unwords
        [ totxt $ method request
        , ( if secure req
            then "https://"
            else "http://"
          )
          <> (totxt $ host req)
          <> ":"
          <> (T.pack . show $ port req)
          <> (totxt $ path request)
        ]
        <>
        ppBody (requestBody request)

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
