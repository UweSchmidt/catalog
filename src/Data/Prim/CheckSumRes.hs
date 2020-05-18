{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Data.Prim.CheckSumRes
  ( CheckSumRes(..)
  , prettyCSR
  )
where

import Data.Prim.CheckSum  (CheckSum)
import Data.Prim.TimeStamp (TimeStamp, formatTimeStamp)
import Data.Prim.Prelude

import qualified Data.Aeson as J

-- ----------------------------------------

data CheckSumRes = CSupdate CheckSum TimeStamp
                 | CSnew    CheckSum
                 | CSerr    CheckSum CheckSum
                 | CSlost
                 | CSok     CheckSum

deriving instance Show CheckSumRes

instance ToJSON CheckSumRes where
  toJSON (CSupdate cs ts) = J.object
    [ "CheckSumRes"  J..= ("CSupdate" :: String)
    , "cs"           J..= cs
    , "ts"           J..= ts
    ]
  toJSON (CSnew cs) = J.object
    [ "CheckSumRes"  J..= ("CSnew" :: String)
    , "cs"           J..= cs
    ]
  toJSON (CSerr cs cs'old) = J.object
    [ "CheckSumRes"  J..= ("CSerr" :: String)
    , "cs"           J..= cs
    , "old"          J..= cs'old
    ]
  toJSON CSlost    = J.object
    [ "CheckSumRes"  J..= ("CSlost" :: String)
    ]
  toJSON (CSok cs) = J.object
    [ "CheckSumRes"  J..= ("CSok" :: String)
    , "cs"           J..= cs
    ]

instance FromJSON CheckSumRes where
  parseJSON  = J.withObject "CheckSumRes" $ \ o ->
    do t <- o J..: "CheckSumRes"
       case t :: String of
         "CSupdate" ->
           CSupdate <$> o J..: "cs"
                    <*> o J..: "ts"
         "CSnew" ->
           CSnew    <$> o J..: "cs"
         "CSerr" ->
           CSerr    <$> o J..: "cs"
                    <*> o J..: "old"
         "CSlost" ->
           pure CSlost
         "CSok" ->
           CSok     <$> o J..: "cs"
         _ -> mzero

-- ----------------------------------------

prettyCSR :: CheckSumRes -> String
prettyCSR (CSupdate cs ts) =
  unwords [ "MODIFIED"
          , "checksum:",  cs ^. isoString
          , "timestamp:", ts &  formatTimeStamp
          ]
prettyCSR (CSnew cs) =
  unwords [ "NEW"
          , "checksum:", cs ^. isoString
          ]
prettyCSR (CSerr cs'new cs'old) =
  unwords [ "CORRUPTED"
          , "checksum:", cs'new ^. isoString
          , "expected:", cs'old ^. isoString
          ]
prettyCSR CSlost =
  unwords [ "DELETED"
          ]
prettyCSR (CSok cs) =
  unwords [ "OK"
          , "checksum:", cs ^. isoString
          ]

-- ----------------------------------------
