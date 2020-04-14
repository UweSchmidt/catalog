{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.CheckSum
  ( CheckSumRes(..)
  , checkImgPart
  , updateCheckSum
  , updateTimeStamp
  , prettyCSR
  )
where

import           Catalog.Cmd
import           Data.ImgTree
import           Data.Prim
import qualified Data.Aeson      as J

-- ----------------------------------------

data CheckSumRes = CSupdate CheckSum TimeStamp
                 | CSnew    CheckSum
                 | CSerr    CheckSum CheckSum
                 | CSlost
                 | CSok     CheckSum

deriving instance Show CheckSumRes

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

processImgPart :: (Path -> ImgPart -> Cmd r)
               -> Path -> Name -> ImgNode
               -> Cmd r
processImgPart cmd p nm n = do
  maybe
    (abort $ "processImgPart: part not found: "
             <> show nm <> " for " <> show p)
    (cmd p)
    (n ^? theImgPart nm)

checkImgPart :: Bool -> Path -> Name -> ImgNode -> Cmd CheckSumRes
checkImgPart onlyUpdate = processImgPart (checkImgPart' onlyUpdate)

checkImgPart' :: Bool -> Path -> ImgPart -> Cmd CheckSumRes
checkImgPart' onlyUpdate p ip = do
  fsp <- path2SysPath (substPathName (ip ^. theImgName) p)
  fex <- fileExist fsp
  if not fex
    then return CSlost
    else do
      fts <- fsTimeStamp <$> fsFileStat fsp

      if onlyUpdate
         &&
        fts == ts0 && not (isempty cs0)
        then return $ CSok cs0
        else do
          fcs <- checksumFile fsp
          return $
            check fts fcs

  where
    ts0 = ip ^. theImgTimeStamp
    cs0 = ip ^. theImgCheckSum

    check ts1 cs1
      | ts1 > ts0   = CSupdate cs1 ts1    -- file modified since last check
      | isempty cs0 = CSnew    cs1        -- checksum not yet computed
      | cs1 /= cs0  = CSerr    cs1 cs0    -- file corrupted
      | otherwise   = CSok     cs0

updateCheckSum :: ObjId -> Name -> CheckSum -> Cmd ()
updateCheckSum i n cs = do
  adjustImg updCS i
  where
    updCS :: ImgParts -> ImgParts
    updCS ps = ps & isoImgPartsMap . ix n . theImgCheckSum .~ cs

updateTimeStamp :: ObjId -> Name -> TimeStamp -> Cmd ()
updateTimeStamp i n ts = do
  adjustImg updTS i
  where
    updTS :: ImgParts -> ImgParts
    updTS ps = ps & isoImgPartsMap . ix n . theImgTimeStamp .~ ts

-- ----------------------------------------
