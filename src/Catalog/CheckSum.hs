{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.CheckSum
  ( checkImgPart
  , updateCheckSum
  , updateTimeStamp
  )
where

import           Catalog.Cmd

import           Data.ImgTree
import           Data.Prim

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
