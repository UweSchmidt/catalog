{-# LANGUAGE
    ConstraintKinds,
    DataKinds,
    FlexibleContexts,
    GADTs,
    OverloadedStrings,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)
{-# LANGUAGE TupleSections #-}

------------------------------------------------------------------------------

module Catalog.GenCheckSum
  ( Eff'CheckSum
  , checkImgPart
  , updateCheckSum
  , updateTimeStamp
  )
where

-- catalog-polysemy modules
import Catalog.Effects
import Catalog.ImgTree.Modify
import Catalog.TextPath       (toFileSysTailPath)

-- catalog modules
import Data.ImgNode
import Data.ImgTree
import Data.Prim

-- ----------------------------------------

type Eff'CheckSum r = ( EffIStore   r
                      , EffError    r
                      , EffJournal  r
                      , EffLogging  r
                      , EffCatEnv   r
                      , EffFileSys  r
                      )

-- ----------------------------------------


processImgPart :: EffError r
               => (Path -> ImgPart -> Sem r a)
               -> Path -> Name -> ImgNode
               -> Sem r a
processImgPart cmd p nm n = do
  maybe
    (throw @Text $
      msgPath p ("processImgPart: part not found: " <>  nm ^. isoText <> " for ")
    )
    (cmd p)
    (n ^? theImgPart nm)

checkImgPart :: Eff'CheckSum r => Bool -> Path -> Name -> ImgNode -> Sem r CheckSumRes
checkImgPart onlyUpdate = processImgPart (checkImgPart' onlyUpdate)

checkImgPart' :: Eff'CheckSum r => Bool -> Path -> ImgPart -> Sem r CheckSumRes
checkImgPart' onlyUpdate path ip = do
  fsp <- toFileSysTailPath (substPathName (ip ^. theImgName) path)
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
          fcs <- genChecksumOfFile fsp
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

updateCheckSum :: Eff'ISEJL r => ObjId -> Name -> CheckSum -> Sem r ()
updateCheckSum i n cs = do
  adjustImg updCS i
  where
    updCS :: ImgParts -> ImgParts
    updCS ps = ps & isoImgPartsMap . ix n . theImgCheckSum .~ cs

updateTimeStamp :: Eff'ISEJL r => ObjId -> Name -> TimeStamp -> Sem r ()
updateTimeStamp i n ts = do
  adjustImg updTS i
  where
    updTS :: ImgParts -> ImgParts
    updTS ps = ps & isoImgPartsMap . ix n . theImgTimeStamp .~ ts


-- don't use lazy bytestrings, this leads to space leaks
-- with very large files (> 1Gb, e.g. .afphoto stacks)

genChecksumOfFile :: (EffFileSys r, EffError r) => TextPath -> Sem r CheckSum
genChecksumOfFile p = do
  r <- mkCheckSum <$> readFileBS p
  return $! r

------------------------------------------------------------------------
