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

------------------------------------------------------------------------------

module Catalog.MetaData.Exif
  ( setMD
  , getMDpart
  )
where

import Catalog.Effects
import Catalog.ImgTree.Access
import Catalog.ImgTree.Modify
import Catalog.MetaData.ExifTool (getExifMetaData)
import Catalog.TimeStamp

import Data.ImgNode
import Data.MetaData
import Data.Prim

-- ----------------------------------------

--
-- the low level ops
-- working on file system paths

-- add or update metadata of an image
-- by extracting exif data form a raw image
-- a jpg or a sidecar and merge it with given
-- metadata

getMDpart :: ( EffCatEnv   r
             , EffError    r
             , EffLogging  r
             , EffFileSys  r
             , EffExecProg r
             )
          => (ImgType -> Bool)
          -> Path
          -> ImgPart
          -> Sem r MetaData
getMDpart p imgPath pt
  | p $ pt ^. theImgType =
      filterMetaData ty <$> getExifMetaData partPath

  | otherwise =
      return mempty
  where
    ty       = pt ^. theImgType
    tn       = pt ^. theImgName
    partPath = substPathName tn . tailPath $ imgPath

setMD :: ( EffIStore   r   -- any effects missing?
         , EffError    r
         , EffJournal  r
         , EffLogging  r
         , EffTime     r
         , EffCatEnv   r
         , EffExecProg r
         , EffFileSys  r
         )
      => (ImgType -> Bool)
      -> ObjId
      -> [ImgPart]
      -> Sem r Bool
setMD p i ps = do
  ip  <- objid2path i

  -- get old metadata
  md0 <- getMetaData i

  -- merge metadata of all image parts with old metadata
  mdn <- mconcat <$> mapM (getMDpart p ip) ps

  if isempty mdn
    then
    return True
    else
    do let md1 = mdn <> md0
       if md1 /= md0
          ||
          isempty (getEXIFUpdateTime md0)
         then
         -- something has changed since last update
         -- so add timestamp and store new metadata
         do md2 <- flip setEXIFUpdateTime md1 <$> whatTimeIsIt
            adjustMetaData (const md2) i
            log'verb $
              msgPath ip "setMD: update exif data for "
         else
         do log'trc $
              msgPath ip "setMD: no change in exif data "
       return False

------------------------------------------------------------------------
