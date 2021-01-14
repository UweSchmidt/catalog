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
import Data.MetaData             ( MetaData
                                 , metaDataAt
                                 , descrRating
                                 , theImgEXIFUpdate
                                 , normMetaData
                                 , splitMetaData
                                 , cleanupOldMetaData
                                 )
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
          -> Sem r (MetaData, MetaData)
getMDpart pf imgPath pt
  | pf (pt ^. theImgType) =
      splitMetaData ty <$> getExifMetaData partPath

  | otherwise =
      return mempty
  where
    ty       = pt ^. theImgType
    tn       = pt ^. theImgName
    partPath = substPathName tn . tailPath $ imgPath

setPartMD ::( EffIStore   r   -- any effects missing?
            , EffError    r
            , EffJournal  r
            , EffLogging  r
            , EffTime     r
            , EffCatEnv   r
            , EffExecProg r
            , EffFileSys  r
            )
          => Path
          -> ObjId
          -> MetaData
          -> ImgPart
          -> Sem r MetaData
setPartMD imgPath i acc pt = do
  (md'i, md'pt) <- ( splitMetaData ty
                     .
                     normMetaData acc ty
                   )
                   <$> getExifMetaData partPath
  adjustPartMetaData (md'pt <>) (ImgRef i tn)
  return (md'i <> acc)
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
      => ObjId
      -> ImgParts
      -> MetaData
      -> Sem r ()
setMD i ps md'old = do
  ip  <- objid2path i
  ts  <- whatTimeIsIt

  -- throw away all old attributes
  -- TODO: maybe thrown away when catalog metadata is updated to new format
  let md0 = cleanupOldMetaData md'old

  -- merge metadata of all image parts with old metadata
  md'new <- foldlMOf traverseParts (setPartMD ip i) md0 ps
  let md' = md'new
            & theImgEXIFUpdate .~ ts
            & metaDataAt descrRating .~ mempty    -- TODO: cleanup
  adjustMetaData (const $ md') i
  log'trc $ msgPath ip "setMD: update exif data done: "

------------------------------------------------------------------------
