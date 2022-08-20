------------------------------------------------------------------------------

module Catalog.MetaData.Exif
  ( setMD
  , getMDpart
  )
where

import Catalog.Effects
       ( Sem
       , EffExecProg
       , EffTime
       , EffLogging
       , EffJournal
       , EffIStore
       , EffFileSys
       , EffError
       , EffCatEnv
       , log'trc
       )
import Catalog.ImgTree.Access
       ( objid2path )

import Catalog.ImgTree.Modify
       ( adjustMetaData
       , adjustPartMetaData
       )
import Catalog.MetaData.ExifTool
       ( getExifMetaData )

import Catalog.TimeStamp
       ( whatTimeIsIt )

import Data.ImgTree
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
          => (MimeType -> Bool)
          -> Path
          -> ImgPart
          -> Sem r (MetaData, MetaData)
getMDpart pf imgPath pt
  | pf (pt ^. theMimeType) =
      splitMetaData ty <$> getExifMetaData partPath

  | otherwise =
      return mempty
  where
    ty       = pt ^. theMimeType
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
  (md'i, md'pt) <- splitMetaData ty
                   .
                   normMetaData ty              -- normalize meta keys
                   .
                   (<> pt ^. theImgMeta)
                   <$>
                   getExifMetaData partPath

  adjustPartMetaData (const md'pt) (ImgRef i tn)
  return (md'i <> acc)
  where
    ty       = pt ^. theMimeType
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

  -- merge metadata of all image parts with old metadata
  md'new <- foldlMOf traverseParts (setPartMD ip i) md'old ps

  -- add EXIF update timestamp
  let md' = md'new & theImgEXIFUpdate .~ ts

  -- set new metadata
  adjustMetaData (const md') i
  log'trc $ msgPath ip "setMD: update exif data done: "

------------------------------------------------------------------------
