{-# LANGUAGE
    ConstraintKinds,
    DataKinds,
    FlexibleContexts,
    GADTs,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)

{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------

module Catalog.TextPath
where

import Catalog.Data.TextPath (addExt)
import Catalog.CatEnv        (CatEnv, catMountPath)
import Catalog.Effects
import Catalog.ImgTreeAccess (objid2path)

import Data.ImgTree
import Data.Prim

import qualified Data.Text     as T

------------------------------------------------------------------------------
--
-- monadic path to/from file path operations


buildImgPath0 :: ImgRef -> SemIS r TextPath
buildImgPath0 (ImgRef i n) = do
  p <- objid2path i
  return $ substPathName n p ^. isoText

buildImgPath :: ImgRef -> SemIS r TextPath
buildImgPath ir = addExt ".jpg" <$> buildImgPath0 ir

-- ----------------------------------------
--
-- basic ops for files system paths

toSysPath :: TextPath -> SemCE r SysTextPath
toSysPath tp
  | hasSlash tp = do
      mp <- (^. catMountPath) <$> ask @CatEnv
      return $ isoTextPath # (mp <> tp)
  | otherwise = toSysPath ("/" <> tp)
  where
    hasSlash = ("/" `T.isPrefixOf`)

-- build a file system path from an internal path
-- remove the redundant "/archive" top level dir

path2SysPath :: Path -> SemCE r SysTextPath
path2SysPath p =
  toSysPath $ tailPath p ^. isoText

-- build a file system path from an internal image path
--
-- "/archive/photos/2016/emil"
-- -->
-- "<mountpath>/docs/exif-meta/photos/2016/emil.json"

path2ExifSysPath :: Path -> SemCE r SysTextPath
path2ExifSysPath ip =
  toSysPath $ ps'exifcache ^. isoText <> tailPath ip ^. isoText <> ".json"

------------------------------------------------------------------------------
