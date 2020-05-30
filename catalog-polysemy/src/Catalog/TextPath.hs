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
  ( buildImgPath
  , buildImgPath0
  , toSysPath
  , path2SysPath
  , toFileSysPath
  )
where

-- catalog-polysemy
import Catalog.Effects
import Catalog.CatEnv         (CatEnv, catMountPath)
import Catalog.ImgTree.Access (objid2path)

-- catalog
import Data.ImgTree
import Data.Prim
import Data.TextPath          (addExt)

-- libraries
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
{-# DEPRECATED toSysPath, path2SysPath "use toFileSysPath instead" #-}
----------------------------------------
--
-- convert a catalog Path into a TextPath representing
-- the file system path of the referenced catalog entry
-- mount path of the catalog

toFileSysPath :: Path -> SemCE r TextPath
toFileSysPath p = do
  env <- ask @CatEnv
  return $ env ^. catMountPath <> p ^. isoText

------------------------------------------------------------------------------
