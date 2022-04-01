------------------------------------------------------------------------------

module Catalog.TextPath
  ( buildImgPath
  , buildImgPath0
--  , toSysPath
--  , path2SysPath
  , toFileSysPath
  , toFileSysTailPath
  , pxMountPath
  )
where

-- catalog-polysemy
import Catalog.Effects
       ( Sem
       , EffIStore
       , EffCatEnv
       , ask
       , TextPath
       )
import Catalog.CatEnv
       ( CatEnv
       , catMountPath
       )
import Catalog.ImgTree.Access
       ( objid2path )

-- catalog
import Data.ImgTree
       ( ImgRef'(ImgRef)
       , ImgRef
       )
import Data.Prim
       ( IsoText(isoText)
       , Path
       , (^.)
       , tailPath
       , substPathName
       )
import Data.TextPath
       ( addExt
       , (<//>)
       )

------------------------------------------------------------------------------
--
-- monadic path to/from file path operations


buildImgPath0 :: EffIStore r => ImgRef -> Sem r TextPath
buildImgPath0 (ImgRef i n) = do
  p <- objid2path i
  return $ substPathName n p ^. isoText

buildImgPath :: EffIStore r => ImgRef -> Sem r TextPath
buildImgPath ir = addExt ".jpg" <$> buildImgPath0 ir

----------------------------------------
--
-- convert a catalog Path into a TextPath representing
-- the file system path of the referenced catalog entry
-- mount path of the catalog

toFileSysPath :: EffCatEnv r => Path -> Sem r TextPath
toFileSysPath p = do
  env <- ask @CatEnv
  return $ env ^. catMountPath <> p ^. isoText

toFileSysTailPath :: EffCatEnv r => Path -> Sem r TextPath
toFileSysTailPath = toFileSysPath . tailPath

pxMountPath :: EffCatEnv r => TextPath -> Sem r TextPath
pxMountPath tp = do
  env <- ask @CatEnv
  return $ env ^. catMountPath <//> tp


------------------------------------------------------------------------------
