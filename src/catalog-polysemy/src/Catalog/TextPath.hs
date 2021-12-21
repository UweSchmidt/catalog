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
       ( ask
       , SemIS
       , SemCE
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


buildImgPath0 :: ImgRef -> SemIS r TextPath
buildImgPath0 (ImgRef i n) = do
  p <- objid2path i
  return $ substPathName n p ^. isoText

buildImgPath :: ImgRef -> SemIS r TextPath
buildImgPath ir = addExt ".jpg" <$> buildImgPath0 ir

----------------------------------------
--
-- convert a catalog Path into a TextPath representing
-- the file system path of the referenced catalog entry
-- mount path of the catalog

toFileSysPath :: Path -> SemCE r TextPath
toFileSysPath p = do
  env <- ask @CatEnv
  return $ env ^. catMountPath <> p ^. isoText

toFileSysTailPath :: Path -> SemCE r TextPath
toFileSysTailPath = toFileSysPath . tailPath

pxMountPath :: TextPath -> SemCE r TextPath
pxMountPath tp = do
  env <- ask @CatEnv
  return $ env ^. catMountPath <//> tp


------------------------------------------------------------------------------
