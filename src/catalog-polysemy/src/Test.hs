module Test
where

import Data.Prim
import Data.ImgNode
import Data.RefTree
import Data.ImgTree
import Data.NavTree
import Data.ImageStore

import Catalog.Effects
import Catalog.Run
import Catalog.CatEnv
import Catalog.CatalogIO
import Catalog.ImgTree.Modify
import Catalog.Logging
import Catalog.ImgTree.Access
import Catalog.ImgTree.Modify

-- simple tests

testCatEnv :: CatEnv
testCatEnv =
  defaultCatEnv
  & catMountPath   .~ "../../data"
  & catJsonArchive .~ "catalog/photos-0.5.hashid.json"

testAppEnv :: AppEnv
testAppEnv =
  defaultAppEnv
  & appEnvCat .~ testCatEnv

loadCatalog :: IO NavTree
loadCatalog = do
  (is, res) <- runApp emptyImgStore testAppEnv load
  print res
  return
   (is ^. theImgTree . to mkNavTree)
    where
      load = loadImgStore (testCatEnv ^. catJsonArchive)

cleanupAllRefs :: Eff'ISEJL r => Sem r ()
cleanupAllRefs = do
  t <- mkNavTree <$> dt
  sequenceA_ $
    cleanupUndefRefs setDirEntries
                     setColEntries
                     clearColImg
                     clearColBlog
                     (allUndefinedObjIds t)
                     t
  where
    setDirEntries ref des = adjustDirEntries (const des)     ref
    setColEntries ref ces = adjustColEntries (const ces)     ref
    clearColBlog  ref     = adjustColBlog    (const Nothing) ref
    clearColImg   ref     = adjustColImg     (const Nothing) ref

------------------------------------------------------------------------------
