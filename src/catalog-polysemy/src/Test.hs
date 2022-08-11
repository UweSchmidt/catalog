module Test
where

import Data.Prim
import Data.ImgNode
import Data.RefTree
import Data.ImgTree
import Data.NavTree
import Data.ImageStore

import Catalog.Run
import Catalog.CatEnv
import Catalog.CatalogIO

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

------------------------------------------------------------------------------
