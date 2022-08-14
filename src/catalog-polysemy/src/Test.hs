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

import qualified Data.Set  as S
import qualified Data.Text as T
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

invImgTree' :: Eff'ISEL r
            => (Maybe ImgNode   -> Sem r ())
            -> (Maybe ImgNode   -> Sem r ())
            -> (Maybe ImgNode   -> Sem r ())
            -> (Maybe ObjIds    -> Sem r ())
            -> (Maybe [NavTree] -> Sem r ())
            -> (Maybe [(ObjId, ColEntry)] -> Sem r ())
            -> (Maybe [NavTree] -> Sem r ())
            -> NavTree
            -> Sem r ()
invImgTree' = invImgTree

checkInvImgTree :: Eff'ISEL r => Sem r ()
checkInvImgTree = do
  t <- mkNavTree <$> dt
  invImgTree'
    asRootNode
    asRootColNode
    asRootDirNode
    asUndefinedIds
    asJunkInDir
    asJunkInCol
    asUplink
    t
  where
    asRootNode    = check
                    "root ref doesn't point to a ROOT node"
                    fmtNode

    asRootColNode = check
                    "root collection ref doesn't point to a COL node"
                    fmtNode

    asRootDirNode = check
                    "root dir ref doesn't point to a DIR node"
                    fmtNode

    asUndefinedIds = check
                     "undefined refs found "
                     fmtObjIds

    asJunkInDir    = check
                     "none IMG or none DIR nodes found in dir hierachy"
                     fmtTrees

    asJunkInCol    = check
                     "col entry with illegal ref or undefined part name"
                     fmtIdColEntries

    asUplink       = undefined

    check :: Eff'ISEL r => Text -> (a -> Text) -> Maybe a -> Sem r ()
    check msg f = maybe (return ())
                        (\ v -> log'err $ msg <> f v)

    fmtNode :: ImgNode -> Text
    fmtNode _n = mempty

    fmtObjIds :: ObjIds -> Text
    fmtObjIds = T.concat . map (indent . view isoText) . S.toList

    fmtTrees :: [NavTree] -> Text
    fmtTrees = T.concat . map (indent . fmtT)
      where
        fmtT t =
          "ref: " <> t ^. _2 . isoText
          <>
          ", node: " <>  t ^. curNode . to show . isoText

    fmtIdColEntries :: [(ObjId, ColEntry)] -> Text
    fmtIdColEntries = T.concat . map (indent . fmtIdColEntry)

    fmtIdColEntry :: (ObjId, ColEntry) -> Text
    fmtIdColEntry (r', e') =
      "ref: " <> r' ^. isoText
      <>
      ", entry: " <> e' ^. to show . isoText

    indent :: Text -> Text
    indent = ("\n    " <>)

------------------------------------------------------------------------------
