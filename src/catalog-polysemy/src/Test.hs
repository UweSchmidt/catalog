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

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Text       as T

-- simple tests

testCatEnv :: CatEnv
testCatEnv =
  defaultCatEnv
  & catMountPath   .~ "../../data"
  & catJsonArchive .~ "catalog/photos-0.5.hashid.json"

testAppEnv :: AppEnv
testAppEnv =
  defaultAppEnv
  & appEnvCat      .~ testCatEnv
  & appEnvLogLevel .~ LogTrc
  & appEnvJournal  .~ Nothing

loadCatalog :: IO NavTree
loadCatalog = do
  (is, res) <- runApp emptyImgStore testAppEnv load
  print res
  return
   (is ^. theImgTree . to mkNavTree)
    where
      load = do
        loadImgStore (testCatEnv ^. catJsonArchive)
        checkInvImgTree
        statsImgTree

-- --------------------

cleanupAllRefs :: Eff'ISEJL r => ObjIds -> Sem r ()
cleanupAllRefs rs = do
  log'warn "cleanupAllRefs: remove undefined refs from COL and DIR nodes"
  t <- mkNavTree <$> dt

  let cmds = cleanupUndefRefs setDirEntries
                     setColEntries
                     clearColImg
                     clearColBlog
                     rs
                     t
  log'trc $ "cleanupAllRefs: # of edit cmds: " <> length cmds ^. isoText
  sequenceA_ cmds

  where
    setDirEntries ref des = do
      log'ref "set new DIR entries" ref
      adjustDirEntries (const des) ref

    setColEntries ref ces = do
      log'ref "set new COL entries" ref
      adjustColEntries (const ces) ref

    clearColBlog ref = do
      log'ref "clear COL blog ref" ref
      adjustColBlog (const Nothing) ref

    clearColImg ref = do
      log'ref "clear COL img ref" ref
      adjustColImg (const Nothing) ref

    log'ref txt ref =
      log'trc $ "    ref: " <> fmtR ref <> " " <> txt

-- --------------------

checkInvImgTree :: Eff'ISEJL r => Sem r ()
checkInvImgTree = do
  log'info "ImgTree invariant check"

  t <- mkNavTree <$> dt

  asRootNode    $ hasRootNode    t
  asRootColNode $ hasRootColNode t
  asRootDirNode $ hasRootDirNode t

  let undefs    = noUndefinedIds t
  asUndefinedIds undefs

  -- remove undefs from tree
  t1 <- case undefs of
          Just rs -> do
            cleanupAllRefs rs
            mkNavTree <$> dt
          Nothing -> return t

  asJunkInDir   $ noJunkInDirs t1
  asJunkInCol   $ noJunkInCols t1

  asUplink      $ uplinkCheck  t1

  log'info "ImgTree invariant check done"

  return ()
  where
    asRootNode    = check
                    "check root ref"
                    "root ref doesn't point to a ROOT node"
                    fmtNode

    asRootColNode = check
                    "check root collection ref"
                    "root collection ref doesn't point to a COL node"
                    fmtNode

    asRootDirNode = check
                    "check root directory ref"
                    "root dir ref doesn't point to a DIR node"
                    fmtNode

    asUndefinedIds = check
                     "check undefined refs"
                     "undefined ref(s) found "
                     fmtObjIds

    asJunkInDir    = check
                     "check referencee in DIR nodes"
                     "none IMG or none DIR node(s) found in dir hierachy"
                     fmtTrees

    asJunkInCol    = check
                     "check references and image names in COL entries"
                     "col entry with illegal ref or undefined part name"
                     fmtIdColEntries

    asUplink       = check
                     "check parent refs"
                     "parent ref(s) don't point to parent entries"
                     fmtRefPairs

    check :: Eff'ISEL r
          => Text -> Text -> (a -> [Text])
          -> Maybe a
          -> Sem r ()
    check tt msg f res = do
      case res of
        Nothing ->           log'trc  (l1 <> ": O.K.")
        Just v  -> traverse_ log'warn (l1 : l2 : f v)
        where
          l1 = "checkInvImgTree: " <> tt
          l2 = "checkInvImgTree: " <> msg

    fmtNode :: ImgNode -> [Text]
    fmtNode _n = mempty

    fmtObjIds :: ObjIds -> [Text]
    fmtObjIds = map (indent . ("ref: " <>) . fmtR) . S.toList

    fmtTrees :: [NavTree] -> [Text]
    fmtTrees = map (indent . fmtT)
      where
        fmtT t =
          "ref: " <> t ^. _2 . to fmtR
          <>
          ", node: " <>  t ^. curNode . to (take 36 . show) . isoText

    fmtRefPairs :: [(ObjId, ObjId)] -> [Text]
    fmtRefPairs = map (indent . fmtP)
      where
        fmtP (r, pr) =
          "ref: " <> fmtR r <> ", wrong parent ref: " <> fmtR pr

    fmtIdColEntries :: [(ObjId, ColEntry)] -> [Text]
    fmtIdColEntries = map (indent . fmtIdColEntry)

    fmtIdColEntry :: (ObjId, ColEntry) -> Text
    fmtIdColEntry (r', e') =
      "ref: " <> fmtR r'
      <>
      ", entry: " <> e' ^. to show . isoText

fmtR :: ObjId -> Text
fmtR r = show r ^. isoText

indent :: Text -> Text
indent = ("  " <>)

------------------------------------------------------------------------------

type NodeStats = (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int)

nodeStats :: Getter ImgNode NodeStats
nodeStats = to f
  where
    f ROOT{} = (Sum 1, mempty, mempty, mempty, mempty)
    f COL{}  = (mempty, Sum 1, mempty, mempty, mempty)
    f DIR{}  = (mempty, mempty, Sum 1, mempty, mempty)
    f IMG{}  = (mempty, mempty, mempty, Sum 1, mempty)
    f _      = (mempty, mempty, mempty, mempty, Sum 1)

imgTreeStats :: Fold NavTree NodeStats
imgTreeStats = allEntries . curNode . nodeStats

statsImgTree :: Eff'ISEL r => Sem r ()
statsImgTree = do
  t <- mkNavTree <$> dt

  let (nr, nc, nd, ni, nu) = t ^. imgTreeStats
  logStat "ROOT" nr
  logStat "COL " nc
  logStat "DIR " nd
  logStat "IMG " ni
  logStat "NULL" nu
  return ()

  where
    logStat t (Sum n) =
      log'info $ "# of " <> t <> " node refs: " <> n ^. isoText



-- -----------------------------------------
