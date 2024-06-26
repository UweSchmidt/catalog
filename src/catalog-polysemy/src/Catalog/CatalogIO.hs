------------------------------------------------------------------------------

module Catalog.CatalogIO
  ( Eff'CatIO
  , initImgStore
  , loadImgStore
  , snapshotImgStore
  , saveImgStore
  )
where

-- catalog-polysemy modules
import Catalog.Effects
       ( TextPath
       , Sem
       , Eff'ISEJL
       , EffCatEnv
       , EffError
       , EffExecProg
       , EffFileSys
       , EffIStore
       , EffJournal
       , EffLogging
       , EffTime
       , readFileLB
       , writeFileLB
       , log'info
       , log'verb
       , get
       , modify'
       , put
       , liftExcept
       , throw
       , ask
       )
import Catalog.CatEnv
       ( CatEnv
       , catJsonArchive
       , catSaveBothIx
       )
import Catalog.GenCollections
       ( genSysCollections )
import Catalog.ImgTree.Access
       ( mapImgStore2Path
       , mapImgStore2ObjId
       )
import Catalog.Invariant
       ( checkImgStore )
import Catalog.Journal
       ( journal )
import Catalog.TextPath
       ( pxMountPath )
import Catalog.TimeStamp
       ( nowAsIso8601 )

-- polysemy-tools
import Polysemy.ExecProg
       ( execScript )

-- catalog modules
import Catalog.Version
       ( version
       , date
       )

import Data.ImageStore
       ( ImgStore
       , theCatMetaData
       , mkImgStore
       )
import Data.ImgTree
       ( mkEmptyImgRoot )

import Data.Journal
       ( Journal'(LoadImgStore, SaveImgStore, InitImgStore) )

import Data.MetaData
       ( metaTextAt
       , descrCatalogWrite
       , descrCatalogVersion
       )
import Data.Prim

import Data.TextPath
       ( takeDir
       , splitExtension
       )

-- libraries

import qualified Data.Aeson               as J
import qualified Data.Text                as T

-- ----------------------------------------

type Eff'CatIO r = ( EffIStore   r   -- any effects missing?
                   , EffError    r
                   , EffJournal  r
                   , EffLogging  r
                   , EffCatEnv   r
                   , EffTime     r
                   , EffExecProg r
                   , EffFileSys  r
                   )

-- ----------------------------------------
--
-- catalog metadata

setCatMetaData :: (EffIStore r, EffTime r) => Sem r ()
setCatMetaData = do
  let catVer  = unwords [version, date] ^. isoText
  catWrt     <- nowAsIso8601

  let catMeta = mempty
              & metaTextAt descrCatalogVersion .~ catVer
              & metaTextAt descrCatalogWrite   .~ catWrt

  modify' @ImgStore (\ s -> s & theCatMetaData %~ (catMeta <>))

----------------------------------------
--
-- save the whole image store
-- file path must be a relative path
-- to the mount path

saveImgStore :: Eff'CatIO r => TextPath -> Sem r ()
saveImgStore p = do
  sp <- pxMountPath p
  log'info $ "saveImgStore: store catalog into file:  " <> toText sp

  bs <- toBS
  writeFileLB sp bs
  journal $ SaveImgStore p

  log'info $ "saveImgStore: catalog stored into file: " <> toText sp
  where
    toBS
      | isHashIdArchive p =
          prettyJSON [] <$> get @ImgStore

      | isPathIdArchive p =
          prettyJSON [] <$> mapImgStore2Path

      | otherwise =
          throw @Text $ "saveImgStore: wrong archive extension in " <> toText p

----------------------------------------
--
-- take a snapshot of the catalog and store it in a git archive
-- Test: save 2 versions of the catalog
-- with ObjId (hashes) as keys and (more readable) with Path as keys
--
-- both can be loaded during server startup

snapshotImgStore :: Eff'CatIO r => Text -> Sem r ()
snapshotImgStore cmt = do
  pt <- (^. catJsonArchive) <$> ask @CatEnv

  log'verb $ "snapshotImgStore: make a snapshot into " <> pt
  setCatMetaData
  saveImgStore pt
  checkinImgStore cmt pt

  whenM ((^. catSaveBothIx) <$> ask) $ do
    let pt' = switchArchiveName pt
    log'verb $ "snapshotImgStore: make a snapshot into " <> pt'
    saveImgStore pt'
    checkinImgStore cmt pt'

  log'verb $ "snapshotImgStore: snapshot created and checked in into " <> pt


checkinImgStore :: Eff'CatIO r => Text -> TextPath -> Sem r ()
checkinImgStore cmt f = do
  pt <- pxMountPath f
  ts <- nowAsIso8601
  void $ execScript (checkinScript pt ts)
  where
    qt s = "'" <> s <> "'"

    cmt' | cmt == mempty  = cmt
         | otherwise      = ", " <> cmt

    -- the git script is somewhat fragile,
    -- if there are untracked files in the dir
    -- git returns exit code 1, this generates an error
    -- even if everything was o.k.
    checkinScript :: TextPath -> Text -> Text
    checkinScript pt ts =
      T.unlines
      [ "cd " <> qt (takeDir pt)
      , "git add -A"
      , "git diff --quiet --exit-code --cached || " <>
        "git commit -a -m " <> qt ("catalog-server: " <> ts <> ", " <> pt <> cmt')
      ]

----------------------------------------
--
-- load the image store from a .json file

loadImgStore :: Eff'CatIO r => TextPath -> Sem r ()
loadImgStore f = do
  sp <- pxMountPath f
  log'info $ "loadImgStore: load catalog from file " <> sp
  bs <- readFileLB sp

  case fromBS bs of
    Nothing -> do
      throw @Text $
        "loadImgStore: JSON input corrupted: " <> sp

    Just st -> do
      let md = st ^. theCatMetaData
      log'info $
        "loadImgStore: catalog loaded, version " <>
        md ^. metaTextAt descrCatalogVersion <>
        " written at " <>
        md ^. metaTextAt descrCatalogWrite

      put st
      journal $ LoadImgStore f

      -- make a "FS check" and throw away undefined refs
      log'info "loadImgStore: check catalog integrity"
      checkImgStore
      log'info "loadImgStore: catalog loading complete"
  where

    fromBS
      | isPathIdArchive f = fmap mapImgStore2ObjId . J.decode'
      | otherwise         = J.decode'

----------------------------------------

initImgRoot :: Eff'ISEJL r => Name -> Name -> Name -> Sem r ()
initImgRoot rootName colName dirName = do
  root <- liftExcept $ mkEmptyImgRoot rootName dirName colName
  put $ mkImgStore root mempty
  journal $ InitImgStore rootName colName dirName

initImgStore :: Eff'CatIO r => Sem r ()
initImgStore = do
  log'info $ "initImgStore: catalog-polysemy version " <> (isoString # version) <>
             " from " <> (isoString # date)

  env <- ask
  initImgRoot n'archive n'collections n'photos
  loadImgStore (env ^. catJsonArchive)
  genSysCollections
  setCatMetaData

  log'info "initImgStore: server initialized"

----------------------------------------

archiveName :: TextPath -> (TextPath, Text, Text)
archiveName p = (p', e1, e2)
  where
    (p1, e2) = splitExtension p
    (p', e1) = splitExtension p1

isHashIdArchive :: TextPath -> Bool
isHashIdArchive p = e1 == hidx
  where
    (_, e1, _) = archiveName p

isPathIdArchive :: TextPath -> Bool
isPathIdArchive p = e1 == pidx
  where
    (_, e1, _) = archiveName p

switchArchiveName :: TextPath -> TextPath
switchArchiveName p =
  p' <> e1' <> e2'
  where
    (p', e1, e2') = archiveName p
    e1' | e1 == hidx = pidx
        | e1 == pidx = hidx
        | otherwise  = e1

pidx, hidx :: Text
pidx = ".pathid"
hidx = ".hashid"

------------------------------------------------------------------------
