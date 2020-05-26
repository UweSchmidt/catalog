{-# LANGUAGE
    ConstraintKinds,
    DataKinds,
    FlexibleContexts,
    GADTs,
    OverloadedStrings,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)
{-# LANGUAGE TupleSections #-}

------------------------------------------------------------------------------

module Catalog.CatalogIO
  ( Eff'CatIO
  , loadImgStore
  , snapshotImgStore
  , saveImgStore
  )
where

-- catalog-polysemy modules
import Catalog.Effects
import Catalog.CatEnv         (CatEnv, catJsonArchive, catSaveBothIx)
import Catalog.ImgTree.Access (mapImgStore2Path, mapImgStore2ObjId)
import Catalog.Invariant      (checkImgStore)
import Catalog.Journal        (journal)
import Catalog.TextPath       (toSysPath)
import Catalog.TimeStamp      (nowAsIso8601)

-- polysemy-tools
import System.ExecProg        (execScript)

-- catalog modules
import Catalog.Version        (version, date)

import Data.ImageStore        (ImgStore, theCatMetaData)
import Data.Journal           (Journal'(LoadImgStore, SaveImgStore))
import Data.MetaData          (metaDataAt, descrCatalogWrite, descrCatalogVersion)
import Data.Prim
import Data.TextPath          (takeDir, splitExtension)

-- libraries

import qualified Data.Aeson               as J
import qualified Data.Aeson.Encode.Pretty as J
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
              & metaDataAt descrCatalogVersion .~ catVer
              & metaDataAt descrCatalogWrite   .~ catWrt

  modify' @ImgStore (\ s -> s & theCatMetaData %~ (catMeta <>))

-- --------------------

encodeJSON :: ToJSON a => a -> LazyByteString
encodeJSON = J.encodePretty' conf
  where
    conf = J.defConfig
           { J.confIndent  = J.Spaces 2 }

-- save the whole image store
-- file path must be a relative path
-- to the mount path

saveImgStore :: Eff'CatIO r => TextPath -> Sem r ()
saveImgStore p = do
  bs <- toBS
  sp <- toSysPath p
  log'verb $ "saveImgStore: save state to " <> toText sp
  writeFileLB (sp ^. isoTextPath) bs
  journal $ SaveImgStore (p ^. isoString)   -- FilePath in Journal def
  where
    toBS
      | isHashIdArchive p =
          encodeJSON <$> get @ImgStore

      | isPathIdArchive p =
          encodeJSON <$> mapImgStore2Path

      | otherwise =
          throw @Text $ "saveImgStore: wrong archive extension in " <> toText p

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
  pt <- (^. isoTextPath) <$> toSysPath f
  ts <- nowAsIso8601
  log'verb $ T.unwords ["bash  []", checkinScript pt ts]
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

loadImgStore :: Eff'CatIO r => TextPath -> Sem r ()
loadImgStore f = do
  sp <- (^. isoTextPath) <$> toSysPath f
  log'verb $ "loadImgStore: load State from " <> sp
  bs <- readFileLB sp

  case fromBS bs of
    Nothing -> do
      throw @Text $
        "loadImgStore: JSON input corrupted: " <> sp

    Just st -> do
      put st
      journal $ LoadImgStore (f ^. isoString)
      -- make a "FS check" and throw away undefined refs
      checkImgStore
  where

    fromBS
      | isPathIdArchive f = fmap mapImgStore2ObjId . J.decode'
      | otherwise         = J.decode'

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
