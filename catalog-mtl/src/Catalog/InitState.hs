{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.InitState
       (initState)
where

import           Catalog.Cmd.ArchiveCollection
import           Catalog.Cmd.Basic
import           Catalog.Cmd.Types
import           Catalog.System.CatalogIO

import           Control.Lens

import           Data.ImageStore
import           Data.ImgTree
import           Data.Journal
import           Data.Prim

-- ----------------------------------------

initImgStore :: Name -> Name -> Name -> Cmd ()
initImgStore rootName colName dirName = do
  r <- liftE $
    mkEmptyImgRoot rootName dirName colName
  put $ mkImgStore r mempty
  journalChange $ InitImgStore rootName colName dirName

-- ----------------------------------------
--
-- initialization on program start

initState :: Env -> IO (Either String ImgStore)
initState env = do
  (res, store) <- runCmd' env $ do
    jp' <- view envJsonArchive
    initImgStore n'archive n'collections n'photos
    loadImgStore jp'
    genSysCollections
    setCatMetaData
  case res of
    Left msg ->
      return (Left $ show msg)
    Right () ->
      return (Right store)

-- ----------------------------------------
