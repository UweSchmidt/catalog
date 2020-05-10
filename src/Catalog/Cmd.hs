{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Cmd
       ( module Catalog.Cmd
       , module Catalog.Cmd.Types
       , module Catalog.Cmd.Basic
       , module Catalog.Cmd.Fold
       , module Catalog.Cmd.Invariant
       , module Catalog.Cmd.List
       , module Catalog.Cmd.CopyRemove
       , module Catalog.Cmd.ArchiveCollection
       , module Catalog.System.CatalogIO
       , module Catalog.System.IO
       , module Control.Monad.ReaderStateErrIO
       )
where

import           Catalog.Cmd.ArchiveCollection
import           Catalog.Cmd.Basic
import           Catalog.Cmd.CopyRemove
import           Catalog.Cmd.Fold
import           Catalog.Cmd.Invariant
import           Catalog.Cmd.List
import           Catalog.Cmd.Types
import           Catalog.Journal
import           Catalog.System.IO
import           Catalog.System.CatalogIO
import           Control.Lens
import           Control.Monad.ReaderStateErrIO
import           Data.ImageStore
import           Data.ImgTree
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

initEnv :: IO Env
initEnv =
  return defaultEnv


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
