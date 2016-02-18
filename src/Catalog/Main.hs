{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Main
where

import           Catalog.Cmd
-- import           Catalog.FilePath
import           Catalog.Sync
import           Catalog.Rules
-- import           Control.Arrow ((***))
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.RWSErrorIO
-- import qualified Data.Aeson as J
-- import qualified Data.Aeson.Encode.Pretty as J
-- import qualified Data.ByteString.Lazy.Char8 as L
-- import           Data.Function.Util
import           Data.ImageStore
import           Data.ImageTree
-- import           Data.List ({-intercalate,-} partition)
-- import           Data.Maybe
-- import           Data.Prim.CheckSum
-- import           Data.Prim.Name
-- import           Data.Prim.Path
-- import           Data.Prim.PathId
-- import           Data.Prim.TimeStamp
import           Data.RefTree
-- import qualified Data.Set as S
-- import           System.FilePath -- ((</>))
-- import           System.Posix (FileStatus)
import qualified System.Posix as X

ccc :: IO (Either Msg (), ImgStore, Log)
ccc = runCmd $ do
  mountPath <- io X.getWorkingDirectory
  initImgStore "archive" "collections" mountPath
  trcCmd cwnPath >> trcCmd cwnLs >> return ()
  saveImgStore ""

  refRoot <- use (theImgTree . rootRef)
  refImg  <- use (theImgTree . theNodeVal refRoot . theRootImgDir)
  cwSet refImg >> trcCmd cwnPath >> trcCmd cwnType >> return ()

  cwe <- we
  refDir1 <- mkImgDir cwe "emil"
  cwSet refDir1 >> trcCmd cwnPath >> trcCmd cwnType >> trcCmd cwnFilePath >> return ()

  cwe' <- we
  pic1 <- mkImg cwe' "pic1"
  pic2 <- mkImg cwe' "pic2"
  trcCmd cwnLs >> return ()

  cwSet pic2 >> trcCmd cwnPath >> trcCmd cwnType >> trcCmd cwnLs >> return ()
  cwe'' <- we
  (mkImg cwe'' "xxx" >> return ()) `catchError` (\ _ -> return ()) -- error

  cwRoot >> trcCmd cwnType >> trcCmd cwnLs >> trcCmd cwnPath >> return ()
--  trcCmd (fromFilePath "/home/uwe/haskell/apps/catalog/emil") >> return ()
  saveImgStore ""
  rmImgNode pic1
  rmImgNode pic2
  rmImgNode refDir1

  idSyncFS refImg
  saveImgStore ""
  trc "save state to c1.json"
  saveImgStore "c1.json"
  trc "load state from c1.json"
  loadImgStore "c1.json"
  saveImgStore ""
  listImages >>= io . putStrLn
  cwnListPaths >>= trc
  cwnListNames >>= trc
  we >>= applyRules buildRules >>= (io . print)
