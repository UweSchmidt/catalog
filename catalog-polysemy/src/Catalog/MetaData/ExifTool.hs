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

------------------------------------------------------------------------------

module Catalog.MetaData.ExifTool
  ( getExifMetaData
  , callExifProg
  )
where

import Catalog.Effects
import Catalog.TextPath  (toFileSysPath)
import Data.MetaData
import Data.Prim

import Polysemy.ExecProg (execProg)

import qualified Data.Aeson as J
import qualified Data.Text  as T

-- ----------------------------------------

getExifMetaData :: ( EffExecProg r
                   , EffCatEnv   r
                   , EffFileSys  r
                   , EffLogging  r
                   , EffError    r
                   )
                => Path
                -> Sem r MetaData
getExifMetaData srcPath = do
  p <- toFileSysPath srcPath
  ex <- fileExist p
  if ex
    then do
      bs <- callExifProg p
      bsToMetaData bs
    else do
      log'warn $ "exiftool: file not found: " <> p
      return mempty
  where
    -- p = sp ^. isoTextPath

callExifProg :: ( EffExecProg r
                , EffError    r
                , EffLogging  r )
             => TextPath -> Sem r ByteString
callExifProg p =
  execProg "exiftool"
             ["-groupNames", "-json", p]
             mempty
  `catch`
  (\ e -> do
      log'warn $ T.unwords ["exiftool failed for", p <> ", error:", e]
      return mempty
  )

bsToMetaData :: ( EffError r
                , EffLogging r )
             => ByteString -> Sem r MetaData
bsToMetaData =
  either (\ e -> do
             log'warn $ ("getExifTool: " <> T.pack e)
             return mempty
         ) return
  .
  J.eitherDecodeStrict'

------------------------------------------------------------------------
