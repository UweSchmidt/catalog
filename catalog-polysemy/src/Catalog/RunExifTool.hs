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

module Catalog.RunExifTool
  -- ( getExifMetaData )
where

import Catalog.Effects

import Data.MetaData
import Data.Prim

import System.ExecProg

import qualified Data.Aeson as J
import qualified Data.Text  as T

-- ----------------------------------------

type EffExecProg r = Member ExecProg r

type SemFSEX   r a = ( EffFileSys r
                     , EffExecProg r
                     ) => Sem r a

-- ----------------------------------------

getExifMetaData :: ( EffExecProg r
                   , EffFileSys  r
                   , EffLogging  r
                   , EffError    r
                   )
                => SysTextPath
                -> Sem r MetaData
getExifMetaData sp = do
  ex <- fileExist p
  if ex
    then do
      bs <- callExifProg p
      bsToMetaData bs
    else do
      log'warn $ "exiftool: file not found: " <> p
      return mempty
  where
    p = sp ^. isoTextPath

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
  either (throw @Text . ("getExifTool: " <>) . T.pack) return
  .
  J.eitherDecodeStrict'

------------------------------------------------------------------------
