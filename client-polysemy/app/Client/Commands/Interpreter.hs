{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE
    DataKinds,
    FlexibleContexts,
    GADTs,
    OverloadedStrings,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TemplateHaskell,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Client.Commands.Interpreter
  ( -- * Interpreter
    evalCCommands
  )
where

import Polysemy
import Polysemy.Consume
-- import Polysemy.Error
import Polysemy.Logging
-- import Polysemy.Reader

-- import System.HttpRequest

import Client.Commands

import Server.Commands
-- import Server.Commands.ClientInterpreter

import Data.Prim
import Data.ImgNode hiding (theMetaData)
import Data.MetaData

import Catalog.Workflow
       ( imgReqTypes
       , isoPathPos
       )


-- import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.Text            as T
-- import qualified Data.Text.Encoding   as T

------------------------------------------------------------------------------

type CCmdEffects r = (Members '[Consume Text, SCommand, Logging] r)

evalCCommands :: CCmdEffects r => InterpreterFor CCommand r
evalCCommands =
  interpret $
  \ c -> case c of
    CcEntry p -> do
      n <- theEntry p
      writeln $ show n ^. isoText

    CcLs p -> do
      ps <- evalLs p
      sequenceA_ . map (writeln . (^. isoText)) $ ps

    CcLsmd pp keys -> do
      md <- evalMetaData pp keys
      sequenceA_ . map (writeln . (^. isoText)) $ prettyMD md

    CcSetmd1 pp key val ->
      evalSetMetaData1 pp key val

    CcDelmd1 pp key ->
      evalSetMetaData1 pp key "-"

{-# INLINE evalCCommands #-}

------------------------------------------------------------------------------

evalLs :: CCmdEffects r => Path -> Sem r [Path]
evalLs p = do
  subcols <$> theEntry p
  where
    subcols :: ImgNodeP -> [Path]
    subcols n
      | isCOL  n  = n ^.. theColEntries . traverse . theColColRef
      | isROOT n  = n ^.. theRootImgCol <> n ^.. theRootImgDir
      | isDIR  n  = n ^.. theDirEntries . isoDirEntries . traverse
      | otherwise = []

{-# INLINE evalLs #-}

------------------------------------------------------------------------------

evalMetaData :: CCmdEffects r => PathPos -> [Name] -> Sem r MetaData
evalMetaData pp@(p, cx) keys = do
  log'trc $ untext ["evalMetaData:", from isoText . isoPathPos # pp]
  r <- (^. selectByNames keys)
       <$>
       theMetaData (fromMaybe (-1) cx) p
  log'trc $ untext ["res =", show r ^. isoText]
  return r

------------------------------------------------------------------------------

evalSetMetaData1 :: CCmdEffects r => PathPos -> Name -> Text -> Sem r ()
evalSetMetaData1 pp@(p, cx) key val = do
  log'trc $ untext [ "evalSetMetaData:"
                   , from isoText . isoPathPos # pp
                   , key ^. isoText
                   , val
                   ]
  let md1 = mempty & metaDataAt key .~ val
  _r <- setMetaData1 (fromMaybe (-1) cx) md1 p
  return ()

------------------------------------------------------------------------------
