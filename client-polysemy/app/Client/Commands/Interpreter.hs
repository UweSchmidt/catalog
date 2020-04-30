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
import Polysemy.Error
import Polysemy.Logging
import Polysemy.State

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

type CCmdEffects r = (Members '[Consume Text, Error Text, SCommand, Logging] r)

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

    CcDownload p rt geo dir withSeqNo overwrite ->
      evalDownload p rt geo dir withSeqNo overwrite

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
infixr 6 +/+

(+/+) :: Text -> Text -> Text
t1 +/+ t2 = t1 <> "/" <> t2

evalDownload :: CCmdEffects r
             => Path -> ReqType -> Geo
             -> Text -> Bool    -> Bool
             -> Sem r ()
evalDownload p rt geo d withSeqNo overwrite = do
  let geo'
        | geo == geo'org = orgGeo ^. isoText
        | otherwise      = geo    ^. isoText
  let px = "docs" +/+ rt ^. isoText <> geo'
  let d1 = d +/+ px

  dx <- dirExist d
  if dx
    then fmap snd . runState (0::Int)
         $ evalDownload1 rt geo d1 (nextSqn withSeqNo) overwrite p
    else abortWith $ "Download dir not found: " <> d

evalDownload1 :: ( CCmdEffects r
                 , Member (State Int) r
                 )
              => ReqType -> Geo
              -> Text -> GenSeqNum r -> Bool -> Path
              -> Sem r ()
evalDownload1 rt geo d genSqn overwrite = evalDownload'
  where
    evalDownload' p = do
      let d' = d <> p ^. isoText

      log'trc $ untext [ "evalCDownload:"
                       , p ^. isoText
                       ]

      -- get collection from server
      n <- theEntry p
      unless (isCOL n) $
        abortWith $ untext [ "evalCDownload:"
                           , "no collection found for path"
                           , p ^. isoText
                           ]

      -- create download subdir
      unlessM (dirExist d') $ do
        log'trc $ untext [ "evalCDownload: create dir"
                         , d'
                         ]

      createDir d'

      -- download all collection entries
      forM_ (zip [(0::Int) ..] $ n ^. theColEntries . isoSeqList) $
        \ (i, ce) -> colEntry (dli p i) evalDownload' ce

        where
          dli :: Path -> Int -> Path -> Name -> Sem r ()
          dli dpath pos _ipath _ipart = do
            undefined


createDir :: Text -> Sem r ()
createDir = undefined

dirExist :: Text -> Sem r Bool
dirExist = undefined

--------------------
--
-- generate sequence numbers for downloaded images
-- to retain download sequence in filenames

type GenSeqNum r = Member (State Int) r => Sem r Text

nextSqn :: Member (State Int) r => Bool -> Sem r Text

nextSqn False =
  pure ""

nextSqn True = do
  n <- get @Int
  let n1 = n + 1
  put n1
  pure $ toS n1
  where
    toS i = (replicate l '0' <> s <> "-") ^. isoText
      where
        s = show i
        l = (4 - length s) `max` 0


------------------------------------------------------------------------------
