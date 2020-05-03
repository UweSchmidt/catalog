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

    -- derived commands
  , checksumFile

    -- aux
  , defaultPath
  )
where

import Polysemy
import Polysemy.Consume
import Polysemy.FileSystem
import Polysemy.Error
import Polysemy.Logging
import Polysemy.Reader
import Polysemy.State

-- import System.HttpRequest

import Client.Commands

import Server.Commands
-- import Server.Commands.ClientInterpreter

import Data.Prim
import Data.ImgNode hiding (theMetaData)
import Data.MetaData

import Catalog.CheckSum
       ( CheckSumRes(..)
       , prettyCSR
       )

import Catalog.FilePath
       ( isoPicNo )

import Catalog.Workflow
       ( isoPathPos )


-- import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.Text            as T
-- import qualified Data.Text.Encoding   as T

------------------------------------------------------------------------------

type CCmdEffects r =
  (Members '[Consume Text, Error Text, SCommand, Logging, FileSystem] r)

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

    CcSnapshot msg ->
      snapshot msg defaultPath

    CcCheckSum p part onlyUpdate onlyMissing ->
      runReader (CSEnv onlyUpdate onlyMissing False) $
         evalCheckSum prettyCheckSumRes p part

    CcUpdCSum p part onlyUpdate forceUpdate ->
      runReader (CSEnv onlyUpdate True forceUpdate) $
         evalCheckSum updateCheckSumRes p part

{-# INLINE evalCCommands #-}

------------------------------------------------------------------------------

evalLs :: CCmdEffects r => Path -> Sem r [Path]
evalLs p = do
  subcols <$> theEntry p
  where
    subcols :: ImgNodeP -> [Path]
    subcols n
      | isCOL  n  =        n ^.. theColEntries . traverse . theColColRef
      | isROOT n  =        n ^.. theRootImgCol <> n ^.. theRootImgDir
      | isDIR  n  = sort $ n ^.. theDirEntries . isoDirEntries . traverse
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
  let px = "docs" +/+ rt ^. isoText +/+ geo'
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
          d' = d <> p ^. isoText

          -- dli :: Path -> Int -> Path -> Name -> Sem r ()
          dli dpath pos _ipath _ipart = do
            px <- genSqn

            let pn = pos ^. isoPicNo . isoText
            let sp = d' +/+ (px <> pn <> ".jpg")
            let pp = dpath `snocPath` (isoText # pn)

            log'trc $ untext [ "evalDownload:"
                             , "download JPG image copy to"
                             , sp
                             ]

            unless overwrite $
              whenM (fileExist sp) $
                void $
                abortWith $
                untext [ "evalDownload:"
                       , "file for download already exists"
                       , sp <> ","
                       , "cleanup download dir or use -f (--force) option"
                       ]

            lbs <- jpgImgCopy rt geo pp
            writeFileLB sp lbs


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

data CSEnv = CSEnv { csOnlyUpdate  :: Bool
                   , csOnlyMissing :: Bool
                   , csForceUpdate :: Bool
                   }

type CSEffects r = ( Member (Reader CSEnv) r
                   , Member (Consume Text) r
                   , Member Logging r
                   , Member SCommand r
                   )

type CSCmd r a  = Path -> Name -> CheckSumRes -> Sem r a

--------------------

evalCheckSum :: CSEffects r
             => CSCmd r () -> Path -> Name -> Sem r ()

evalCheckSum k p part
  | isempty part = theEntry p >>= evalCheckSum' k p
  | otherwise    = evalCheckSumPart k p part


evalCheckSum' :: CSEffects r
              =>  CSCmd r () -> Path -> ImgNodeP -> Sem r ()
evalCheckSum' k p e
  | isIMG e = do
      mconcat <$>
        traverse
          (evalCheckSumPart k p)
          (e ^.. theParts . isoImgParts . traverse . theImgName)

  | isDIR e = do
      writeln $ untext [p ^. isoText <> ":", "CHECK dir"]

      mconcat <$>
        traverse
          (\ p' -> theEntry p' >>= evalCheckSum' k p')
          (sort $ e ^.. theDirEntries . isoDirEntries . traverse)
          -- ^  traversal with ascending path names

  | otherwise = pure ()


evalCheckSumPart :: CSEffects r
                 => CSCmd r () -> Path -> Name -> Sem r ()

evalCheckSumPart k p part = do
  onlyUpdate <- asks @CSEnv csOnlyUpdate

  log'trc $ untext ["evalCCheckSumPart:"
                   , p ^. isoText
                   , "part:"
                   , part ^. isoText
                   ]

  r <- checkImgPart onlyUpdate part p

  log'trc $ untext ["res =", show r ^. isoText]

  k p part r

----------------------------------------
--
-- output for CcUpdCSum

updateCheckSumRes :: CSEffects r => CSCmd r ()

updateCheckSumRes p part csr = do
  let issue = prettyCheckSumRes p part csr

  case csr of
    CSupdate cs'new ts'new -> do
      updateCheckSum  cs'new part p
      updateTimeStamp ts'new part p
      issue

    CSnew cs'new -> do
      updateCheckSum  cs'new part p
      issue

    CSerr cs'new _cs'old -> do
      forceUpdate <- asks @CSEnv csForceUpdate
      when forceUpdate $
        updateCheckSum  cs'new part p
      issue

    CSlost -> do
      issue

    CSok _cs'new ->
      pure ()

----------------------------------------
--
-- output for CcCheckSum command

prettyCheckSumRes :: CSEffects r => CSCmd r ()

prettyCheckSumRes p part csr = do
  onlyMissing <- asks @CSEnv csOnlyMissing

  case (onlyMissing, csr) of
    (True, CSok _) -> return ()         -- only errors are issued
    _              -> writeln $
                      untext [ substPathName part p ^. isoText <> ":"
                             , prettyCSR csr ^. isoText
                             ]

------------------------------------------------------------------------------

-- don't use lazy bytestrings, this leads to space leaks
-- with very large files (> 1Gb, e.g. .afphoto stacks)


checksumFile :: ( Member FileSystem r )
             => TextPath -> Sem r CheckSum
checksumFile p = do
  r <- mkCheckSum <$> readFileBS p
  return $! r

------------------------------------------------------------------------------

defaultPath :: Path
defaultPath = "/archive"

------------------------------------------------------------------------------
