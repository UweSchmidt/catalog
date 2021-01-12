{-# LANGUAGE
    ConstraintKinds,
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

module Client.Effects.ClientCmd.Interpreter
  ( -- * Interpreter
    evalClientCmd

    -- derived commands
  , checksumFile

    -- aux
  , defaultPath
  )
where

-- polysemy and polysemy-tool
import Polysemy
import Polysemy.Consume
import Polysemy.FileSystem
import Polysemy.Error
import Polysemy.Logging
import Polysemy.Reader
import Polysemy.State

-- catalog-data
import Data.Prim
import Data.ImgNode hiding (theMetaData)
import Data.MetaData ( MetaKey
                     , MetaData
                     , isoMetaDataMDT
                     , metaTextAt
                     , filterKeysMD
                     , prettyMD
                     )

import Text.SimpleParser
       ( parseMaybe
       , parseGlob
       )

-- catalog-polysemy
import Catalog.Effects.CatCmd

-- client-polysemy
import Client.Effects.ClientCmd

-- libraries
import qualified Data.Aeson.Encode.Pretty as J
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

------------------------------------------------------------------------------

type CCmdEffects r =
  (Members '[Consume Text, Error Text, CatCmd, Logging, FileSystem] r)

evalClientCmd :: CCmdEffects r => InterpreterFor ClientCmd r
evalClientCmd =
  interpret $
  \ c -> case c of
    CcGlob p -> do
      ps <- globExpand p
      traverse_ (\ p' -> writeln $ p' ^. isoText) ps

    CcEntry p -> do
      ps <- globExpand p
      traverse_ (\ p' -> do n <- theEntry p'
                            writeln $ p' ^. isoText
                            writeln $ viaJsonToText n
                ) ps

    CcLsSub p -> do
      ps <- evalGlobLs p
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

    CcCheckSum p part onlyUpdate onlyMissing -> do
      ps <- globExpand p
      runReader (CSEnv onlyUpdate onlyMissing False) $
         evalCheckSums prettyCheckSumRes ps part

    CcUpdCSum p part onlyUpdate forceUpdate -> do
      ps <- globExpand p
      runReader (CSEnv onlyUpdate True forceUpdate) $
         evalCheckSums updateCheckSumRes ps part

    CcMediaPath p -> do
      mps <- evalMediaPath p
      sequenceA_ . map (writeln . (^. isoText)) $ mps

    CcUndoList -> do
      es <- listUndoEntries
      sequenceA_ . map (uncurry prettyUndo) $ es

{-# INLINE evalClientCmd #-}

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

evalLss :: CCmdEffects r => [Path] -> Sem r [Path]
evalLss ps = concat <$> traverse evalLs ps

{-# INLINE evalLss #-}

------------------------------------------------------------------------------
--
-- evalLs with glob style wildcards

evalGlobLs :: CCmdEffects r => Path -> Sem r [Path]
evalGlobLs = globExpand >=> evalLss

------------------------------------------------------------------------------
--
-- expand glob style pattern

globExpand :: CCmdEffects r => Path -> Sem r [Path]
globExpand p = do
  ps <- globExpand' (listFromPath p) [defaultPath]
  return $ if null ps
           then [p]
           else ps
  where
    globExpand' []         = return
    globExpand' (gl : gls) = cont gls . filterGlob gl
      where
        cont []   = return
        cont gls' = evalLss >=> globExpand' gls'

filterGlob :: Text -> [Path] -> [Path]
filterGlob globPattern = filter (matchGlob globPattern)

matchGlob :: Text -> (Path -> Bool)
matchGlob globPattern =
  case parseMaybe parseGlob (globPattern ^. isoString) of
    Nothing  -> const False
    Just prs -> (\ p -> isJust . parseMaybe prs $ lastPath p ^. isoString)

------------------------------------------------------------------------------

evalMediaPath :: CCmdEffects r => Path -> Sem r [Path]
evalMediaPath p = do
  log'trc $ untext ["evalMediaPath:", p ^. isoText]
  rs <- theMediaPath p
  log'trc $ ("res = " <> show rs) ^. isoText
  return rs

------------------------------------------------------------------------------

evalMetaData :: CCmdEffects r => PathPos -> [MetaKey] -> Sem r MetaData
evalMetaData pp@(p, cx) keys = do
  log'trc $ untext ["evalMetaData:", from isoText . isoPathPos # pp]
  r <- (\ mt ->  (isoMetaDataMDT # mt) & filterKeysMD (`elem` keys))
       <$>
       theMetaData (fromMaybe (-1) cx) p
  log'trc $ untext ["res =", show r ^. isoText]
  return r

------------------------------------------------------------------------------

evalSetMetaData1 :: CCmdEffects r => PathPos -> MetaKey -> Text -> Sem r ()
evalSetMetaData1 pp@(p, cx) key val = do
  log'trc $ untext [ "evalSetMetaData:"
                   , from isoText . isoPathPos # pp
                   , key ^. isoText
                   , val
                   ]
  let md1 = mempty & metaTextAt key .~ val
  _r <- setMetaData1 (fromMaybe (-1) cx) (md1 ^. isoMetaDataMDT) p
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
    then
    do ps <- globExpand p
       fmap snd . runState (0::Int)
         $ traverse_ (evalDownload1 rt geo d1 (nextSqn withSeqNo) overwrite) ps
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
                   , Member CatCmd r
                   )

type CSCmd r a  = Path -> Name -> CheckSumRes -> Sem r a

--------------------

evalCheckSums :: CSEffects r
              => CSCmd r () -> [Path] -> Name -> Sem r ()
evalCheckSums k ps part = do
  traverse_ (\ p' -> evalCheckSum k p' part) ps

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


checksumFile :: Member FileSystem r
             => TextPath -> Sem r CheckSum
checksumFile p = do
  r <- mkCheckSum <$> readFileBS p
  return $! r

------------------------------------------------------------------------------

prettyUndo :: Member (Consume Text) r
           => HistoryID -> Text -> Sem r ()
prettyUndo hid cmt = writeln $ (show hid ++ ". ") ^. isoText <> cmt

------------------------------------------------------------------------------

defaultPath :: Path
defaultPath = p'archive

------------------------------------------------------------------------------

viaJsonToText :: ToJSON a => a -> Text
viaJsonToText = TL.toStrict . TL.decodeUtf8 . J.encodePretty' conf
  where
    conf = J.defConfig
           { J.confIndent  = J.Spaces 2 }

------------------------------------------------------------------------------
