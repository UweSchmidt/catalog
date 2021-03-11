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
import Polysemy.Delay
import Polysemy.FileSystem
import Polysemy.Error
import Polysemy.Logging
import Polysemy.Reader
import Polysemy.State
import Polysemy.Time

-- catalog-data
import Data.Prim
import Data.ImgNode
import Data.MetaData ( MetaKey
                     , MetaData
                     , isoMetaDataMDT
                     , metaTextAt
                     , metaDataAt
                     , metaMimeType
                     , filterKeysMetaData
                     , prettyMetaData

                       -- MetaKey values
                     , compositeImageSize
                     , fileMimeType
                     , fileName
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
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TL

------------------------------------------------------------------------------

type CCmdEffects r =
  (Members '[Consume Text, Error Text, Logging, FileSystem, Time, Delay, CatCmd] r)

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
      sequenceA_ . map (writeln . (^. isoText)) $ prettyMetaData md

    CcSetmd1 pp key val ->
      evalSetMetaData1 pp key val

    CcDelmd1 pp key ->
      evalSetMetaData1 pp key "-"

    CcSetColImg pp p ->
      evalSetColImg pp p

    CcSetColBlog pp p ->
      evalSetColBlog pp p

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

    CcExifUpdate p recursive force -> do
      ps <- globExpand p
      traverse_ (evalExifUpdate recursive force) ps

    CcCheckMeta p -> do
      ps <- globExpand p
      traverse_ (\ p' -> theEntry p' >>= checkMeta p') ps

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
      | isDIR  n  = sort $ n ^.. theDirEntries . traverse
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

  r <- (\ mt ->  (isoMetaDataMDT # mt) & filterKeysMetaData (`elem` keys))
       <$>
       theMetaDataText (fromMaybe (-1) cx) p

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

evalSetColImg :: CCmdEffects r => PathPos -> Path -> Sem r ()
evalSetColImg pp@(sp, cx) cp = do
  log'trc $ untext [ "evalSetColImg:"
                   , (isoPathPos # pp) ^. isoText
                   , cp ^. isoText
                   ]
  setCollectionImg sp (fromMaybe (-1) cx) cp
  return ()

evalSetColBlog :: CCmdEffects r => PathPos -> Path -> Sem r ()
evalSetColBlog pp@(sp, cx) cp = do
  log'trc $ untext [ "evalSetColImg:"
                   , (isoPathPos # pp) ^. isoText
                   , cp ^. isoText
                   ]
  setCollectionBlog sp (fromMaybe (-1) cx) cp
  return ()

------------------------------------------------------------------------------

evalExifUpdate :: forall r. CCmdEffects r => Bool -> Bool -> Path -> Sem r ()
evalExifUpdate recursive force = exifUpdate
  where
    sync = syncExif False force

    exifUpdate p = do
      log'trc $ untext [ "evalExifUpdate", p ^. isoText]
      n <- theEntry p

      when (isIMG n) $ do           -- exif update IMG entry
        sync p

      when (isDIR n) $ do           -- exif update DIR entry
        writeln $ untext [p ^. isoText <> ":", "update metadata"]

        es <- traverse partEntry (n ^.. theDirEntries . traverse)

        -- sync IMG entries
        traverse_ sync $ lefts es

        -- sync DIR entries recursively
        when recursive $ do
          traverse_ exifUpdate $ rights es

      when (isCOL n || isROOT n) $ do
        log'warn $ untext
          ["EXIF update only for images and dirs, ignored:", p ^. isoText]

    partEntry :: Path -> Sem r (Either Path Path)
    partEntry p = do
      isi <- isIMG <$> theEntry p
      return $ (if isi then Left else Right) p

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
          (sort $ e ^.. theDirEntries . traverse)
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

checkMeta :: forall r. CCmdEffects r => Path -> ImgNodeP -> Sem r ()
checkMeta p e
  | isIMG e = do
      traverse_
        (checkMetaPart mdi)
        (e ^.. theParts . traverseParts . theImgMeta)

  | isDIR e = do
      writeln $ untext [p ^. isoText <> ":", "CHECK dir"]

      traverse_
        (\ p' -> theEntry p' >>= checkMeta p')
        (sort $ e ^.. theDirEntries . traverse)

  | isROOT e = do
      let p' = e ^. theRootImgDir
      theEntry p' >>= checkMeta p'


  | otherwise =
      return ()

  where
    mdi :: MetaData
    mdi = e ^. theMetaData

    checkMetaPart :: CCmdEffects r => MetaData -> MetaData -> Sem r ()
    checkMetaPart _mdi mdp
      | hasSizeMT ty
        &&
        isempty geo  = do
          writeln $ untext [ p ^. isoText
                           , "part=", nm
                           , ": no metadata tag found for "
                           , compositeImageSize ^. isoText
                           ]
      | otherwise = return ()
      where
        nm  = mdp ^. metaTextAt fileName
        ty  = mdp ^. metaDataAt fileMimeType . metaMimeType
        geo = mdp ^. metaDataAt compositeImageSize

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
