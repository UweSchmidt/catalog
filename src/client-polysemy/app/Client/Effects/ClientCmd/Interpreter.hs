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
       ( Member
       , Sem
       , InterpreterFor
       , Members
       , interpret
       )
import Polysemy.Consume
       ( Consume
       , writeln
       )
import Polysemy.FileSystem
       ( TextPath
       , createDir
       , dirExist
       , fileExist
       , readFileBS
       , writeFileLB
       , FileSystem
       )
import Polysemy.Error
       ( Error )

import Polysemy.Logging
       ( Logging
       , abortWith
       , log'err
       , log'trc
       , log'warn
       , log'verb
       , untext
       )
import Polysemy.Reader
       ( Reader
       , asks
       , runReader
       )
import Polysemy.State
       ( State
       , runState
       , get
       , put
       )
import Polysemy.HttpRequest
       ( HttpRequest )

import Polysemy.HttpRequest.SimpleRequests
       ( Request )

-- catalog-data
import Data.Prim

import Data.ImgTree
       ( ImgNodeP )

import Data.ImgNode
       ( colEntryM
       , isCOL
       , isDIR
       , isIMG
       , isROOT
       , isoImgParts
       , theColColRef
       , theColEntries
       , theColEntry
       , theDirEntries
       , theImgMeta
       , theImgName
       , theMetaData
       , theParts
       , theRootImgCol
       , theRootImgDir
       , traverseParts
       )
import Data.MetaData
       ( MetaKey
       , MetaData
       , isoMetaDataMDT
       , metaTextAt
       , metaDataAt
       , metaGPS
       , metaMimeType
       , lookupByKeys
       , filterKeysMetaData
       , prettyMetaData

         -- MetaKey values
       , compositeGPSPosition
       , compositeImageSize
       , descrAddress
       , descrGPSPosition
       , fileMimeType
       , fileName
       )
import Data.History
       ( HistoryID )

import Text.SimpleParser
       ( parseMaybe
       , parseGlobText
       )

-- catalog-polysemy
import Catalog.Effects.CatCmd
       ( applyUndo
       , checkImgPart
       , dropUndoEntries
       , jpgImgCopy
       , listUndoEntries
       , newUndoEntry
       , setCollectionBlog
       , setCollectionImg
       , setMetaData1
       , snapshot
       , syncExif
       , theEntry
       , theMediaPath
       , theMetaDataText
       , updateCheckSum
       , updateTimeStamp
       , htmlPage
       , jsonPage
       , CatCmd
       )

-- client-polysemy
import Client.Effects.ClientCmd
       ( ClientCmd(..) )

import GPS.Effects.GeoLocCmd
       ( Cache
       , lookupGeoCache
       , loadGeoCache
       , saveGeoCache
       )

-- libraries
import qualified Data.Text                as T

------------------------------------------------------------------------------

type CCmdEffects r =
  (Members '[ Consume Text
            , Error Text
            , Logging
            , FileSystem
            , Reader Request
            , HttpRequest
            , Cache GPSposDec GeoAddrList
            , CatCmd
            ] r)

evalClientCmd :: CCmdEffects r => InterpreterFor ClientCmd r
evalClientCmd =
  interpret $
  \ case
    CcGlob p -> do
      ps <- globExpand p
      traverse_ (\ p' -> writeln $ p' ^. isoText) ps

    CcEntry p -> do
      ps <- globExpand p
      traverse_ (\ p' -> do n <- theEntry p'
                            writeln $ p' ^. isoText
                            writeln $ prettyJSONText jPageKeys n
                ) ps

    CcLsSub p -> do
      ps <- evalGlobLs p
      traverse_ (writeln . (^. isoText)) ps

    CcLsmd pp0 keys -> do
      pp <- globExpandPP pp0
      md <- evalMetaData pp keys
      traverse_ (writeln . (^. isoText)) $ prettyMetaData md

    CcSetmd1 pp0 key val -> do
      pp <- globExpandPP pp0
      evalSetMetaData1 pp key val

    CcDelmd1 pp0 key -> do
      pp <- globExpandPP pp0
      evalSetMetaData1 pp key "-"

    CcSetColImg pp p ->
      evalSetColImg pp p

    CcSetColBlog pp p ->
      evalSetColBlog pp p

    CcDownload p rt geo dir withSeqNo overwrite ->
      evalDownload p rt geo dir withSeqNo overwrite

    CcSnapshot msg -> do
      _id <- newUndoEntry $ "snapshot " <> msg
      snapshot msg defaultPath

    CcCheckSum p part onlyUpdate onlyMissing -> do
      ps <- globExpand p
      runReader (CSEnv onlyUpdate onlyMissing False) $
         evalCheckSums prettyCheckSumRes ps part

    CcUpdCSum p part onlyUpdate forceUpdate -> do
      ps  <- globExpand p
      _id <- newUndoEntry $ "update checksum " <> (p ^. isoText)

      runReader (CSEnv onlyUpdate True forceUpdate) $
         evalCheckSums updateCheckSumRes ps part

    CcMediaPath p -> do
      mps <- evalMediaPath p
      traverse_ (writeln . (^. isoText)) mps

    CcUndoList -> do
      es <- listUndoEntries
      traverse_ (uncurry prettyUndo) es

    CcApplyUndo hid -> do
      applyUndo hid

    CcDropUndo hid
      -- drop all edits older than hid
      | hid /= 0 -> do
          dropUndoEntries hid

      -- drop all edits older than last catalog save command
      | otherwise -> do
          es <- listUndoEntries
          let i = maybe 0 fst
                  . uncons
                  . map fst
                  . filter (("save catalog" `T.isPrefixOf`) . snd)
                  $ es
          when (i /= 0) $ do
            dropUndoEntries i

    CcExifUpdate p recursive force -> do
      ps <- globExpand p
      traverse_ (evalExifUpdate recursive force) ps

    CcCheckMeta p -> do
      ps <- globExpand p
      traverse_ (\ p' -> theEntry p' >>= checkMeta p') ps

    CcGeoAddress p force -> do
      loadGeoCache
      _id <- newUndoEntry $ "add geo address " <> (p ^. isoText)
      ps  <- globExpand p
      traverse_ (\ p' -> theEntry p' >>= setGeoAddress force p') ps
      saveGeoCache

    CcGetAddress g -> do
      log'verb $ untext ["get address for GPS coordinate: ", (ppString # g) ^. isoText]
      loadGeoCache
      a <- lookupGeoCache g
      case a of
        Just (GA{_display_name = ad} : _) -> do
          writeln ad
          saveGeoCache
        _ -> log'err $ "no address found for: " <> (ppString # g) ^. isoText

    CcPage path -> do
      showDoc path

{-# INLINE evalClientCmd #-}

------------------------------------------------------------------------------

showDoc :: CCmdEffects r => Path -> Sem r ()
showDoc path
  | Just (rty, geo, rp, _ext) <- parseDocPath path = do
      case rty of
        RJson -> do
          jpage <- jsonPage geo rp
          writeln (prettyJSONText jPageKeys jpage)
        _
          | rty == RPage || rty == RPage1 -> do
              hpage <- htmlPage rty geo rp
              writeln (lbsToText hpage)

        _ -> do
          abortWith $ "doc type not supported: " <> path ^. isoText

  | otherwise =
      abortWith $ "illegal doc path: " <> path ^. isoText

jPageKeys :: [Text]
jPageKeys =
  [ "imgReq"         -- JImgPage
  , "oirGeo"
  , "img"
  , "imgNavRefs"
  , "imgNavImgs"

  , "colDescr"         -- JColPage
  , "navIcons"
  , "c1Icon"
  , "contIcons"
  , "blogCont"       -- JPage
  , "now"

  , "prev"           -- PrevNextPar
  , "next"
  , "par"
  , "fwrd"

  , "eReq"           -- EDescr
  , "eMeta"

  , "rType"          -- ReqType
  , "rPathPos"
  , "rGeo"
  ]

parseDocPath :: Path -> Maybe (ReqType, Geo, Path, Text)
parseDocPath p0
  | Just rty     <- mrty
    , Just rawPath <- checkAndRemExt (ext ^. isoString) p3
    , Just geo     <- mgeo
    , rty `elem` [RPage, RPage1, RJson, RImg, RImgfx, RIcon, RIconp]
    , n0 == "docs"
               = Just (rty, geo, rawPath, ext)
  | otherwise  = Nothing
  where
    (n0, p1) = p0 ^. viewTop
    (n1, p2) = p1 ^. viewTop
    (n2, p3) = p2 ^. viewTop
    mrty     = n1 ^? (isoString . ppString)
    ext      = maybe "" reqType2ext mrty
    mgeo     = readGeo' (n2 ^. isoString)

------------------------------------------------------------------------------

evalLs :: CCmdEffects r => Path -> Sem r [Path]
evalLs p = do
  subcols <$> theEntry p
  where
    subcols :: ImgNodeP -> [Path]
    subcols n
      | isCOL  n  =        n ^.. theColEntries . traverse . theColEntry . theColColRef
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

globExpand' :: CCmdEffects r => [Text] -> [Path] -> Sem r [Path]
globExpand' []         = return
globExpand' (gl : gls) = cont gls . filterGlob gl
  where
    cont []   = return
    cont gls' = evalLss >=> globExpand' gls'

filterGlob :: Text -> [Path] -> [Path]
filterGlob globPattern = filter (matchGlob globPattern)

matchGlob :: Text -> (Path -> Bool)
matchGlob globPattern =
  case parseMaybe parseGlobText globPattern of
    Nothing  -> const False
    Just prs -> (\ p -> isJust . parseMaybe prs $ lastPath p ^. isoText)

globExpandPP :: CCmdEffects r => PathPos -> Sem r PathPos
globExpandPP (p, cx) = do
  ps <- globExpand p
  case ps of
    [p'] -> do
      log'trc $ untext [ "globExpand:", show (p', cx) ^. isoText]
      return (p', cx)
    []   ->
      abortWith $ untext [ "globExpand:"
                         , "no path found for "
                         , p ^. isoText
                         ]
    _    ->
      abortWith $ untext [ "globExpand:"
                         , "path not unique for pattern "
                         , p ^. isoText
                         ]

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
  let pth = (isoPathPos # pp) ^. isoText
  log'trc $ untext [ "evalSetMetaData:"
                   , pth
                   , key ^. isoText
                   , val
                   ]
  _id <- newUndoEntry $ "set matadata in " <> pth
  let md1 = mempty & metaTextAt key .~ val
  _r <- setMetaData1 (fromMaybe (-1) cx) (md1 ^. isoMetaDataMDT) p
  return ()

------------------------------------------------------------------------------

evalSetColImg :: CCmdEffects r => PathPos -> Path -> Sem r ()
evalSetColImg =
  evalSetCol' "evalSetColImg:" "set collection image " setCollectionImg

evalSetColBlog :: CCmdEffects r => PathPos -> Path -> Sem r ()
evalSetColBlog =
  evalSetCol' "evalSetColBlog:" "set collection blog " setCollectionBlog

evalSetCol' :: CCmdEffects r
            => Text
            -> Text
            -> (Path -> Int -> Path -> Sem r ())
            -> PathPos -> Path
            -> Sem r ()
evalSetCol' fnm msg setCol pp@(sp, cx) cp = do
  let pth = (isoPathPos # pp) ^. isoText
  cps <- globExpand cp
  log'trc $ untext [ fnm
                   , pth
                   , T.intercalate ", " $ map (^. isoText) cps
                   ]
  _id <- newUndoEntry $ msg <> pth
  traverse_ (setCol sp (fromMaybe (-1) cx)) cps

------------------------------------------------------------------------------

evalExifUpdate :: forall r. CCmdEffects r
               => Bool -> Bool -> Path -> Sem r ()
evalExifUpdate recursive force p0 = do
  _id <- newUndoEntry $ "exif update " <> (p0 ^. isoText)

  exifUpdate p0
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
        \ (i, ce) -> colEntryM (dli p i) evalDownload' ce

        where
          d' = d <> p ^. isoText

          -- dli :: Path -> Int -> Path -> Name -> Sem r ()
          dli dpath pos _ipath _ipart = do
            px <- genSqn

            let pn = picNoToText pos
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
  | isEmpty part = theEntry p >>= evalCheckSum' k p
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
          --  ^  traversal with ascending path names

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

  r   <- checkImgPart onlyUpdate part p

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

setGeoAddress :: forall r. CCmdEffects r
              => Bool
              -> Path
              -> ImgNodeP -> Sem r ()
setGeoAddress force p e
  | isIMG e = do
      setImgAddress

  | isCOL e = do
      setImgAddress                             -- process COL metadata
      traverse_
        (setGeoAddress' . colEntryM const id)   -- process COL entries
        (e ^. theColEntries)                    -- recurse into sub COLs
                                                -- or process IMG entries
  | isDIR e = do
      traverse_ setGeoAddress' (e ^. theDirEntries)

  | otherwise = return ()
  where
    setGeoAddress' :: CCmdEffects r => Path -> Sem r ()
    setGeoAddress' p' = theEntry p' >>= setGeoAddress force p'

    setImgAddress :: CCmdEffects r => Sem r ()
    setImgAddress
      | Just loc <- mgps
      , force
        ||
        isEmpty mAddr = do
          log'trc $ untext [ "setGeoAddress: update addr for"
                           , p ^. isoText
                           ]
          mga <- lookupGeoCache loc
          case mga of
            Nothing ->  -- no result got from location service
              return ()

            Just [] ->  -- no address for this loc
              return ()

                        -- take the display_name of the 1. available address
                        -- and add this to metadata of path p
            Just (GA{_display_name = ad} : _) -> do
              let md1 = mempty & metaTextAt descrAddress .~ ad
              _r <- setMetaData1 (-1) (md1 ^. isoMetaDataMDT) p
              return ()


      | otherwise = return ()
      where
        md :: MetaData
        md = e ^. theMetaData

        mgps :: Maybe GPSposDec
        mgps = lookupByKeys [descrGPSPosition, compositeGPSPosition] md ^. metaGPS

        mAddr :: Text
        mAddr = md ^. metaTextAt descrAddress

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
        isEmpty geo  = do
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
