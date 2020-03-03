{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main
where

import           Catalog.CmdAPI
import           Catalog.System.IO
import           Catalog.FilePath          ( isoPicNo )
import           Catalog.Workflow          ( PathPos
                                           , imgReqTypes
                                           , isoPathPos
                                           )
import           Data.Prim          hiding ( argument )
import           Data.ImgNode
import           Data.MetaData             ( prettyMD
                                           , selectByParser
                                           )
import           Data.Aeson                ( decode
                                           , encode
                                           )
import           Text.SimpleParser         ( SP )
import qualified Text.SimpleParser   as SP

import           Control.Monad.ReaderStateErrIO

import           Network.HTTP.Client       ( Request(..)
                                           , RequestBody(..)
                                           , Response(..)
                                           , Manager
                                           , defaultManagerSettings
                                           , defaultRequest
                                           , httpLbs
                                           , newManager
                                           )
import           Network.HTTP.Types.Header ( hContentType )
import           Network.HTTP.Types.Method ( Method )
import           Network.HTTP.Types.Status ( statusCode
                                           , statusMessage
                                           )
import           Options.Applicative
import           System.IO
import           System.Exit

-- --------------------
--
-- version of the software
-- version number is updated automatically

version :: String
version = "0.2.4.4"

date :: String
date = "2020-03-03"

appname :: String
appname = "client"

-- ----------------------------------------

main :: IO ()
main = mainWithArgs appname $ \ env -> do
  env' <- initCEnv env
  res  <- runCCmd env' catalogClient
  exitWith $
    either (const $ ExitFailure 1) (const ExitSuccess) $ res

-- ----------------------------------------

data CC
  = CC'ls       Path
  | CC'md       PathPos KeyParser
  | CC'entry    Path
  | CC'download Path ReqType Geo FilePath
  | CC'noop

type KeyParser = SP String

img'variants :: String
img'variants =
  intercalate ", " . map (^. isoString) $ imgReqTypes

-- ----------------------------------------

catalogClient :: CCmd ()
catalogClient = do
  c <- view envCC
  -- p <- view (envPath . isoString . from isoString)
  case c of
    CC'entry p ->
      evalCEntry p >>= io . print

    CC'ls p ->
      evalCLs p >>= io . sequenceA_ . map print

    CC'md pp keys ->
      evalCMetaData pp keys >>= io . sequenceA_ . map putStrLn . prettyMD

    CC'download p rt geo d0 -> do
      -- dir hierachy equals cache hierachy in catalog server
      let geo'
            | geo == geo'org = orgGeo
            | otherwise      = geo ^. isoString
      let px = "docs" </> (rt ^. isoString) </> geo'

      let d  = isoFilePath # d0
      let d1 = isoFilePath # (d0 </> px)

      dx  <- dirExist d
      if dx
        then evalCDownload rt geo d1 p
        else abort $ "Download dir not found: " <> d0

    CC'noop ->
      trc $ "nothing to do"

evalCEntry :: Path -> CCmd ImgNodeP
evalCEntry p = do
      trc $ unwords ["evalCEntry:", p ^. isoString]
      r <- reqCmd $ TheCollection p
      trc $ unwords ["res =", show r]
      return r

evalCLs :: Path -> CCmd [Path]
evalCLs p = do
      trc $ unwords ["evalCLs:", p ^. isoString]
      subcols <$> reqCmd (TheCollection p)
  where
    subcols :: ImgNodeP -> [Path]
    subcols n
      | isCOL  n  = n ^.. theColEntries . traverse . theColColRef
      | isROOT n  = n ^.. theRootImgCol
      | otherwise = []

evalCMetaData :: PathPos -> KeyParser -> CCmd MetaData
evalCMetaData pp@(p, cx) keyP = do
      trc $ unwords ["evalCMetaData:", from isoString . isoPathPos # pp]
      r <- (^. selectByParser keyP)
           <$>
           (reqCmd $ TheMetaData (fromMaybe (-1) cx) p)
      trc $ unwords ["res =", show r]
      return r

evalCDownload :: ReqType -> Geo -> SysPath -> Path -> CCmd ()
evalCDownload rt geo d p = do
  -- pre: directory d already exists
  trc $ unwords [ "evalCDownload:"
                , p ^. isoString
                ]

  -- name clash with already existing directory
  whenM (dirExist d') $
    void $
    abort $ unwords
      [ "evalCDownload:"
      , "dir already exists"
      , d' ^. isoFilePath
      , ", cleanup download dir"
      , d ^. isoFilePath
      ]

  -- get collection from server
  n <- evalCEntry p
  unless (isCOL n) $
    abort $ unwords
      [ "evalCDownload:"
      , "no collection found for path"
      , p ^. isoString
      ]

  -- create download subdir
  createDir d'

  -- download all collection entries
  forM_ (zip [(0::Int) ..] $ n ^. theColEntries . isoSeqList) $
    \ (i, ce) -> colEntry (dli p i) (dlc) ce

  where
    d' = d & isoFilePath %~ (<> p ^. isoString)

    -- download an image
    dli :: Path -> Int -> Path -> Name -> CCmd ()
    dli dpath pos _ipath _ipart = do

      trc $ unwords
        [ "evalCDownload:"
        , "download JPG image copy to"
        , sp ^. isoFilePath
        ]

      lbs <- reqCmd (JpgImgCopy rt geo pp)
      writeFileLB sp lbs
      where
        pn :: String
        pn = pos ^. isoPicNo
        sn = "/" <> pn <> ".jpg"
        pp = dpath `snocPath` mkName pn
        sp = d' & isoFilePath %~ (<> sn)

    -- download a subcollection: recursive call
    dlc :: Path -> CCmd ()
    dlc cpath = evalCDownload rt geo d cpath

-- ----------------------------------------

reqCmd :: Cmd' a -> CCmd a

-- catalog modifications

reqCmd (SaveBlogSource pos t p) =
  paramJSONmodify "saveblogsource" p (pos, t)

reqCmd (ChangeWriteProtected ixs ro p) =
  paramJSONmodify "changeWriteProtected" p (ixs, ro)

reqCmd (SortCollection ixs p) =
  paramJSONmodify "sort" p ixs

reqCmd (RemoveFromCollection ixs p) =
  paramJSONmodify "removeFromCollection" p ixs

reqCmd (CopyToCollection ixs dst p) =
  paramJSONmodify "copyToCollection" p (ixs, dst)

reqCmd (MoveToCollection ixs dst p) =
  paramJSONmodify "moveToCollection" p (ixs, dst)

reqCmd (SetCollectionImg sPath pos p) =
  paramJSONmodify "colimg" p (sPath, pos)

reqCmd (SetCollectionBlog sPath pos p) =
  paramJSONmodify "colblog" p (sPath, pos)

reqCmd (NewCollection nm p) =
  paramJSONmodify "newcol" p nm

reqCmd (RenameCollection nm p) =
  paramJSONmodify "renamecol" p nm

reqCmd (SetMetaData ixs md p) =
  paramJSONmodify "setMetaData" p (ixs, md)

reqCmd (SetMetaData1 pos md p) =
  paramJSONmodify "setMetaData1" p (pos, md)

reqCmd (SetRating ixs r p) =
  paramJSONmodify "setRating" p (ixs, r)

reqCmd (SetRating1 pos r p) =
  paramJSONmodify "setRating1" p (pos, r)

reqCmd (Snapshot t p) =
  paramJSONmodify "snapshot" p t

reqCmd (SyncCollection p) =
  simpleJSONmodify "syncCol" p

reqCmd (SyncExif p) =
  simpleJSONmodify "syncExif" p

reqCmd (NewSubCollections p) =
  simpleJSONmodify "newSubCols" p

-- catalog queries

reqCmd (TheCollection p) =
  simpleJSONget "collection" p

reqCmd (IsWriteable p) =
  simpleJSONget "isWritable" p

reqCmd (IsRemovable p) =
  simpleJSONget "isRemovable" p

reqCmd (IsSortable p) =
  simpleJSONget "isSortable" p

reqCmd (IsCollection p) =
  simpleJSONget "isCollection" p

reqCmd (TheBlogContents pos p) =
  paramJSONget "blogcontents" p pos

reqCmd (TheBlogSource pos p) =
  paramJSONget "blogsource" p pos

reqCmd (TheMetaData pos p) =
  paramJSONget "metadata" p pos

reqCmd (TheRating pos p) =
  paramJSONget "rating" p pos

reqCmd (TheRatings p) =
  simpleJSONget "ratings" p

-- resized images and html pages

reqCmd (JpgImgCopy rt geo p) =
  basicDocReq ".jpg" rt geo p

reqCmd (HtmlPage rt geo p) =
  basicDocReq ".html" rt geo p

-- on client side the dir path prefix for
-- accessing static files is ignored
-- this arg is added by the servant server

reqCmd (StaticFile _dp bn) =
  basicReq "GET" id (mkPath bn')
  where
    bn' = isoText # bn

-- --------------------

p'edit'html
  , p'index'html
  , p'favicon'ico
  , p'rpc'js      :: Cmd' LazyByteString

p'index'html  = StaticFile ps'html       "index.html"
p'edit'html   = StaticFile ps'html       "edit.html"
p'favicon'ico = StaticFile ps'icons      "favicon.ico"
p'rpc'js      = StaticFile ps'javascript "rpc.js"

-- --------------------

simpleJSONget :: FromJSON a => Name -> Path -> CCmd a
simpleJSONget = simpleJSONReq "get"

simpleJSONmodify :: FromJSON a => Name -> Path -> CCmd a
simpleJSONmodify = simpleJSONReq "modify"

paramJSONget ::  (FromJSON r, ToJSON a) => Name -> Path -> a -> CCmd r
paramJSONget = paramJSONReq "get"

paramJSONmodify ::  (FromJSON r, ToJSON a) => Name -> Path -> a -> CCmd r
paramJSONmodify = paramJSONReq "modify"

-- --------------------

simpleJSONReq :: FromJSON r => Name -> Name -> Path -> CCmd r
simpleJSONReq mn cn p = basicJSONReq (buildPath mn cn p)

paramJSONReq ::  (FromJSON r, ToJSON a) => Name -> Name -> Path -> a -> CCmd r
paramJSONReq mn cn p = argJSONReq (buildPath mn cn p)

argJSONReq :: (ToJSON a, FromJSON r) => Path -> a -> CCmd r
argJSONReq path' arg' =
  basicReq "POST" (setReqBodyJSON $ encode arg') path' >>= jsonDecode

basicJSONReq :: FromJSON r => Path -> CCmd r
basicJSONReq path' =
  basicReq "POST" id path' >>= jsonDecode

basicDocReq :: String -> ReqType -> Geo -> Path -> CCmd LazyByteString
basicDocReq ext rt geo path0 =
  basicReq "GET" id path'
  where
    path'  = "docs" `consPath` img' `consPath` geo' `consPath` path1
    img'   = rt   ^. isoString . from isoString
    geo'   = geo  ^. isoString . from isoString
    path1  = path0 & isoString %~ (<> ext)

basicReq ::  Method
         -> (Request -> Request)
         -> Path
         -> CCmd LazyByteString
basicReq method' setBody path' = do
  host' <- view $ envHost . isoString
  port' <- view $ envPort

  let request = setBody $ defaultRequest
        { method = method'
        , host   = isoString # host'
        , port   = port'
        , path   = path' ^. isoString . from isoString
        }
  verbose $ show'basicReq request

  manager  <- view envManager
  response <- liftIO $ httpLbs request manager

  let rbody  = responseBody   response
  let status = responseStatus response
  let scode  = statusCode status

  unless (scode == 200) $
    if scode == 500
    then
      abort $ rbody ^. isoString
    else
      abort $ unwords [show scode, statusMessage status ^. isoString]

  return
    (responseBody response)

show'basicReq :: Request -> String
show'basicReq r =
  unwords [ method r ^. isoString
          , "http://"
            <> host r ^. isoString
            <> ":"
            <> show (port r)
            <> path r ^. isoString
          ]

setReqBodyJSON :: LazyByteString -> Request -> Request
setReqBodyJSON body' req =
  req { requestBody =
          RequestBodyLBS body'
      , requestHeaders =
          (hContentType, "application/json; charset=utf-8") :
          requestHeaders req
      }

buildPath :: Name -> Name -> Path -> Path
buildPath mn cn p = mn `consPath` cn `consPath` p

jsonDecode :: FromJSON r => LazyByteString -> CCmd r
jsonDecode lbs =
  maybe
    (abort "JSON decode error")
    return
    (decode lbs)

-- --------------------
--
-- the client monad, a reader-state-error-io monad

type CCmd   = Action CEnv CState

runCCmd :: CEnv -> CCmd a -> IO (Either Msg a)
runCCmd env cmd = fst <$> runAction cmd env emptyCState

-- --------------------
--
-- the app state, currently empty

type CState = ()

emptyCState :: CState
emptyCState = ()

-- ----------------------------------------
--
-- environment and command line IF

data CEnv = CEnv
  { _trc         :: Bool
  , _verbose     :: Bool
  , _host        :: ByteString
  , _port        :: Int
  , _cc          :: CC
  , _logOp       :: String -> IO ()
  , _manager     :: Manager
  }

instance Config CEnv where
  traceOn   e = e ^. envTrc
  verboseOn e = e ^. envVerbose
  getLogOp  e = e ^. envLogOp

defaultCEnv :: CEnv
defaultCEnv = CEnv
  { _trc          = False
  , _verbose      = False
  , _host         = "localhost"
  , _port         = 3001
  , _cc           = CC'noop
  , _logOp        = defaultLogger
  , _manager      = defaultManager
  }

defaultPath :: Path
defaultPath = "/archive"

----------------------
--
-- the constructor used in commandline option parsing

mkCEnv :: (Bool, Bool)
       -> (ByteString, Int)
       -> CC
       -> CEnv
mkCEnv (trc', verbose')
       (host', port')
       cc' =
  CEnv trc' verbose'
       host' port'
       cc'
       defaultLogger
       defaultManager

-- --------------------

initCEnv :: CEnv -> IO CEnv
initCEnv env = do
  manager <- newManager defaultManagerSettings
  return (env & envManager .~ manager)

defaultLogger :: String -> IO ()
defaultLogger = hPutStrLn stderr

defaultManager :: Manager
defaultManager = undefined

envTrc :: Lens' CEnv Bool
envTrc k e = (\ new -> e {_trc = new}) <$> k (_trc e)

envVerbose :: Lens' CEnv Bool
envVerbose k e = (\ new -> e {_verbose = new}) <$> k (_verbose e)

envHost :: Lens' CEnv ByteString
envHost k e = (\ new -> e {_host = new}) <$> k (_host e)

envPort :: Lens' CEnv Int
envPort k e = (\ new -> e {_port = new}) <$> k (_port e)

envCC :: Lens' CEnv CC
envCC k e = (\ new -> e {_cc = new}) <$> k (_cc e)

envLogOp :: Lens' CEnv (String -> IO ())
envLogOp k e = (\ new -> e {_logOp = new}) <$> k (_logOp e)

envManager :: Lens' CEnv Manager
envManager k e = (\ new -> e {_manager = new}) <$> k (_manager e)

-- ----------------------------------------
--
-- cmd line option parsing
--

mainWithArgs :: String -> (CEnv -> IO ()) -> IO ()
mainWithArgs theAppName theAppMain =
  execParser (appInfo theAppName)
  >>= theAppMain

appInfo :: String -> ParserInfo CEnv
appInfo pname =
  info (helper <*> envp)
  ( fullDesc
    <> progDesc "query, modify and download collections from catalog"
    <> header ("catalog-" ++ pname ++ " - " ++ version ++ " (" ++ date ++ ")")
  )

envp :: Parser CEnv
envp = mkCEnv
  <$> verboseP
  <*> hostP
  <*> cmdP

-- --------------------

verboseP :: Parser (Bool, Bool)
verboseP = (,)
  <$> ( flag (defaultCEnv ^. envTrc) True
        ( long "trc"
          <> short 't'
          <> help "Turn on trace output"
        )
      )
  <*> ( flag (defaultCEnv ^. envVerbose) True
        ( long "verbose"
          <> short 'v'
          <> help "Turn on verbose output"
        )
      )

hostP :: Parser (ByteString, Int)
hostP = (,)
  <$> strOption
      ( long "host"
        <> short 'H'
        <> metavar "HOST"
        <> showDefault
        <> value "localhost"
        <> help "The host for the catalog server"
      )
  <*> option auto
      ( long "port"
        <> short 'P'
        <> help "The port listened at by the catalog server"
        <> showDefault
        <> value 3001
        <> metavar "PORT"
      )

cmdP :: Parser CC
cmdP = subparser $
  command "ls"
    ( (CC'ls    <$> pathP)
      `withInfo`
      ( "List subcollections, default PATH is: "
        ++ show defaultPath
      )
    )
  <>
  command "md"
    ( mdP
      `withInfo`
      ( "Show metadata for a collection or entry of a collection, default PATH is: "
        ++ show defaultPath
      )
    )
  <>
  command "download"
    ( downLoadP
      `withInfo`
      "Download all images of a collection"
    )
  <>
  command "entry"
    ( (CC'entry <$> pathP)
      `withInfo`
      ( "Dump catalog entry, for testing and debugging, default Path is: "
        ++ show defaultPath
      )
    )
  <>
  command "noop"
    ( pure CC'noop
      `withInfo`
      "Do nothing"
    )

mdP :: Parser CC
mdP = flip CC'md
  <$> option wildcardParser
      ( long "keys"
        <> short 'k'
        <> metavar "KEY-SELECT"
        <> value SP.manyChars
        <> help "Select metadata keys by searching of a substring"
      )
  <*> pathPP

downLoadP :: Parser CC
downLoadP = dl
  <$> downLoadOptP
  <*> pathP1
  where
    dl (reqtype', geo', dest') path'
      = CC'download path' reqtype' geo' dest'

downLoadOptP :: Parser (ReqType, Geo, FilePath)
downLoadOptP = (,,)
  <$> option imgReqReader
      ( long "variant"
        <> short 'i'
        <> help ( "The image variant, one of ["
                  ++ img'variants
                  ++ "], default: img"
                )
        <> value RImg
        <> metavar "IMG-VARIANT"
      )
  <*> option geoReader
      ( long "geometry"
        <> short 'g'
        <> help "The image geometry: <width>x<height> or org (original size)"
        <> value (Geo 1 1)
        <> metavar "GEOMETRY"
      )
  <*> strOption
      ( long "dest"
          <> short 'd'
          <> metavar "DOWNLOAD-DIR"
          <> showDefault
          <> value "."
          <> help "The dir to store downloads"
      )

pathP :: Parser Path
pathP = pathP1 <|> pure defaultPath

pathP1 :: Parser Path
pathP1 = argument str (metavar "PATH")

pathPP :: Parser PathPos
pathPP = (^. isoPathPos) <$> pathP

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

-- ----------------------------------------
--
-- app specific option parsers

imgReqReader :: ReadM ReqType
imgReqReader = eitherReader parse
  where
    parse arg =
      maybe
        (Left $ "Wrong image format: " ++ arg)
        Right
        (arg ^? prismString)

geoReader :: ReadM Geo
geoReader = eitherReader parse
  where
    parse arg =
      maybe
        (Left $ "Wrong geometry: " ++ arg)
        Right
        (readGeo' arg)

wildcardParser :: ReadM KeyParser
wildcardParser = eitherReader parse
  where
    parse arg = Right $ wildcard2parser arg

    wildcard2parser :: String -> KeyParser
    wildcard2parser [] = SP.manyChars
    wildcard2parser xs = SP.anyStringThen (SP.string xs) <* SP.manyChars


-- ----------------------------------------
