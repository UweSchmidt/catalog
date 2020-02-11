{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main
where

import           Catalog.CmdAPI            ( Cmd'(..)
                                           , ReqType(..)
                                           )
import           Data.Prim

import           Data.Aeson                ( decode
                                           , encode
                                           )
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
import           System.IO

-- ----------------------------------------

data CEnv = CEnv
  { _trc         :: Bool
  , _verbose     :: Bool
  , _host        :: ByteString
  , _port        :: Int
  , _path        :: Path
  , _geo         :: Geo
  , _logOp       :: String -> IO ()
  , _manager     :: Manager
  }

instance Config CEnv where
  traceOn   e = e ^. envTrc
  verboseOn e = e ^. envVerbose
  getLogOp  e = e ^. envLogOp

mkCEnv :: Bool
       -> Bool
       -> ByteString
       -> Int
       -> Path
       -> Geo
       -> (String -> IO ())
       -> Manager
       -> CEnv
mkCEnv = CEnv


defaultCEnv :: CEnv
defaultCEnv = CEnv
  { _trc          = False
  , _verbose      = False
  , _host         = "localhost"
  , _port         = 3001
  , _path         = ""
  , _geo          = Geo 1 1
  , _logOp        = defaultLogger
  , _manager      = defaultManager
  }

initCEnv :: CEnv -> IO CEnv
initCEnv env = do
  manager <- newManager defaultManagerSettings
  return (env & envManager .~ manager)

defaultLogger :: String -> IO ()
defaultLogger = hPutStr stderr

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

envPath :: Lens' CEnv Path
envPath k e = (\ new -> e {_path = new}) <$> k (_path e)

envGeo :: Lens' CEnv Geo
envGeo k e = (\ new -> e {_geo = new}) <$> k (_geo e)

envLogOp :: Lens' CEnv (String -> IO ())
envLogOp k e = (\ new -> e {_logOp = new}) <$> k (_logOp e)

envManager :: Lens' CEnv Manager
envManager k e = (\ new -> e {_manager = new}) <$> k (_manager e)

-- --------------------

type CState = ()

emptyCState :: CState
emptyCState = ()

-- --------------------

type CCmd   = Action CEnv CState

runCCmd :: CEnv -> CCmd a -> IO (Either Msg a)
runCCmd env cmd = fst <$> runAction cmd env emptyCState

-- ----------------------------------------

main :: IO ()
main = do
  env <- initCEnv defaultCEnv
  res <- runCCmd env (return ())
  either (error . show) return res

main1 :: Show a => CCmd a -> IO ()
main1 cmd = do
  env <- initCEnv defaultCEnv
  res <- runCCmd env (cmd)
  print res -- either (error . show) return res
  return ()

t1 :: IO ()
t1 = main1 (reqCmd $ TheCollection $ readPath "/archive/photos")

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

-- ----------------------------------------
