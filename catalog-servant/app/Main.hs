{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude ()
import Prelude.Compat

import Control.Concurrent.MVar ( MVar
                               , newMVar, readMVar, takeMVar, swapMVar, putMVar
                               , withMVar)
import Control.Exception       ( SomeException
                               , catch) -- , try, toException)
import Control.Monad.ReaderStateErrIO
                               ( Msg(..) )
import Network.Wai.Handler.Warp( setPort
                               , setLogger
                               , defaultSettings
                               , runSettings
                               )
import Network.Wai.Logger      ( withStdoutLogger )

import Servant

import System.Directory        ( doesFileExist )
import System.Exit             ( die )
import System.IO               ( hPutStrLn, stderr, hFlush )

import qualified Data.ByteString.Lazy as LBS

-- catalog modules
import Data.Prim
import Data.ImageStore         ( ImgStore )

import Catalog.Cmd
import Catalog.EvalCmd
import Catalog.FilePath        ( splitDirFileExt
                               , isoPicNo
                               )

import Catalog.Options         ( mainWithArgs )
import Catalog.Workflow        ( ReqType(..)
                               , PathPos
                               , emptyReq'
                               , reqType2AR
                               , processReqImg
                               , processReqPage
                               , thePageCnfs
                               , rType
                               , rGeo
                               , rPathPos
                               )

-- servant interface
import API

-- import Debug.Trace

-- ----------------------------------------
--
-- eval commands

mkcmd1' :: (Cmd r -> Handler r)
        -> (Path -> Cmd' r)
        -> [Text]
        -> Handler r
mkcmd1' toHandler cmd =
  toHandler . evalCmd . cmd . listToPath


mkcmd2' :: (Cmd r -> Handler r)
        -> (a -> Path -> Cmd' r)
        -> [Text]
        -> a
        -> Handler r
mkcmd2' toHandler cmd path args =
  toHandler . evalCmd . cmd args . listToPath $ path


-- ----------------------------------------
-- the server

catalogServer :: Env ->
                 (forall a . Cmd a -> Handler a) ->
                 (forall a . Cmd a -> Handler a) ->
                 Server CatalogAPI
catalogServer env runR runM =
  ( bootstrap
    :<|>
    ( assets'css
      :<|>
      assets'icons
      :<|>
      assets'javascript
    )
    :<|>
    ( root'html
      :<|>
      favicon'ico
      :<|>
      rpc'js
    )
  )
  :<|>
  ( json'read
    :<|>
    json'modify
  )
  :<|>
  ( get'icon
    :<|>
    get'iconp
    :<|>
    get'img
    :<|>
    ( get'html
      :<|>
      get'html1
    )
    :<|>
    get'movie
  )

  where
    mountPath = env ^. envMountPath . isoFilePath
    static p  = do
      -- trace ("static: " ++ mountPath ++ p) $
        serveDirectoryWebApp (mountPath ++ p)

    bootstrap         = static ps'bootstrap
    assets'css        = static ps'css
    assets'icons      = static ps'icons
    assets'javascript = static ps'javascript

    -- movies are served statically to enable streaming
    -- the original .mp4 movies are accessed
    -- by a path prefix "/docs/movies/archive/<syncdir>/" ++ <path-to-mp4>
    -- image object
    -- <syncdir> default is "photos"

    get'movie         = static $ "/" ++ env ^. envSyncDir

    -- root html files are located under /assets/html

    root'html :: BaseName HTMLStatic -> Handler LazyByteString
    root'html bn = staticFile ps'html bn

    favicon'ico :: Handler LazyByteString
    favicon'ico = staticFile ps'icons bn
      where
        bn :: BaseName ICO
        bn = BaseName "favicon.ico"

    rpc'js :: Handler LazyByteString
    rpc'js = staticFile ps'javascript bn
      where
        bn :: BaseName JSStatic
        bn = BaseName "rpc-servant.js"

    staticFile :: FilePath -> BaseName a -> Handler LazyByteString
    staticFile dirPath (BaseName n) = do
      ex <- liftIO $ doesFileExist fp
      case ex of
        False ->
          throwError err404
        True ->
          liftIO (LBS.readFile fp)
      where
        fp = mountPath ++ dirPath ++ "/" ++ n ^. isoString

    -- --------------------
    -- new URL handlers

    -- parsed URL -> org URL, for error messages

    backToPath :: ReqType -> Geo -> [Text] -> String
    backToPath req geo path =
      mconcat . map ('/' :) $
      ( reqType2AR req ^. isoString
        : geo  ^. isoString
        : map (^. isoString) path
      )

    -- parser for object path
    --
    -- remove extension
    -- parse optional collection index
    --
    -- example: path2colPath ".jpg" ["collections","2018", "may", "pic-0007.jpg"]
    --          -> Just ("/collections/2018/may", Just 7)

    path2colPath :: String -> [Text] -> Maybe PathPos
    path2colPath ext ts
      | Just (dp, fn, ex) <- splitDirFileExt ps
      , ex == ext =
          Just $ buildPP dp fn
      | otherwise =
          Nothing
      where
        ps = concatMap (('/' :) . (^. isoString)) ts

        buildPP dp' fn'
          | cx < 0    = (readPath $ dp' </> fn', Nothing)
          | otherwise = (readPath   dp',         Just cx)
          where
            cx = fn' ^. from isoPicNo

    -- --------------------

    cachedResponse :: Maybe Text -> LazyByteString -> CachedByteString
    cachedResponse mbref bs =
      addHeader (cval ^. isoText) bs
      where
        cval
          | isEditRef = "no-store"
          | otherwise = "public, max-age=" ++ show aDay

        ref = fromMaybe mempty mbref ^. isoString

        isEditRef = "/edit.html" `isSuffixOf` ref

        aDay :: Int
        aDay = 24 * 60 * 60

    -- --------------------
    -- handle icon request

    get'icon  :: Geo' -> [Text] -> Maybe Text -> Handler CachedByteString
    get'icon  = get'img' RIcon

    get'iconp :: Geo' -> [Text] -> Maybe Text -> Handler CachedByteString
    get'iconp = get'img' RIconp

    get'img  :: Geo' -> [Text] -> Maybe Text  -> Handler CachedByteString
    get'img  = get'img' RImg

    get'img' :: ReqType
             -> Geo' -> [Text] -> Maybe Text  -> Handler CachedByteString
    get'img' rt (Geo' geo) ts@(_ : _) referer

      -- check for collection path with .jpg extension
      | Just ppos <- path2colPath ".jpg" ts =
          do
            res <- runR $ processReqImg (mkReq rt geo ppos)
                          >>= toSysPath
                          >>= readFileLB
            return $ cachedResponse referer res

    get'img' rt (Geo' geo) ts _ref =
      notThere rt geo ts

    -- --------------------
    -- handle html pages

    get'html :: Geo' -> [Text] -> Handler LazyByteString
    get'html = get'html' RPage

    get'html1 :: Geo' -> [Text] -> Handler LazyByteString
    get'html1 = get'html' RPage1

    get'html' :: ReqType -> Geo' -> [Text] -> Handler LazyByteString
    get'html' rt (Geo' geo) ts@(_ : _)
      | Just ppos <- path2colPath ".html" ts
      , exPageConf geo =
          runR $ processReqPage (mkReq rt geo ppos)

    get'html' rt (Geo' geo) ts =
      notThere rt geo ts

    -- --------------------
    -- aux ops

    exPageConf geo = isJust $ lookup geo thePageCnfs

    mkReq rt geo ppos' =
      emptyReq' & rType    .~ rt
                & rGeo     .~ geo
                & rPathPos .~ ppos'

    notThere :: ReqType -> Geo -> [Text] -> Handler a
    notThere rt geo ts =
      throwError $
      err404 { errBody =
                 ( "document not found: " ++ backToPath rt geo ts )
                 ^. from isoString
             }


    -- --------------------

    mkR1' = mkcmd1' runR
    mkR2' = mkcmd2' runR

    json'read =
      mkR1' TheCollection
      :<|>
      mkR1' IsWriteable
      :<|>
      mkR1' IsRemovable
      :<|>
      mkR1' IsSortable
      :<|>
      mkR1' IsCollection
      :<|>
      mkR2' TheBlogContents
      :<|>
      mkR2' TheBlogSource
      :<|>
      mkR2' TheMetaData
      :<|>
      mkR2' TheRating
      :<|>
      mkR1' TheRatings


    mkM1' = mkcmd1' runM
    mkM2' = mkcmd2' runM
    mkM3' = mkM2' . uncurry

    json'modify =
      mkM3' SaveBlogSource
      :<|>
      mkM3' ChangeWriteProtected
      :<|>
      mkM2' SortCollection
      :<|>
      mkM2' RemoveFromCollection
      :<|>
      mkM3' CopyToCollection
      :<|>
      mkM3' MoveToCollection
      :<|>
      mkM3' SetCollectionImg
      :<|>
      mkM3' SetCollectionBlog
      :<|>
      mkM2' NewCollection
      :<|>
      mkM2' RenameCollection
      :<|>
      mkM3' SetMetaData
      :<|>
      mkM3' SetMetaData1
      :<|>
      mkM3' SetRating
      :<|>
      mkM3' SetRating1
      :<|>
      mkM2' Snapshot
      :<|>
      mkM1' SyncCollection
      :<|>
      mkM1' SyncExif
      :<|>
      mkM1' NewSubCollections

-- ----------------------------------------
--
-- process command line args,
-- build env, configure log command
-- and init server state

main :: IO ()
main = mainWithArgs "servant" $ \ env -> do
  -- create a semaphore for syncing log output
  sem  <- newMVar ()
  let env' = env & envLogOp .~ logCmd sem

  est  <- initState env'
  either die (main' env') est
  where
    -- a log command that syncronizes
    -- output of messages to stderr
    logCmd :: MVar () -> (String -> IO ())
    logCmd sem s =
      withMVar sem $ \ _ -> do hPutStrLn stderr s
                               hFlush    stderr

main' :: Env -> ImgStore -> IO ()
main' env st = do
  -- create MVars for the image archive state
  mvRead <- newMVar st
  mvMody <- newMVar st

  let runRead  = runReadCmd env mvRead
  let runMody  = runModyCmd env mvRead mvMody

  withStdoutLogger $ \logger -> do
    let settings = setPort (env ^. envPort) $ setLogger logger defaultSettings
    runSettings settings $
      serve (Proxy :: Proxy CatalogAPI) $
      catalogServer env runRead runMody

-- curl -v http://localhost:8081/bootstrap/dist/css/bootstrap-theme.css
-- curl -v http://localhost:8081/assets/javascript/html-album.js
-- curl -v http://localhost:8081/edit.html

-- ----------------------------------------

-- there are 2 mvars for the image store
--
-- one for reading operations, those can run in parallel
-- so the mvar is not emptied when starting an action
--
-- the 2. is for modifying the store, those operations run sequentially
-- and at the end they update both mvars with the new state

runReadCmd :: Env -> MVar ImgStore -> Cmd a -> Handler a
runReadCmd env mvs cmd = do
  res <- liftIO runc
  either raise500 return res
  where
    runc = do
      store <- readMVar mvs
      res <- ( (^. _1) <$> runAction cmd env store )
             `catch`
             -- TODO this still does not catch: error "some error"
             (\ e -> return (Left . Msg . show $ (e :: SomeException)))
      return res

runModyCmd :: Env -> MVar ImgStore -> MVar ImgStore -> Cmd a -> Handler a
runModyCmd env mvr mvm cmd = do
  res <- liftIO runc
  either raise500 return res
  where
    runc = do
      store <- takeMVar mvm
      res <- ( do
                 (res', new'store) <- runAction cmd env store
                 _old <- swapMVar mvr new'store
                 putMVar mvm new'store
                 return res'
             )
             `catch`
             (\ e -> return (Left . Msg . show $ (e :: SomeException)))
      return res

raise500 :: Msg -> Handler a
raise500 (Msg msg) =
  throwError $ err500 { errBody = msg' }
  where
    msg' = show msg ^. from isoString

-- ----------------------------------------
