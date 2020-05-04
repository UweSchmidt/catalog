{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude ()
import Prelude.Compat

import Control.Concurrent.Async( async
                               , link
                               )
import Control.Concurrent.STM  ( atomically
                               , TMVar
                               , newTMVarIO
                               , putTMVar
                               , readTMVar
                               , swapTMVar
                               , takeTMVar
                               , TChan
                               , newTChanIO
                               , readTChan
                               , writeTChan
                               )
import Control.Exception       ( SomeException
                               , catch -- , try, toException
                               )
import Control.Monad.ReaderStateErrIO
                               ( Msg(..) )
import Network.Wai.Handler.Warp( setPort
                               , setLogger
                               , defaultSettings
                               , runSettings
                               )
import Network.Wai.Logger      ( withStdoutLogger )

import Servant

import System.Exit             ( die )
import System.IO               ( hPutStrLn, stderr, hFlush )

-- catalog modules
import Data.Prim
import Data.ImageStore         ( ImgStore )
import Catalog.Cmd             ( Env
                               , runAction
                               , liftIO
                               , envLogOp
                               , envMountPath
                               , envPort
                               , envSyncDir
                               , initState
                               )
import Catalog.EvalCmd         ( Cmd'(..)
                               , evalCmd
                               )
import Catalog.Options         ( mainWithArgs )

-- servant interface
import API

-- import Debug.Trace


-- ----------------------------------------
-- the server

catalogServer :: Env
              -> (forall a . Cmd' a  -> Handler a )
              -> (forall a . Cmd' a  -> Handler a )
              -> (           Cmd' () -> Handler ())
              -> Server CatalogAPI
catalogServer env runR runM runB =
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
    get'imgfx
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
    staticFile dirPath (BaseName n) =
      runR $ StaticFile dirPath n

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

    get'imgfx  :: Geo' -> [Text] -> Maybe Text  -> Handler CachedByteString
    get'imgfx  = get'img' RImgfx

    get'img' :: ReqType
             -> Geo' -> [Text] -> Maybe Text  -> Handler CachedByteString
    get'img' rt (Geo' geo) ts referer = do
      res <- runR . JpgImgCopy rt geo . listToPath $ ts
      return $ cachedResponse referer res

     -- --------------------
    -- handle html pages

    get'html :: Geo' -> [Text] -> Handler LazyByteString
    get'html = get'html' RPage

    get'html1 :: Geo' -> [Text] -> Handler LazyByteString
    get'html1 = get'html' RPage1

    get'html' :: ReqType -> Geo' -> [Text] -> Handler LazyByteString
    get'html' rt (Geo' geo) =
      runR . HtmlPage rt geo . listToPath

    -- --------------------

    runR1 :: forall r.
             (Path -> Cmd' r) -> [Text] -> Handler r
    runR1 cmd' = runR  . cmd' . listToPath

    runR2 :: forall a r.
             (a -> Path -> Cmd' r) -> [Text] -> a -> Handler r
    runR2 cmd' ts args = runR  . cmd' args . listToPath $ ts

    runR3 :: forall a b r.
             (a -> b -> Path -> Cmd' r) -> [Text] -> (a, b) -> Handler r
    runR3 = runR2 . uncurry

    json'read =
      runR1 TheCollection
      :<|>
      runR1 IsWriteable
      :<|>
      runR1 IsRemovable
      :<|>
      runR1 IsSortable
      :<|>
      runR1 IsCollection
      :<|>
      runR2 TheBlogContents
      :<|>
      runR2 TheBlogSource
      :<|>
      runR2 TheMetaData
      :<|>
      runR2 TheRating
      :<|>
      runR1 TheRatings
      :<|>
      runR3 CheckImgPart

    runM1 :: forall r.
             (Path -> Cmd' r) -> [Text] -> Handler r
    runM1 cmd' = runM . cmd' . listToPath

    runM2 :: forall a r.
             (a -> Path -> Cmd' r) -> [Text] -> a -> Handler r
    runM2 cmd' ts args = runM . cmd' args . listToPath $ ts

    runM3 :: forall a b r.
             (a -> b -> Path -> Cmd' r) -> [Text] -> (a, b) -> Handler r
    runM3 = runM2 . uncurry

    runB2 :: forall a.
             (a -> Path -> Cmd' ()) -> [Text] -> a -> Handler ()
    runB2 cmd' ts args = runB . cmd' args . listToPath $ ts

    json'modify =
      runM3 SaveBlogSource
      :<|>
      runM3 ChangeWriteProtected
      :<|>
      runM2 SortCollection
      :<|>
      runM2 RemoveFromCollection
      :<|>
      runM3 CopyToCollection
      :<|>
      runM3 MoveToCollection
      :<|>
      runM3 SetCollectionImg
      :<|>
      runM3 SetCollectionBlog
      :<|>
      runM2 NewCollection
      :<|>
      runM2 RenameCollection
      :<|>
      runM3 SetMetaData
      :<|>
      runM3 SetMetaData1
      :<|>
      runM3 SetRating
      :<|>
      runM3 SetRating1
      :<|>
      runB2 Snapshot          -- background action: save catalog
      :<|>
      runM1 SyncCollection
      :<|>
      runM1 SyncExif
      :<|>
      runM1 NewSubCollections
      :<|>
      runM3 UpdateCheckSum
      :<|>
      runM3 UpdateTimeStamp

-- ----------------------------------------
--
-- process command line args,
-- build env, configure log command
-- and init server state

main :: IO ()
main = mainWithArgs "servant" $ \ env -> do
  -- create a semaphore for syncing log output
  sem  <- newTMVarIO ()
  let env' = env & envLogOp .~ logCmd sem

  est  <- initState env'
  either die (main' env') est
  where
    -- a log command that syncronizes
    -- output of messages to stderr
    logCmd :: TMVar () -> (String -> IO ())
    logCmd sem s = do
      v <- atomically $ takeTMVar sem
      hPutStrLn stderr s
      hFlush    stderr
      atomically $ putTMVar sem v

main' :: Env -> ImgStore -> IO ()
main' env st = do
  -- create TMVars for reading and modifying image archive store
  -- and a queue for saving the image store in a background thread
  mvRead  <- newTMVarIO st
  mvMody  <- newTMVarIO st
  mvQueue <- newTChanIO

  let runRead :: (forall a . Cmd' a -> Handler a)
      runRead  = runReadCmd       env mvRead
  let runMody :: (forall a . Cmd' a -> Handler a)
      runMody  = runModyCmd       env mvRead mvMody
  let runBg    = runBackgroundCmd env mvRead mvQueue

  -- start background thread for saving image store
  bgq <- async $ jobQueue mvQueue
  link bgq

  -- start servant server
  withStdoutLogger $ \logger -> do
    let settings =
          defaultSettings & setPort   (env ^. envPort)
                          & setLogger logger

    runSettings settings $
      serve (Proxy :: Proxy CatalogAPI) $
      catalogServer env runRead runMody runBg

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

runReadCmd :: Env -> TMVar ImgStore -> Cmd' a -> Handler a
runReadCmd env mvs cmd' = do
  res <- liftIO runc
  either raise500 return res
  where
    runc = do
      -- read the store, but leave the store in the TMVar mvs
      -- for other read operations
      -- in case of an exception, the mvs TMVar is still valid,
      -- no restore has to be done

      store <- atomically $ readTMVar mvs
      (res, _store) <- runAction (evalCmd cmd') env store
                       `catch`
                       -- TODO this still does not catch: error "some error"
                       (\ e ->
                           return
                           (Left . Msg $ show (e :: SomeException), store)
                       )
      return res

runModyCmd :: Env -> TMVar ImgStore -> TMVar ImgStore -> Cmd' a -> Handler a
runModyCmd env mvr mvm cmd' = do
  res <- liftIO runc
  either raise500 return res
  where
    runc = do
      -- the mvm TMVar is read and emptied
      -- nobody else can modify until the modifying action is
      -- finished and the result stored back into mvm TMVar
      --
      -- in case of an exception the mvm TMVar must be restored
      -- with the initial value

      store <- atomically $ takeTMVar mvm
      (res, new'store) <- runAction (evalCmd cmd') env store
                          `catch`
                          (\ e ->
                              return
                              (Left . Msg $ show (e :: SomeException), store)
                          )
      atomically $
        swapTMVar mvr new'store
        >>
        putTMVar  mvm new'store
      return res

runBackgroundCmd :: Env -> TMVar ImgStore -> TChan Job -> Cmd' () -> Handler ()
runBackgroundCmd env mvs queue cmd' = do
  liftIO $
    atomically $
    writeTChan queue $ runc
  return ()
  where
    runc :: Job
    runc = do
      store <- atomically $ readTMVar mvs
      _ <- runAction (evalCmd cmd') env store
      return ()

type Job = IO ()

jobQueue :: TChan Job -> IO ()
jobQueue q = forever $ do
  job <- atomically $ readTChan q
  job


raise500 :: Msg -> Handler a
raise500 (Msg msg) =
  throwError $ err500 { errBody = msg' }
  where
    msg' = show msg ^. from isoString

-- ----------------------------------------
