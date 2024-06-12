------------------------------------------------------------------------------
--
-- the preliminary main module for testing
-- should be moved to an app

module Main
where

import Prelude ()
import Prelude.Compat
       ( (++)
       , ($)
       , (<$>)
       , (.)
       , Eq((==))
       , Monad(return)
       , Num((*))
       , Show(show)
       , Applicative((*>), (<*))
       , Monoid(mempty)
       , Bool
       , Int
       , Maybe(..)
       , IO
       , Either(..)
       , FilePath
       , String
       , const
       , either
       , otherwise
       , uncurry
       )

-- polysemy and polysemy-tools
import Polysemy.State.RunTMVar
       ( createJobQueue )

import Polysemy.FileSystem
       ( TextPath )

-- catalog-polysemy
import Catalog.CatalogIO
       ( initImgStore )

import Catalog.CatEnv
       ( CatEnv
       , appEnvCat
       , appEnvJournal
       , appEnvLogLevel
       , appEnvPort
       , catJsonArchive
       , catMountPath
       , catGPSCache
       , catFontName
       )
import Catalog.Effects.CatCmd
       ( applyUndo
       , changeWriteProtected
       , checkImgPart
       , copyToCollection
       , dropUndoEntries
       , htmlPage
       , jsonPage
       , isCollection
       , isRemovable
       , isSortable
       , isWriteable
       , jpgImgCopy
       , listUndoEntries
       , moveToCollection
       , newCollection
       , newSubCollections
       , newUndoEntry
       , removeFromCollection
       , renameCollection
       , saveBlogSource
       , setCollectionBlog
       , setCollectionImg
       , setMetaData
       , setMetaData1
       , setRating
       , setRating1
       , snapshot
       , sortCollByDate
       , sortCollection
       , staticFile
       , syncCollection
       , syncExif
       , theBlogContents
       , theBlogSource
       , theEntry
       , theMediaPath
       , theMetaDataText
       , theRating
       , theRatings
       , updateCheckSum
       , updateTimeStamp
       )
import Catalog.Effects.CatCmd.Interpreter
       ( writeStaticFile )

import Catalog.GenImages
       ( selectFont )

import Catalog.GenPages
       ( JPage )

import Catalog.History
       ( emptyHistory )

import Catalog.Run
       ( CatApp
       , JournalHandle
       , runRead
       , runMody
       , runBG
       , runLogQ
       )

-- catalog-data
import Data.Prim
       ( Path
       , ReqType(..)
       , Text
       , (^.)
       , (#)
       , (.~)
       , Alternative(some, many)
       , LazyByteString
       , IsoString(isoString)
       , IsoText(isoText)
       , optional
       , (&)
       , fromMaybe
       , p'arch'photos
       , p'html
       , p'icons
       , p'javascript
       , p'bootstrap
       , p'css
       , p'icons
       , p'javascript
       , initPath
       , listToPath
       , snocPath
       , tailPath
       , showPath
       , from
       )
import Data.ImageStore
       ( emptyImgStore )

import Text.SimpleParser
       ( anyStringThen
       , matchP
       , char
       , digitChar
       , string
       , try
       , eof
       )

-- libs
import Control.Concurrent.STM.TMVar
       ( newTMVarIO )

import Control.Monad.IO.Class
       ( liftIO )

import Data.IORef
       ( newIORef )

import System.Exit
       ( die )
import System.IO
       ( stdout
       , stderr
       , IOMode(WriteMode)
       , openFile
       )

-- servant libs
import Servant
       ( JSON
       , Server
       , Handler
       , Proxy(..)
       , ServerError(errBody)
       , type (:<|>)((:<|>))
       , addHeader
       , serve
       , err500
       , serveDirectoryWebApp
       , throwError
       )

import Network.Wai.Handler.Warp
       ( setPort
       , setLogger
       , defaultSettings
       , runSettings
       )

-- servant interface
import APIf
       ( CachedByteString
       , CatalogAPI
       , BaseName(BaseName)
       , HTMLStatic
       , JSStatic
       , ICO
       , Geo'(..)
       )

import Options
       ( serverOptions )
import Logger
       ( withCatLogger )

------------------------------------------------------------------------------

catalogServer :: CatEnv
              -> (forall a . CatApp a -> Handler a)
              -> (forall a . CatApp a -> Handler a)
              -> (forall a . CatApp a -> Handler ())
              -> Server CatalogAPI
catalogServer env runReadC runModyC runBGC =
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
      :<|>
      ( get'gpscache'json
        :<|>
        put'gpscache'json
      )
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
      :<|>
      get'json
    )
  )
  :<|>
  get'archive

  where
    mountPath :: FilePath
    mountPath = env ^. catMountPath . isoString

    static p = do
      serveDirectoryWebApp (mountPath ++ showPath p)

    bootstrap         = static p'bootstrap
    assets'css        = static p'css
    assets'icons      = static p'icons
    assets'javascript = static p'javascript

    -- movies are served statically to enable streaming
    -- the original .mp4 movies are accessed
    -- by a path prefix "/archive/photos/" ++ <path-to-mp4>

    get'archive       = static (tailPath p'arch'photos)

    -- root html files are located under /assets/html

    root'html :: BaseName HTMLStatic -> Handler LazyByteString
    root'html bn = staticDoc' $ staticPath p'html bn

    gpscachePath :: Path
    gpscachePath =
      staticPath (initPath (isoText # (env ^. catJsonArchive))) bn
      where
        bn :: BaseName JSON
        bn = BaseName $ env ^. catGPSCache

    get'gpscache'json :: Handler LazyByteString
    get'gpscache'json = dynDoc' gpscachePath

    put'gpscache'json :: LazyByteString -> Handler ()
    put'gpscache'json =
      runModyC . putStaticFile
      where
        putStaticFile :: LazyByteString -> CatApp ()
        putStaticFile lbs = do
          writeStaticFile gpscachePath lbs

    favicon'ico :: Handler LazyByteString
    favicon'ico = staticDoc' $ staticPath p'icons bn
      where
        bn :: BaseName ICO
        bn = BaseName "favicon.ico"

    rpc'js :: Handler LazyByteString
    rpc'js = staticDoc' $ staticPath p'javascript bn
      where
        bn :: BaseName JSStatic
        bn = BaseName "rpc-servant.js"

    dynDoc' :: Path -> Handler LazyByteString
    dynDoc' p = runModyC . staticFile $ p ^. isoText

    staticDoc' :: Path -> Handler LazyByteString
    staticDoc' p = runReadC . staticFile $ p ^. isoText

    staticPath :: Path -> BaseName a -> Path
    staticPath dirPath (BaseName n) =
      dirPath `snocPath` (isoText # n)

    -- --------------------

    cachedResponse :: Maybe Text -> LazyByteString -> CachedByteString
    cachedResponse mbref bs =
      addHeader (cval ^. isoText) bs
      where
        cval
          | isEditRef ref = "no-store"    -- disable caching in browser (at least in Chrome)
          | otherwise     = "public, max-age=" ++ show aDay

        ref = fromMaybe mempty mbref ^. isoString

        aDay :: Int
        aDay = 24 * 60 * 60

        -- check whether a referer url is of form ".../edit.html" or ".../edit-x.y.z.html"
        -- with x.y.z as version number

        isEditRef :: String -> Bool
        isEditRef r = matchP eref r
          where
            eref = anyStringThen edit
            edit = string "/edit" <* optional vers <* string ".html" <* eof
            vers = char '-' *> some digitChar *> many subv
            subv = try $ char '.' *> some digitChar

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
      res <- runReadC . jpgImgCopy rt geo . listToPath $ ts
      return $ cachedResponse referer res

     -- --------------------
    -- handle html pages

    get'html :: Geo' -> [Text] -> Handler LazyByteString
    get'html = get'html' RPage

    get'html1 :: Geo' -> [Text] -> Handler LazyByteString
    get'html1 = get'html' RPage1

    get'html' :: ReqType -> Geo' -> [Text] -> Handler LazyByteString
    get'html' rt (Geo' geo) =
      runReadC . htmlPage rt geo . listToPath

    get'json :: Geo' -> [Text] -> Handler JPage
    get'json (Geo' geo) =
      runReadC . jsonPage geo . listToPath

-- --------------------
{-
    runR0 :: forall a.
             CatApp a -> [Text] -> Handler a
    runR0 cmd' _ts = runReadC cmd'      -- throw away redundant path
-}
    runR1 :: forall a .
             (Path -> CatApp a) -> [Text] -> Handler a
    runR1 cmd' = runReadC  . cmd' . listToPath

    runR2 :: forall a1 a.
             (a1 -> Path -> CatApp a) -> [Text] -> a1 -> Handler a
    runR2 cmd' ts args = runReadC  . cmd' args . listToPath $ ts

    runR3 :: forall a1 a2 a.
             (a1 -> a2 -> Path -> CatApp a) -> [Text] -> (a1, a2) -> Handler a
    runR3 = runR2 . uncurry

    json'read =
      runR1 theEntry
      :<|>
      runR1 isWriteable
      :<|>
      runR1 isRemovable
      :<|>
      runR1 isSortable
      :<|>
      runR1 isCollection
      :<|>
      runR2 theBlogContents
      :<|>
      runR2 theBlogSource
      :<|>
      runR2 theMetaDataText
      :<|>
      runR2 theRating
      :<|>
      runR1 theRatings
      :<|>
      runR1 theMediaPath
      :<|>
      runR3 checkImgPart

    runM0 :: forall a.
             CatApp a -> [Text] -> Handler a
    runM0 cmd' _ts = runModyC cmd'      -- throw away redundant path

    runX1 :: forall a1 a.
             (a1 -> CatApp a) -> [Text] -> a1 -> Handler a
    runX1 cmd' _ts = runModyC . cmd'    -- throw away redundant path

    runM1 :: forall a.
             (Path -> CatApp a) -> [Text] -> Handler a
    runM1 cmd' = runModyC . cmd' . listToPath

    runM2 :: forall a1 a.
             (a1 -> Path -> CatApp a) -> [Text] -> a1 -> Handler a
    runM2 cmd' ts args = runModyC . cmd' args . listToPath $ ts

    runM3 :: forall a1 a2 a.
             (a1 -> a2 -> Path -> CatApp a) -> [Text] -> (a1, a2) -> Handler a
    runM3 = runM2 . uncurry

    runB2 :: forall a1 a.
             (a1 -> Path -> CatApp a) -> [Text] -> a1 -> Handler ()
    runB2 cmd' ts args = runBGC . cmd' args . listToPath $ ts

    json'modify =
      runM3 saveBlogSource
      :<|>
      runM3 changeWriteProtected
      :<|>
      runM2 sortCollection
      :<|>
      runM2 sortCollByDate
      :<|>
      runM2 removeFromCollection
      :<|>
      runM3 copyToCollection
      :<|>
      runM3 moveToCollection
      :<|>
      runM3 setCollectionImg
      :<|>
      runM3 setCollectionBlog
      :<|>
      runM2 newCollection
      :<|>
      runM2 renameCollection
      :<|>
      runM3 setMetaData
      :<|>
      runM3 setMetaData1
      :<|>
      runM3 setRating
      :<|>
      runM3 setRating1
      :<|>
      runB2 snapshot          -- background action: save catalog
      :<|>
      runM1 syncCollection
      :<|>
      runM3 syncExif
      :<|>
      runM1 newSubCollections
      :<|>
      runM3 updateCheckSum
      :<|>
      runM3 updateTimeStamp
      :<|>
      runX1 newUndoEntry
      :<|>
      runX1 applyUndo
      :<|>
      runX1 dropUndoEntries
      :<|>
      runM0 listUndoEntries

----------------------------------------

main :: IO ()
main = do
  env   <- serverOptions
  logQ  <- createJobQueue

  rvar  <- newTMVarIO emptyImgStore
  mvar  <- newTMVarIO emptyImgStore
  qu    <- createJobQueue

  hist  <- newIORef emptyHistory

  jh    <- openJournal (env ^. appEnvJournal)

  let runRC :: CatApp a -> Handler a
      runRC = ioeither2Handler . runRead rvar logQ env

  let runMC :: CatApp a -> Handler a
      runMC = ioeither2Handler . runMody jh hist rvar mvar logQ env

  let runBQ :: CatApp a -> Handler ()
      runBQ = liftIO . runBG jh rvar qu logQ env

  -- set the fontname to be used when
  -- generating icons from text

  env1 <- do
    efn <- runRead rvar logQ env selectFont
    return $
      either
      (const env)
      (\ fn -> env & appEnvCat . catFontName .~ fn)
      efn

  -- load the catalog from json file
  do
    eres <-runMody jh hist rvar mvar logQ env1 initImgStore
    either
      (die . (^. isoString))    -- no catalog loaded
      return
      eres

  -- start servant server
  withCatLogger (runLogQ (env1 ^. appEnvLogLevel) logQ) $
    \logger -> do
      let settings =
            defaultSettings & setPort   (env ^. appEnvPort)
                            & setLogger logger

      runSettings settings $
        serve (Proxy :: Proxy CatalogAPI) $
        catalogServer (env1 ^. appEnvCat) runRC runMC runBQ


ioeither2Handler :: IO (Either Text a) -> Handler a
ioeither2Handler cmd = do
  res <- liftIO cmd
  either raise500 return res
  where
    raise500 :: Text -> Handler a
    raise500 msg =
      throwError $ err500 { errBody = msg ^. isoString . from isoString }

openJournal :: Maybe TextPath -> IO JournalHandle
openJournal tp = case tp of
  Nothing
    -> return Nothing
  Just p
    | p == "1"
      -> return $ Just (Left stdout)
    | p == "2"
      -> return $ Just (Left stderr)
    | otherwise
      -> Just . Right <$> openFile (p ^. isoString) WriteMode

------------------------------------------------------------------------------
