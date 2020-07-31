{-# LANGUAGE
    ConstraintKinds,
    DataKinds,
    FlexibleContexts,
    GADTs,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- {-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
--
-- the preliminary main module for testing
-- should be moved to an app

module Main
where

import Prelude ()
import Prelude.Compat

-- polysemy and polysemy-tools
import Polysemy.State.RunTMVar (createJobQueue)

-- catalog-polysemy
import Catalog.CatalogIO       (initImgStore)
import Catalog.CatEnv          ( CatEnv
                               , appEnvCat
                               , appEnvLogLevel
                               , appEnvPort
                               , catMountPath
                               , catFontName
                               )
import Catalog.Effects.CatCmd
import Catalog.GenImages       ( selectFont )
import Catalog.History         ( emptyHistory )
import Catalog.Run             ( CatApp
                               , runRead
                               , runMody
                               , runBG
                               , runLogQ
                               )

-- catalog-data
import Data.Prim
import Data.ImageStore         (emptyImgStore)
import Text.SimpleParser

-- libs
import Control.Concurrent.STM.TMVar (newTMVarIO)
import Control.Monad.IO.Class       (liftIO)
import Data.IORef
import System.Exit                  (die)

-- servant libs
import Servant

import Network.Wai.Handler.Warp     ( setPort
                                    , setLogger
                                    , defaultSettings
                                    , runSettings
                                    )

-- servant interface
import APIf
import Options
import Logger

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
  )
  :<|>
  get'archive

  where
    mountPath :: FilePath
    mountPath = env ^. catMountPath . isoString

    static p = do
      serveDirectoryWebApp (mountPath ++ p ^. isoString)

    bootstrap         = static ps'bootstrap
    assets'css        = static ps'css
    assets'icons      = static ps'icons
    assets'javascript = static ps'javascript

    -- movies are served statically to enable streaming
    -- the original .mp4 movies are accessed
    -- by a path prefix "/archive/photos/" ++ <path-to-mp4>

    get'archive       = static (tailPath p'arch'photos)

    -- root html files are located under /assets/html

    root'html :: BaseName HTMLStatic -> Handler LazyByteString
    root'html bn = staticDoc p'html bn

    favicon'ico :: Handler LazyByteString
    favicon'ico = staticDoc p'icons bn
      where
        bn :: BaseName ICO
        bn = BaseName "favicon.ico"

    rpc'js :: Handler LazyByteString
    rpc'js = staticDoc p'javascript bn
      where
        bn :: BaseName JSStatic
        bn = BaseName "rpc-servant.js"

    staticDoc :: Path -> BaseName a -> Handler LazyByteString
    staticDoc dirPath (BaseName n) =
      runReadC $ staticFile ((dirPath `snocPath` (isoText # n)) ^. isoText)

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

    -- --------------------

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
      runR2 theMetaData
      :<|>
      runR2 theRating
      :<|>
      runR1 theRatings
      :<|>
      runR1 theMediaPath
      :<|>
      runR3 checkImgPart
{-
    runM0 :: forall a.
             CatApp a -> [Text] -> Handler a
    runM0 cmd' _ts = runModyC cmd'      -- throw away redundant path
-}
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
      runM1 syncExif
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

----------------------------------------

main :: IO ()
main = do
  env   <- serverOptions
  logQ  <- createJobQueue

  rvar  <- newTMVarIO emptyImgStore
  mvar  <- newTMVarIO emptyImgStore
  qu    <- createJobQueue

  hist  <- newIORef emptyHistory

  let runRC :: CatApp a -> Handler a
      runRC = ioeither2Handler . runRead rvar logQ env

  let runMC :: CatApp a -> Handler a
      runMC = ioeither2Handler . runMody hist rvar mvar logQ env

  let runBQ :: CatApp a -> Handler ()
      runBQ = liftIO . runBG rvar qu logQ env

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
    eres <-runMody hist rvar mvar logQ env1 initImgStore
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

------------------------------------------------------------------------------
