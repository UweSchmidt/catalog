{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module APIf where

import Prelude ()
import Prelude.Compat
       ( otherwise
       , ($)
       , Eq((==))
       , Ord((>))
       , Semigroup((<>))
       , Bool
       , Int
       , Either(..)
       , (&&)
       , id
       , (<$>)
       , maybe
       )

import Network.HTTP.Media
       ( MediaType
       , (//)
       , (/:)
       )

import Servant
       ( CaptureAll
       , Headers
       , Header
       , JSON
       , Post
       , ReqBody
       , OctetStream
       , Get
       , Capture
       , Raw
       , (:>)
       , (:<|>)
       , MimeRender(..)
       , Accept(contentType)
       , Proxy(..)
       , FromHttpApiData(parseUrlPiece)
       )

import Data.Prim
       ( (^.)
       , readGeo'
       , TimeStamp
       , CheckSum
       , CheckSumRes
       , Name
       , Path
       , LazyByteString
       , IsoString(isoString)
       , Geo
       , Text
       )
import Data.History
        ( HistoryID
        )
import Data.ImgTree
       ( ImgNodeP
       )
import Data.MetaData
       ( MetaDataText
       , Rating
       )
import Catalog.GenPages
       ( JPage )

import qualified Data.Text as T

-- ----------------------------------------
-- the complete API

type CatalogAPI
  = ( AudioAPI
      :<|>
      BootstrapAPI
      :<|>
      AssetsAPI
      :<|>
      RootAPI
    )
    :<|>
    JsonAPI
    :<|>
    NewDocAPI

-- ----------------------------------------
--
-- static files API

-- static bootstrap files
type AudioAPI
  = "audio" :> Raw

-- static bootstrap files
type BootstrapAPI =
  "bootstrap" :> Raw

-- static asset files (css, icons, javascript)
type AssetsAPI
  = "assets" :>
    ( "css"        :> Raw
      :<|>
      "icons"      :> Raw
      :<|>
      "javascript" :> Raw
    )

-- root HTML pages (edit.html, index.html)
-- and server version

type RootAPI
  = Capture "root" (BaseName HTMLStatic) :> Get '[HTMLStatic] LazyByteString
    :<|>
    "favicon.ico" :> Get '[ICO] LazyByteString
    :<|> ( "get-gps-cache" :> Get '[OctetStream] LazyByteString
           :<|>
           "put-gps-cache" :> ReqBody '[OctetStream] LazyByteString
                           :> Post    '[JSON] ()
         )

-- ----------------------------------------
--
-- new URL API

type NewDocAPI
  = "docs" :>
    ( IconAPI
      :<|>
      IconpAPI
      :<|>
      ImgAPI
      :<|>
      ImgfxAPI
      :<|>
      PageAPI
    )
    :<|>
    ArchiveAPI

-- the whole archive dir tree can be served statically and accessed directly
-- via /archive/photos/.../file.ext
-- this is done with movie (.mp4) files, those are served
-- statically due to streaming support in warp library

type ArchiveAPI = "archive" :> "photos" :> Raw


-- a lazy bytestring as response with a cache control header

type CachedByteString
  = Headers '[Header "Cache-Control" Text] LazyByteString

--
-- icons: /icon/<w>x<h>/collections/<path>.jpg           -- collection icon
--        /icon/<w>x<h>/collections/<path>/pic-<ix>.jpg  -- col entry  icon
--
-- the .jpg extension and the pic-<ix> part must be parsed by the handler
-- due to restricted servant URL parsing capabilities

type IconAPI
  = "icon" :> Capture "geo" Geo':> CaptureAll "path" Text :>
    Header "Referer" Text :>
    Get '[JPEG] CachedByteString

type IconpAPI
  = "iconp" :> Capture "geo" Geo':> CaptureAll "path" Text :>
    Header "Referer" Text :>
    Get '[JPEG] CachedByteString

type ImgAPI
  = "img" :> Capture "geo" Geo':> CaptureAll "path" Text :>
    Header "Referer" Text :>
    Get '[JPEG] CachedByteString

type ImgfxAPI
  = "imgfx" :> Capture "geo" Geo':> CaptureAll "path" Text :>
    Header "Referer" Text :>
    Get '[JPEG] CachedByteString

type PageAPI
  = "page"  :> PageAPIfmt
    :<|>
    "page1" :> PageAPIfmt
    :<|>
    "json" :> PageAPIjson

type PageAPIfmt
  = Capture "geo" Geo':> CaptureAll "path" Text :>
    Get '[HTMLStatic] LazyByteString

type PageAPIjson
  = Capture "geo" Geo':> CaptureAll "path" Text :>
    Get '[JSON] JPage

-- ----------------------------------------
--
-- presentation API: .html album pages and .jpg images

-- generate blaze-1920x1200 and other geometry HTML pages

newtype Geo'      = Geo'      Geo

-- ----------------------------------------
--
-- the JSON API for editing and viewing collection and images

type JsonAPI
  = JsonGetAPI
    :<|>
    JsonModifyAPI

-- simple op with a single path argument
-- and without request body
-- and a JSON result

type SimplePost r
  = CaptureAll "path" Text :> Post '[JSON] r

-- op with extra args given as request body
-- with JSON format

type ParamPost a r
  = CaptureAll "path" Text :> ReqBody '[JSON] a :> Post '[JSON] r

-- the query ops

type JsonGetAPI
  = "get" :>
    ( "collection"      :> SimplePost ImgNodeP
      :<|>
      "isWriteable"     :> SimplePost Bool
      :<|>
      "isRemovable"     :> SimplePost Bool
      :<|>
      "isSortable"      :> SimplePost Bool
      :<|>
      "isCollection"    :> SimplePost Bool
      :<|>
      "blogcontents"    :> ParamPost Int Text
      :<|>
      "blogsource"      :> ParamPost Int Text
      :<|>
      "metadata"        :> ParamPost Int MetaDataText
      :<|>
      "rating"          :> ParamPost Int Rating
      :<|>
      "ratings"         :> SimplePost [Rating]
      :<|>
      "mediaPath"       :> SimplePost [Path]
      :<|>
      "checkimgpart"    :> ParamPost (Bool, Name) CheckSumRes
    )

-- the modifying ops

type JsonModifyAPI
  = "modify" :>
    ( "saveblogsource"       :> ParamPost (Int, Text) ()
      :<|>
      "changeWriteProtected" :> ParamPost ([Int], Bool) ()
      :<|>
      "sort"                 :> ParamPost [Int] ()
      :<|>
      "sortByDate"           :> ParamPost [Int] ()
      :<|>
      "removeFromCollection" :> ParamPost [Int] ()
      :<|>
      "copyToCollection"     :> ParamPost ([Int], Path) ()
      :<|>
      "moveToCollection"     :> ParamPost ([Int], Path) ()
      :<|>
      "colimg"               :> ParamPost (Path, Int) ()
      :<|>
      "colblog"              :> ParamPost (Path, Int) ()
      :<|>
      "newcol"               :> ParamPost Name ()
      :<|>
      "renamecol"            :> ParamPost Name ()
      :<|>
      "setMetaData"          :> ParamPost ([Int], MetaDataText) ()
      :<|>
      "setMetaData1"         :> ParamPost (Int, MetaDataText) ()
      :<|>
      "setRating"            :> ParamPost ([Int], Rating) ()
      :<|>
      "setRating1"           :> ParamPost (Int, Rating) ()
      :<|>
      "snapshot"             :> ParamPost Text ()
      :<|>
      "syncCol"              :> SimplePost ()
      :<|>
      "syncExif"             :> ParamPost (Bool, Bool) ()
      :<|>
      "newSubCols"           :> SimplePost ()
      :<|>
      "updateCheckSum"       :> ParamPost (CheckSum, Name) ()
      :<|>
      "updateTimeStamp"      :> ParamPost (TimeStamp, Name) ()
      :<|>
      "newUndoEntry"         :> ParamPost Text HistoryID
      :<|>
      "applyUndo"            :> ParamPost HistoryID ()
      :<|>
      "dropUndoEntries"      :> ParamPost HistoryID ()
      :<|>
      "listUndoEntries"      :> SimplePost [(HistoryID, Text)]
    )

-- ----------------------------------------
--
-- Geo' is a wrapper around Geo to avoid
-- warnings about orphan instance

instance FromHttpApiData Geo' where
  parseUrlPiece s =
    maybe (defaultParseError s) Right g
    where
      g = Geo' <$> readGeo' (s ^. isoString)

-- | Default parsing error.
defaultParseError :: Text -> Either Text a
defaultParseError input = Left ("could not parse: `" <> input <> "'")

-- ----------------------------------------
--
-- the static handlers are a bit clumsy
-- but no idea, how to deliver static files with right mimetype
-- with servant, no good example found yet

-- ----------------------------------------
--
-- JPEG static handler

data JPEG

instance Accept JPEG where
  contentType   :: Proxy JPEG -> MediaType
  contentType _ = "image" // "jpeg"

instance MimeRender JPEG LazyByteString where
  mimeRender :: Proxy JPEG -> LazyByteString -> LazyByteString
  mimeRender _ = id

instance HasExt JPEG where
  theExt   :: Proxy JPEG -> Text
  theExt _ = ".jpg"

-- ----------------------------------------
--
-- ICO static handler

data ICO

instance Accept ICO where
  contentType   :: Proxy ICO -> MediaType
  contentType _ = "image" // "x-icon"

instance MimeRender ICO LazyByteString where
  mimeRender :: Proxy ICO -> LazyByteString -> LazyByteString
  mimeRender _ = id

instance HasExt ICO where
  theExt   :: Proxy ICO -> Text
  theExt _ = ".ico"

-- ----------------------------------------
--
-- JSON static handler

data JSStatic

instance Accept JSStatic where
  contentType   :: Proxy JSStatic -> MediaType
  contentType _ = "application" // "javascript"

instance MimeRender JSStatic LazyByteString where
  mimeRender   :: Proxy JSStatic -> LazyByteString -> LazyByteString
  mimeRender _ = id

-- instance MimeUnrender JSStatic LazyByteString where
--  mimeUnrender _ = id

instance HasExt JSStatic where
  theExt   :: Proxy JSStatic -> Text
  theExt _ = ".js"

-- --------------------
--
-- HTML static handler

data HTMLStatic

instance Accept HTMLStatic where
  contentType   :: Proxy HTMLStatic -> MediaType
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTMLStatic LazyByteString where
  mimeRender :: Proxy HTMLStatic -> LazyByteString -> LazyByteString
  mimeRender _ = id

instance HasExt HTMLStatic where
  theExt   :: Proxy HTMLStatic -> Text
  theExt _ = ".html"


-- ----------------------------------------

class HasExt a where
  theExt :: Proxy a -> Text

  hasExt :: Proxy a -> Text -> Bool
  hasExt px t =
    T.length t > lx
    &&
    T.toLower (T.takeEnd lx t) == ex
    where
      ex = theExt px
      lx = T.length ex

-- ----------------------------------------

newtype BaseName a = BaseName {unBaseName :: Text}

instance HasExt a => FromHttpApiData (BaseName a) where
  parseUrlPiece s
    | hasExt px s = Right $ BaseName s
    | otherwise   = defaultParseError s
    where
      px :: Proxy a
      px = Proxy

-- ----------------------------------------
