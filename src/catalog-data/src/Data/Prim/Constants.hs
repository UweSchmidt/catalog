{-# LANGUAGE OverloadedStrings #-}

module Data.Prim.Constants where

import Data.Prim.Name
       ( Name )

import Data.Prim.Path

import Data.Prim.Prelude

n'archive
  , n'albums
  , n'bycreatedate
  , n'clipboard
  , n'collections
  , n'imports
  , n'photos
  , n'trash :: Name

n'archive      = "archive"
n'albums       = "albums"
n'bycreatedate = "timeline"
n'clipboard    = "clipboard"
n'collections  = "collections"
n'imports      = "imports"
n'photos       = "photos"
n'trash        = "trash"

t'archive
  , t'collections
  , t'photos :: Text

t'archive     = n'archive     ^. isoText
t'collections = n'collections ^. isoText
t'photos      = n'photos      ^. isoText

s'bycreatedate
  , s'clipboard
  , s'collections
  , s'photos
  , s'trash :: String

s'bycreatedate = n'bycreatedate ^. isoString
s'collections  = n'collections  ^. isoString
s'photos       = n'photos       ^. isoString
s'clipboard    = n'clipboard    ^. isoString
s'trash        = n'trash        ^. isoString

p'archive
  , p'arch'photos
  , p'albums
  , p'clipboard
  , p'collections
  , p'bycreatedate
  , p'imports
  , p'photos
  , p'trash :: Path

p'archive      = mkPath n'archive
p'arch'photos  = p'archive     `snocPath` n'photos
p'collections  = p'archive     `snocPath` n'collections
p'albums       = p'collections `snocPath` n'albums
p'bycreatedate = p'collections `snocPath` n'bycreatedate
p'imports      = p'collections `snocPath` n'imports
p'photos       = p'collections `snocPath` n'photos
p'clipboard    = p'collections `snocPath` n'clipboard
p'trash        = p'collections `snocPath` n'trash

p'assets
  , p'html
  , p'icons
  , p'javascript
  , p'css
  , p'blank
  , p'vico
  , p'qmark :: Path

p'assets       = mkPath "assets"
p'html         = p'assets  `snocPath` "html"
p'icons        = p'assets  `snocPath` "icons"
p'javascript   = p'assets  `snocPath` "javascript"
p'css          = p'assets  `snocPath` "css"
p'blank        = p'icons   `snocPath` "blank.jpg"
p'vico         = p'icons   `snocPath` "video-play-3-64.png"
p'qmark        = p'icons   `snocPath` "qm2.jpg"

p'docroot
  , p'exifcache
  , p'zipcache
  , p'gen'icon :: Path

p'docroot      = mkPath "docs"
p'exifcache    = p'docroot `snocPath` "exif-meta"
p'zipcache     = p'docroot `snocPath` "zip-cache"
p'gen'icon     = (p'docroot `snocPath` "generated") `snocPath` "icon"

p'audio
  , p'bootstrap
  , p'cache
  , p'iconsgen  :: Path

p'audio     = mkPath "audio"
p'bootstrap = mkPath "bootstrap"
p'cache     = mkPath "cache" -- old url scheme
p'iconsgen  = (p'cache `concPath` p'icons) `snocPath` "generated" -- old url scheme

-- ----------------------------------------

-- constants for generated collections

tt'albums
  , tt'bydate
  , tt'clipboard
  , tt'imports
  , tt'trash
  , tt'collections
  , tt'photos :: Text

tt'bydate      = "Geordnet nach Datum"
tt'clipboard   = "Clipboard"
tt'albums      = "Alle Alben"
tt'imports     = "Import Historie"
tt'trash       = "Papierkorb"
tt'collections = "Uwe alle seine Bilder"
tt'photos      = "Alle Ordner"

tt'year :: String -> Text
tt'year y = y ^. isoText

tt'month :: String -> String -> Text
tt'month y m =
  unwords [ de'month (read m)
          , y
          ]
  ^. isoText

tt'day :: String -> String -> String -> Text
tt'day y m d =
  unwords [ show  (read d :: Int) ++ "."
          , de'month (read m)
          , y
          ]
  ^. isoText

to'colandname
  , to'dateandtime
  , to'name :: Text

to'colandname  = "colandname"
to'dateandtime = "dateandtime"
to'name        = "name"

-- ----------------------------------------

de'month :: Int -> String
de'month i = [ "Januar", "Februar", "März", "April", "Mai", "Juni"
             , "Juli", "August", "September","Oktober", "November", "Dezember"
             ] !! (i - 1)

-- ----------------------------------------
