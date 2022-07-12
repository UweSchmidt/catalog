module Data.Prim.ReqType
  ( ReqType (..)
  , imgReqTypes
  , reqType2AR
  , reqType2ext
  )
where

import Data.Prim.Prelude

import Data.Prim.Geometry
       ( AspectRatio(..) )

-- ----------------------------------------

data ReqType = RPage    -- deliver HTML col-, img-, movie-, blog page  text/html
             | RPage1
             | RIcon    -- deliver JPG icon, fixed aspectratio         image/jpg
             | RIconp   -- like RIcon with org img aspectratio         image/jpg
             | RImg     -- deliver JPG image                           image/jpg
             | RImgfx   -- JPG imgage, cropped if aspectratio is
                        -- similar to required geometry                image/jpg
             | RBlog    -- ???
             | RRef     -- deliver an url, not a content
             | RMovie   -- currently redundant, movie files are served statically
             | RGif     -- Wackelgif
             | RJson    -- JSON data for client generated pages
             deriving (Eq, Ord, Enum, Show, Read)
             -- !!! nameing convention of constr RXxxx used in isoString

instance IsoText ReqType

instance IsoString ReqType where
  isoString = iso show'ReqType (fromMaybe RRef . read'ReqType)

instance PrismString ReqType where
  prismString = prism' show'ReqType read'ReqType

instance ToJSON ReqType where
  toJSON = toJSON . show'ReqType

instance FromJSON ReqType where
  parseJSON t = do
    r <-  parseJSON t
    maybe mzero return $ read'ReqType r

show'ReqType :: ReqType -> String
show'ReqType = drop 1 . map toLower . show

read'ReqType :: String -> Maybe ReqType
read'ReqType =
  readMaybe . ('R' :) . (_head %~ toUpper)   --    (\ (x : xs) -> 'R' : toUpper x : xs) . (++ " ")

imgReqTypes :: [ReqType]
imgReqTypes = [RIcon .. RImgfx]

-- ----------------------------------------

reqType2AR :: ReqType -> AspectRatio
reqType2AR RIcon  = Fix    -- fixed aspect ratio
reqType2AR RIconp = Pad    -- aspect ratio of org img
reqType2AR RImg   = Pad
reqType2AR RImgfx = Flex   -- flex aspect ratio (crop or pad)
reqType2AR _      = Pad    -- default

reqType2ext :: ReqType -> Text
reqType2ext RPage  = ".html"
reqType2ext RPage1 = ".html"
reqType2ext RImg   = ".jpg"
reqType2ext RImgfx = ".jpg"
reqType2ext RIcon  = ".jpg"
reqType2ext RIconp = ".jpg"
reqType2ext RJson  = ".json"
reqType2ext _      = ""

-- ----------------------------------------
