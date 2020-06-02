{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Prim.ReqType
  ( ReqType (..)
  , imgReqTypes
  , reqType2AR
  )
where

import Data.Prim.Prelude
import Data.Prim.Geometry

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
             deriving (Eq, Ord, Enum, Show, Read)
             -- !!! nameing convention of constr RXxxx used in isoString

instance IsoText ReqType

instance IsoString ReqType where
  isoString = iso show'ReqType (fromMaybe RRef . read'ReqType)

instance PrismString ReqType where
  prismString = prism' show'ReqType read'ReqType

show'ReqType :: ReqType -> String
show'ReqType = drop 1 . map toLower . show

read'ReqType :: String -> Maybe ReqType
read'ReqType =
  readMaybe . (\ (x : xs) -> 'R' : toUpper x : xs) . (++ " ")

imgReqTypes :: [ReqType]
imgReqTypes = [RIcon .. RImgfx]

-- ----------------------------------------

reqType2AR :: ReqType -> AspectRatio
reqType2AR RIcon  = Fix    -- fixed aspect ratio
reqType2AR RIconp = Pad    -- aspect ratio of org img
reqType2AR RImg   = Pad
reqType2AR RImgfx = Flex   -- flex aspect ratio (crop or pad)
reqType2AR _      = Pad    -- default

-- ----------------------------------------
