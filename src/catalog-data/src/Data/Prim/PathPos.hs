{-# LANGUAGE InstanceSigs #-}
module Data.Prim.PathPos
  ( PathPos'(PPs)
  , PathPos
  , VPathPos
  , Pos
  , VPath
  , path2pathPos
  , pathPos2path
  , isoPathPos
  , isoPicNo
  , picNoToText
  , path2colPath
  , mkPathPos
  , isoPPs
  , mkVPath
  , realPath
  , realPathPos
  , virtPath
  )
where

import Data.Prim.Prelude

import Data.Prim.Path
       ( checkAndRemExt
       , isPathPrefix
       , snocPath
       , viewBase
       , Path
       )
import Data.Prim.Constants
       ( p'collections )

import Text.SimpleParser
       ( SP
       , parseMaybe
       , string
       )
import Text.Megaparsec.Char.Lexer
       ( decimal )

-- ----------------------------------------

data PathPos' p = PPs !p !Pos

type Pos = Maybe Int

type PathPos = PathPos' Path

data VPath   = VP
  { _vpx :: !Path           -- points to a symbolic link node (empty COL node marked as link)
  , _rpx :: !Path           -- points to a real COL node
  , _sfx :: !Path           -- _rpx <> _sfx is the path of the referenced COL/IMG node
  }

mkPathPos :: p -> Pos -> PathPos' p
mkPathPos = PPs

mkVPath :: Path -> Path -> Path -> VPath
mkVPath = VP

realPath :: VPath -> Path
realPath vp = _rpx vp <> _sfx vp

realPathPos :: VPathPos -> PathPos
realPathPos = fmap realPath

virtPath :: VPath -> Path
virtPath vp = _vpx vp <> _sfx vp

type VPathPos = PathPos' VPath

deriving instance Functor PathPos'
deriving instance Eq p => Eq (PathPos' p)

instance ToJSON p => ToJSON (PathPos' p) where
  toJSON :: ToJSON p => PathPos' p -> JValue
  toJSON = toJSON . (^. isoPPs)

instance FromJSON p => FromJSON (PathPos' p) where
  parseJSON :: FromJSON p => JValue -> JParser (PathPos' p)
  parseJSON o =  (isoPPs #) <$> parseJSON o

deriving instance Show p => Show (PathPos' p)   -- just for testing
deriving instance Show VPath

isoPPs :: Iso' (PathPos' p) (p, Pos)
isoPPs = iso
         (\(PPs p pos) -> (p, pos))
         (uncurry mkPathPos)

path2pathPos :: Path -> PathPos
path2pathPos p =
  case mcx of
    Just _  -> PPs dp mcx
    Nothing -> PPs p Nothing
  where
    (dp, bn) = p  ^. viewBase
    mcx      = bn ^. isoText . from isoPicNo

pathPos2path :: PathPos -> Path
pathPos2path (PPs p  Nothing) = p
pathPos2path (PPs dp cx) = dp `snocPath` (cx ^. isoPicNo . from isoText)

isoPathPos :: Iso' Path PathPos
isoPathPos = iso path2pathPos pathPos2path

picNoToText :: Int -> Text
picNoToText i
  | i >= 0    = (("pic-" ++ )
                  . fillLeft '0' dgs
                  . show
                  $ i
                ) ^. isoText
  | otherwise = mempty
  where
    dgs = 4  -- 10000 entries in a collection before string gets longer

isoPicNo :: Iso' Pos Text
isoPicNo = iso toS frS
  where
    toS = maybe mempty picNoToText
    frS = parseMaybe picNoParser . (^. isoString)

-- "pic-" prefix is important
-- else collections like 2020 are interpreted as pic no

picNoParser :: SP Int
picNoParser = string "pic-" *> decimal

-- parser for object path
--
-- remove extension
-- parse optional collection index
--
-- example: path2colPath ".jpg" "collections/2018/may/pic-0007.jpg"
--          -> Just ("/collections/2018/may", Just 7)

path2colPath :: String -> Path -> Maybe PathPos
path2colPath ext p =
  (^. isoPathPos)
  <$> ( checkAndRemExt ext p
        >>= guarded (isPathPrefix p'collections)
      )

-- ----------------------------------------
