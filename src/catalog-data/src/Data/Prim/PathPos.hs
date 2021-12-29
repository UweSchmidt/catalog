module Data.Prim.PathPos
  ( PathPos
  , Pos
  , path2pathPos
  , pathPos2path
  , isoPathPos
  , isoPicNo
  )
where

import Data.Prim.Path
       ( snocPath
       , viewBase
       , Path
       )
import Data.Prim.Prelude
       ( fromMaybe
       , (^.)
       , from
       , iso
       , fillLeft
       , Iso'
       , IsoString(isoString)
       )

import Text.SimpleParser
       ( SP
       , parseMaybe
       , string
       )
import Text.Megaparsec.Char.Lexer
       ( decimal )

-- ----------------------------------------

type PathPos = (Path, Pos)
type Pos     = Maybe Int

path2pathPos :: Path -> PathPos
path2pathPos p
  | cx >= 0   = (dp, Just cx)
  | otherwise = (p,  Nothing)
  where
    (dp, bn) = p  ^. viewBase
    cx       = bn ^. isoString . from isoPicNo

pathPos2path :: PathPos -> Path
pathPos2path (p,  Nothing) = p
pathPos2path (dp, Just cx) = dp `snocPath` (cx ^. isoPicNo . from isoString)

isoPathPos :: Iso' Path PathPos
isoPathPos = iso path2pathPos pathPos2path

isoPicNo :: Iso' Int String
isoPicNo = iso toS frS
  where
    toS i
      | i >= 0    = ("pic-" ++ )
                    . fillLeft '0' dgs
                    . show
                    $ i
      | otherwise = mempty
      where
        dgs = 4  -- 10000 entries in a collection before string gets longer

    frS s =
      fromMaybe (-1) $ parseMaybe picNoParser s

-- "pic-" prefix is important
-- else collections like 2020 are interpreted as pic no

picNoParser :: SP Int
picNoParser = string "pic-" *> decimal

-- ----------------------------------------
