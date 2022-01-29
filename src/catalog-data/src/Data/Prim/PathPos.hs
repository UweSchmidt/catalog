module Data.Prim.PathPos
  ( PathPos
  , Pos
  , path2pathPos
  , pathPos2path
  , isoPathPos
  , isoPicNo
  , picNoToText
  )
where

import Data.Prim.Prelude

import Data.Prim.Path
       ( snocPath
       , viewBase
       , Path
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
path2pathPos p =
  case mcx of
    Just _  -> (dp, mcx)
    Nothing -> (p, Nothing)
  where
    (dp, bn) = p  ^. viewBase
    mcx      = bn ^. isoText . from isoPicNo

pathPos2path :: PathPos -> Path
pathPos2path (p,  Nothing) = p
pathPos2path (dp, cx) = dp `snocPath` (cx ^. isoPicNo . from isoText)

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

-- ----------------------------------------
