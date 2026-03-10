module Text.ParsePretty
  ( Tuple3
  , Tuple4

  , parseDate
  , parseTime
  , parseDateTime
  , toYMD
  , toYMDhms

  , ymd2Text
  , hms2Text
  , isoDateInt

  , fmtDate
  , fmtYMD
  , fmtYMDRange
  , fmtKWTitle
  , fmtKWSubTitle
  )
where

import Data.Prim.Prelude

import Text.SimpleParser
  ( SP
  , anyString
  , char
  , count
  , digitChar
  -- , matchP
  , oneOf'
  , parseMaybe
  , spaceChar
  )

import Text.Printf
  ( printf,
  )

import qualified Data.Text         as T
import qualified Text.SimpleParser as SP

-- ------------------------------------------------------------

parseDate :: Text -> Maybe (Tuple3 Int)
parseDate = parseMaybe' dateParser
{-# INLINE parseDate #-}

parseTime :: Text -> Maybe (Tuple4 Int)
parseTime = parseMaybe' timeParser
{-# INLINE parseTime #-}

parseDateTime :: Text -> Maybe (Tuple3 Int, Tuple4 Int)
parseDateTime = parseMaybe' dateTimeParser
{-# INLINE parseDateTime #-}

toYMD :: Text -> Tuple3 Int
toYMD = fromMaybe (0, 0, 0) . parseDate
{-# INLINE toYMD #-}

toYMDhms :: Text -> (Tuple3 Int, Tuple4 Int)
toYMDhms = fromMaybe ((0, 0, 0), (0, 0, 0, 0)) . parseDateTime
{-# INLINE toYMDhms #-}

-- ------------------------------------------------------------

parseMaybe' :: SP a -> Text -> Maybe a
parseMaybe' p = parseMaybe (p <* anyString) . (isoText #)

dateParser :: SP (Int, Int, Int)
dateParser = do
  y <- count 4 digitChar
  m <- oneOf' del *> count 2 digitChar <|> return "00"
  d <- oneOf' del *> count 2 digitChar <|> return "00"
  let (y', m', d') = (read y, read m, read d) :: (Int, Int, Int)
  if y' >= 1800
    && y' < 3001
    && m' >= 0
    && m' <= 12 -- month not given: 0
    && d' >= 0
    && d' <= 31 -- day not given: 0
    then return (y', m', d')
    else mzero
  where
    del = "-:"

timeParser :: SP (Tuple4 Int)
timeParser = do
  h <- count 2 digitChar
  m <- char ':' *> count 2 digitChar <|> return "00"
  s <- char ':' *> count 2 digitChar <|> return "00"
  ms <-
    (take 3 . (++ "000"))
    <$>
    (SP.option "0" $ char '.' *> some digitChar)
  let (h', m', s', ms') = (read h, read m, read s, read ms) :: Tuple4 Int
  if h' >= 0
    && h' <= 24
    && m' >= 0
    && m' < 60
    && s' >= 0
    && s' < 60
    then return (h', m', s', ms')
    else mzero

dateTimeParser :: SP (Tuple3 Int, Tuple4 Int)
dateTimeParser =
  (,)
    <$> dateParser
    <* (char 'T' <|> some spaceChar *> return ' ')
    <*> timeParser

-- ------------------------------------------------------------

ymd2Text :: Tuple3 Int -> Tuple3 Text
ymd2Text (y, m, d) =
   ( printf "%04d" y
   , printf "%02d" m
   , printf "%02d" d
   ) & each %~ T.pack

hms2Text :: Tuple4 Int -> Tuple4 Text
hms2Text (h, m, s, ms) =
  ( printf "%02d" h
  , printf "%02d" m
  , printf "%02d" s
  , printf "%03d" ms
  ) & each %~ T.pack


fmtDate' :: Tuple3 Int -> Words
fmtDate' x@(_y, m, d)
  | m == 0    = [y']
  | d == 0    = [y', m']
  | otherwise = [y', m', d']
  where
    (y', m', d') = ymd2Text x

fmtY' :: Int -> Words
fmtY' i
  | i > 0     = [i ^. isoText]
  | otherwise = []

fmtM' :: Int -> Words
fmtM' i
  | 1 <= i && i <= 12 = [ms !! (i - 1)]
  | otherwise         = []
  where
    ms = [ "Januar"
         , "Februar"
         , "März"
         , "April"
         , "Mai"
         , "Juni"
         , "Juli"
         , "Augut"
         , "September"
         , "Oktober"
         , "November"
         , "Dezember"
         ]

fmtD' :: Int -> Words
fmtD' i
  | 1 <= i && i <= 31 = [(show i <> ".") ^. isoText]
  | otherwise         = []

fmtYMD' :: Tuple3 Int -> Words
fmtYMD' x = [fmtYMD x]

fmtYMDRange' :: Tuple3 Int -> Tuple3 Int -> Words
fmtYMDRange' lb@(y1, m1, d1) ub@(y2, m2, d2)
  | d1 /= d2         -- one day is missing: ignore days
    &&
    d1 `min` d2 == 0 = fmtYMDRange' (y1, m1, 0) (y2, m2, 0)

  | lb >  ub  = []
  | lb == ub  =                                   fmtYMD' ub
  | y1 == y2
    &&
    m1 == m2  = fmtD' d1             <> ["bis"] <> fmtYMD' ub
  | y1 == y2  = fmtD' d1 <> fmtM' m1 <> ["bis"] <> fmtYMD' ub
  | otherwise -- lb <  ub
              = fmtYMD' lb           <> ["bis"] <> fmtYMD' ub

words2text :: Words -> Text
words2text = T.intercalate " "

-- --------------------

fmtDate :: Tuple3 Int -> Text
fmtDate = T.intercalate "-" . fmtDate'

fmtYMD :: Tuple3 Int -> Text
fmtYMD (y, m, d) =
  words2text $ fmtD' d <> fmtM' m <> fmtY' y

fmtYMDRange :: Tuple3 Int -> Tuple3 Int -> Text
fmtYMDRange d1 d2 = words2text $ fmtYMDRange' d1 d2

fmtKWTitle :: Tuple3 Int -> Tuple3 Int -> Text -> Text
fmtKWTitle lb ub kw = words2text $ [kw <> ":"] <> fmtYMDRange' lb ub

fmtKWSubTitle :: (Int, Int, Int) -> Text
fmtKWSubTitle (subCnt, imgCnt, colCnt) =
  T.intercalate ", " $ map snd $ filter ((/= 0) . fst) sts
  where
    sts = zip [subCnt, colCnt, imgCnt] [st0, st1, st2]

    st0 = subCnt ^. isoText <> " Schlüssel" <> (if subCnt == 1 then "wort" else "wörter")
    st2 = imgCnt ^. isoText <> " Bild" <> (if imgCnt == 1 then "" else "er")
    st1 = colCnt ^. isoText <> " Sammlung" <> (if colCnt == 1 then "" else "en")

-- ------------------------------------------------------------

isoDateInt :: Iso' (Tuple3 Int) Int
isoDateInt = iso to3 fr3
  where
    to3 (y, m, d) = (y * 100 + m) * 100 + d

    fr3 i = (y, m, d)
      where
        (my, d) = i `divMod` 100
        (y, m) = my `divMod` 100

-- ------------------------------------------------------------
