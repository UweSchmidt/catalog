{-# LANGUAGE InstanceSigs #-}
module Data.Prim.GPS
  ( GPSposDeg
  , GPSposDec
  , GPSdeg
  , gpsLat
  , gpsLong
  , isoDegDec
  , isoGoogleMapsDegree
  , googleMapsGPSdec

  , GeoLocAddrList
  , GeoAddrList
  , GeoAddress(..)
  , GeoAddress1
  )
where

import Data.Prim.Prelude
       ( Alternative((<|>), empty, some)
       , MonadPlus(mzero)
       , Text
       , Iso'
       , Lens'
       , Prism'
       , IsoMaybe(isoMaybe)
       , ParsePrintString(..)
       , FromJSON(parseJSON)
       , ToJSON(toJSON)
       , (>=>)
       , isDigit
       , (^?)
       , from
       , iso
       , prism'
       , (#)
       )
import Text.SimpleParser
    ( (<++>),
      anyStringThen,
      manyChars,
      anySingle,
      msp,
      noCaseWord,
      parseMaybe,
      satisfy,
      char,
      digitChar,
      string,
      SP,
      try,
      caseWord )

import Text.Printf
       ( printf )

import qualified Data.Aeson          as J
import qualified Data.Map            as M
import qualified Text.SimpleParser   as SP

-- ----------------------------------------

data GPSdir = N | E | S | W deriving (Eq, Show)

data GPSdeg = GPSdeg !Int !Int !Double !GPSdir deriving (Eq, Show)

data GPSpos' a =
  GPSpos { _gpsLat  :: !a
         , _gpsLong :: !a
         }
  deriving (Eq, Ord, Show)

type GPSposDeg = GPSpos' GPSdeg  -- GPS position (lat, long) in deg, min, sec, dir
type GPSposDec = GPSpos' Double  -- GPS position (lat, long) in decimal degrees


gpsLat :: Lens' (GPSpos' a) a
gpsLat k gp = (\ new -> gp {_gpsLat = new}) <$> k (_gpsLat gp)

gpsLong :: Lens' (GPSpos' a) a
gpsLong k gp = (\ new -> gp {_gpsLong = new}) <$> k (_gpsLong gp)

-- conversion of degree from/to decimal degree
-- and parse/show can be implemeted by an iso and two prisms

-- conversion decimal <-> degree

isoDegDec :: Iso' GPSposDeg GPSposDec
isoDegDec = iso toDec frDec
  where
    toDec (GPSpos lat long) = GPSpos (deg2dec       lat) (deg2dec       long)
    frDec (GPSpos lat long) = GPSpos (dec2deg (N,S) lat) (dec2deg (E,W) long)


-- parse and show positions in degree format

instance ParsePrintString GPSposDeg where
  ppString :: Prism' String GPSposDeg
  ppString = prism' showPos
             (parseMaybe parserPos)
    where
      showPos :: GPSposDeg -> String
      showPos (GPSpos lat long) =
        showDeg lat
        ++ ", " ++
        showDeg long

      parserPos :: SP GPSposDeg
      parserPos = do
        lat  <- msp *> parserDeg [N, S] <* msp <* char ','
        long <- msp *> parserDeg [E, W] <* msp
        return $ GPSpos lat long

-- parse and show positions in decimal form
--
-- "53.3, 10.0" ^? ppString      -> Just (GPSpos 53.3 10.0)
-- ppString # (GPSpos 53.3 10.0) -> "53.3, 10.0"

instance ParsePrintString GPSposDec where
  ppString :: Prism' String GPSposDec
  ppString = prism' showPosDec
                       (parseMaybe parserPos)
    where
      showPosDec :: GPSposDec -> String
      showPosDec (GPSpos lat long) =
        printf "%.9f" lat
        ++ "," ++
        printf "%.9f" long

      parserPos :: SP GPSposDec
      parserPos = msp *> parserPosDec <* msp

-- the googleMaps parser accepts various input formats
-- as GPS position
--
-- .1a a google maps url with a part .../@53.0,10.0,17z...
-- .1b a google maps url with a part ...&ll=53.0%2C-90.0&z=20
-- .2 a pair of decimal lat/long coords: 53.0,10.0
-- .3 the GPS format in EXIF data
--
-- these formats are parsed and the result is maybe a pair of doubles

googleMapsGPSdec :: Prism' String GPSposDec
googleMapsGPSdec = prism' pos2mapsUrl
                         (\ s -> parseMaybe parserMapsUrl s
                                 <|>
                                 (s ^? ppString . isoDegDec)
                         )
  where
    -- generate a Google maps url
    pos2mapsUrl :: GPSposDec -> String
    pos2mapsUrl pos =
      "https://www.google.com/maps/search/"
      <> "?api=1"
      <> "&query=" <> (ppString # pos)
      <> "&zoom=16"                       -- default: 15
      <> "&basemap=satellite"             -- or roadmap, terrain


    -- parse a Google maps url or just a GPSposDec (pair of doubles)
    parserMapsUrl :: SP GPSposDec
    parserMapsUrl =
      -- old maps format .1a
      try ( anyStringThen "/@" *> parserPosDec <*
            char ',' <* some digitChar <* manyChars anySingle
          )
      <|>
      -- new maps format .1b
      try ( anyStringThen "&ll=" *> parserPosDec <*
            char '&' <* manyChars anySingle
          )
      <|>
      -- search maps format .1c
      try ( anyStringThen "&query=" *> parserPosDec <*
            char '&' <* manyChars anySingle
          )
      <|>
      -- .2 lat/long pair or EXIF data
      try parserPosDec

-- a use case for the whole GPS stuff:
--
-- transform a Google Maps url into a string representing a GPS position
-- in EXIF format and vice versa

isoGoogleMapsDegree :: Iso' String String
isoGoogleMapsDegree =
  iso googleMapsUrl2degree degree2googleMapsUrl
  where
    googleMapsUrl2degree :: String -> String
    googleMapsUrl2degree url =
      isoMaybe # res
      where
        deg :: Maybe GPSposDeg
        deg = url ^? googleMapsGPSdec . from isoDegDec

        res :: Maybe String
        res = (ppString #) <$> deg

    degree2googleMapsUrl :: String -> String
    degree2googleMapsUrl deg =
      isoMaybe # res
      where
        dec :: Maybe GPSposDec
        dec = deg ^? ppString . isoDegDec

        res :: Maybe String
        res = (ppString #) <$> dec

instance ParsePrintString GPSdeg where
  ppString = prism' showDeg (parseMaybe $ parserDeg [N, E, S, W])

instance ToJSON GPSposDec where
  toJSON (GPSpos lo la) = toJSON [lo, la]

instance FromJSON GPSposDec where
  parseJSON = parseJSON >=> toGPS
    where
      toGPS [lo, la] = return $ GPSpos lo la
      toGPS _        = mzero

-- --------------------
--
-- helper funtions

parserPosDec :: SP GPSposDec
parserPosDec = tt <$> signedFloat <* del <*> signedFloat <* msp
  where
    tt x y = GPSpos (read x) (read y)

    del = (msp <* char ',')
          <|>
          (noCaseWord "%2C")   -- new maps format .1b

showDeg :: GPSdeg -> String
showDeg (GPSdeg d m s r) = unwords
  [ show d, "deg", show m ++ "'", fmtSec s ++ "\"", show r]
  where
    fmtSec :: Double -> String
    fmtSec = reverse . rem0 . reverse . printf "%.9f"

    rem0 ('0' : xs@(c2 : _))
      | isDigit c2          = rem0 xs
    rem0 xs                 = xs

parserDeg :: [GPSdir] -> SP GPSdeg
parserDeg dirs = do
  deg <- read <$> (some digitChar <* msp <* noCaseWord "deg" <* msp)
  mn  <- read <$> (some digitChar <* char '\''               <* msp)
  sec <- read <$> (floatParser    <* char '"'                <* msp)
  dir <- parserDir
  return $ GPSdeg deg mn sec dir
  where
    parserDir :: SP GPSdir
    parserDir = foldr ((<|>) . parserD) empty dirs
      where
        parserD :: GPSdir -> SP GPSdir
        parserD d = caseWord (show d) >> return d

deg2dec :: GPSdeg -> Double
deg2dec (GPSdeg d m s r) =
  (d' + m'/60 + s/3600) *
  ( if r == N || r == E
    then  1
    else -1
  )
  where
    d' = fromIntegral d
    m' = fromIntegral m

dec2deg :: (GPSdir, GPSdir) -> Double -> GPSdeg
dec2deg (po, ne) x = GPSdeg d'  m'  s'  c'
  where
    s'       = r2 * 60
    (m', r2) = properFraction (r1 * 60)
    (d', r1) = properFraction x'
    (x', c')
      | x >= 0    = ( x, po)
      | otherwise = (-x, ne)

signedFloat :: SP String
signedFloat =
  msp *>
  ( ( ((: []) <$> satisfy (\ x -> x == '+' || x == '-'))
      <|>
      return ""
    )
    <++>
    floatParser
  )

floatParser :: SP String
floatParser =
  ( some digitChar
    <++>
    SP.option ".0"
    ( string "."
      <++>
      SP.option "0" (some digitChar)
    )
  )
  <|>
  (("0." ++) <$> (char '.' *> some digitChar))

-- ----------------------------------------

type GeoLocAddrList = [(GPSposDec, GeoAddrList)]

type GeoAddrList = [GeoAddress]

data GeoAddress = GA
  { _display_name :: Text
  , _geo_address  :: GeoAddress1
  }
  deriving (Show)   -- just for testing

type GeoAddress1 = M.Map Text Text

-- --------------------

instance FromJSON GeoAddress where
  parseJSON = J.withObject "GeoAddress" $ \ o ->
    GA
    <$> o J..: "display_name"
    <*> o J..: "address"


instance ToJSON GeoAddress where
  toJSON (GA dn ga) = J.object
    [ "display_name" J..= dn
    , "address"      J..= ga
    ]

-- ----------------------------------------
--
-- Tests
--
-- parsing Google maps urls is critical, Google sometimes changes the format

{-

>>> let s1  = " 53.575644, -9.767767 "
>>> let s11 = "53.575644,-9.767767"
>>>
>>> (s1  ^? ppString) :: Maybe GPSposDec
>>> (s11 ^? ppString) :: Maybe GPSposDec
>>>
Just (GPSpos {_gpsLat = 53.575644, _gpsLong = -9.767767})
Just (GPSpos {_gpsLat = 53.575644, _gpsLong = -9.767767})

>>> let s2  = "https://www.google.com/maps/place/34%C2%B011'19.1%22N+118%C2%B040'26.5%22W/@34.1886344,-118.6762237,17z/data=!3m1!4b1!4m14!1m7!3m6!1s0x80c2c75ddc27da13:0xe22fdf6f254608f4!2sLos+Angeles,+CA,+USA!3b1!8m2!3d34.0522342!4d-118.2436849!3m5!1s0x0:0x0!7e2!8m2!3d34.1886303!4d-118.6740354"
>>> let s22 = "https://www.google.com/maps/place/40%C2%B045'58.8%22N+0%C2%B044'16.0%22E/@40.766342,0.7355993,1230m/data=!3m2!1e3!4b1!4m14!1m7!3m6!1s0x12a107cdc6829017:0x91989cc9d39014fd!2sEbro+Delta,+Spain!3b1!8m2!3d40.6934831!4d0.6962853!3m5!1s0x0:0x0!7e2!8m2!3d40.7663384!4d0.7377877"
>>>
>>> (s2  ^? googleMapsGPSdec) :: Maybe GPSposDec
>>> (s22 ^? googleMapsGPSdec) :: Maybe GPSposDec
>>>
Just (GPSpos {_gpsLat = 34.1886344, _gpsLong = -118.6762237})
Just (GPSpos {_gpsLat = 40.766342, _gpsLong = 0.7355993})

>>> let s3  = "53 deg 2' 10.3\" N , 10 deg 0' 20.1\" E"
>>>
>>>
>>> (s3  ^? ppString) :: Maybe GPSposDeg
>>>
Just (GPSpos {_gpsLat = GPSdeg 53 2 10.3 N, _gpsLong = GPSdeg 10 0 20.1 E})

-}
