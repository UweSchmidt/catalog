-- types for image geometry
{-# LANGUAGE InstanceSigs #-}

module Data.Prim.Geometry
where

import Data.Prim.Prelude
    ( Text,
      Alternative((<|>), many),
      fromMaybe,
      toLower,
      toUpper,
      (&),
      _head,
      (^.),
      iso,
      (#),
      (%~),
      AsEmpty,
      Field1(_1),
      Field2(_2),
      Iso',
      Lens',
      FromJSON(parseJSON),
      ToJSON(toJSON),
      isDigit,
      readMaybe,
      IsoText(..),
      IsoString(..) )

import Text.SimpleParser
    ( try, parseMaybe, satisfy, SP, digits, char, string )

-- ----------------------------------------

data Geo = Geo !Int !Int

deriving instance Show Geo
deriving instance Eq   Geo
deriving instance Ord  Geo

instance FromJSON Geo where
  parseJSON v = do
    Just geo <- readGeo' <$> parseJSON v
    return geo

instance ToJSON   Geo where
  toJSON geo = toJSON (geo ^. isoText)

instance IsoString Geo where
  isoString :: Iso' Geo String
  isoString = iso showGeo readGeo
  {-# INLINE isoString #-}

instance IsoText Geo where
  isoText :: Iso' Geo Text
  isoText = isoString . isoText
  {-# INLINE isoText #-}

instance Semigroup Geo where
  (<>) :: Geo -> Geo -> Geo
  Geo 0 0 <> geo2 = geo2
  geo1    <> _    = geo1
  {-# INLINE (<>) #-}

instance Monoid Geo where
  mempty :: Geo
  mempty  = Geo 0 0
  {-# INLINE mempty #-}

instance AsEmpty Geo

geo'org :: Geo
geo'org = Geo 1 1

orgGeo :: String
orgGeo = "org"

geo2pair :: Iso' Geo (Int, Int)
geo2pair = iso (\ (Geo w h) -> (w, h)) (uncurry Geo)

theW :: Lens' Geo Int
theW = geo2pair . _1

theH :: Lens' Geo Int
theH = geo2pair . _2

showGeo :: Geo -> String
showGeo (Geo w h) = show w ++ "x" ++ show h

readGeo :: String -> Geo
readGeo s =
  fromMaybe (error $ "parseGeo: no parse: " ++ s) $
  readGeo' s

readGeo' :: String -> Maybe Geo
readGeo' = parseMaybe geoParser

-- extract a geo from a string, e.g. "ImageSize : 900x600 ..."
readGeo'' :: String -> Maybe Geo
readGeo'' =
  parseMaybe $ junk *> geoParser <* junk
  where
  junk = many (satisfy (not . isDigit))

geoParser :: SP Geo
geoParser = try pg <|> pg'org
  where
    pg'org = string "org" >> return geo'org

    pg = do
      x <- read <$> digits
      _ <- char 'x'
      y <- read <$> digits
      return (Geo x y)

flipGeo :: Geo -> Geo
flipGeo (Geo w h) = Geo h w

-- ----------------------------------------

data AspectRatio = Fix | Pad | Crop | Flex
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

instance IsoString AspectRatio where
  isoString = iso toS frS
    where
      toS = (_head %~ toLower) . show
      frS = fromMaybe Pad . readMaybe . (& _head %~ toUpper)

instance IsoText AspectRatio

arParser :: SP AspectRatio
arParser =
  foldr1 (<|>) $ map toAP [minBound .. maxBound]
  where
    toAP :: AspectRatio -> SP AspectRatio
    toAP ar = try (string (ar ^. isoString) >> return ar)

-- ----------------------------------------

data GeoAR = GeoAR !Int !Int !AspectRatio

deriving instance Eq   GeoAR
deriving instance Ord  GeoAR
deriving instance Show GeoAR

instance IsoString GeoAR where
  isoString = iso toS frS
    where
      toS x = (x ^. geoar2pair . _2 . isoString) ++ "-" ++
              (x ^. geoar2pair . _1 . isoString)

      frS s
        | (ar, '-' : g) <- break (== '-') s = geoar2pair # (isoString # g, isoString # ar)
        | otherwise                         = GeoAR 1 1 Pad

instance IsoText GeoAR

instance FromJSON GeoAR where
  parseJSON v = do
    Just geo <- readGeoAR <$> parseJSON v
    return geo

instance ToJSON GeoAR where
  toJSON geo = toJSON (geo ^. isoText)


mkGeoAR :: Geo -> AspectRatio -> GeoAR
mkGeoAR (Geo w h) = GeoAR w h

geoar'org :: GeoAR
geoar'org = geoar2pair # (geo'org, Pad)

geoar2pair :: Iso' GeoAR (Geo, AspectRatio)
geoar2pair = iso (\ (GeoAR w h ar) -> (Geo w h, ar))
                 (\ (Geo w h, ar) -> GeoAR w h ar)

theGeo :: Lens' GeoAR Geo
theGeo = geoar2pair . _1

theAR :: Lens' GeoAR AspectRatio
theAR  = geoar2pair . _2

readGeoAR :: String -> Maybe GeoAR
readGeoAR = parseMaybe geoARParser

geoARParser :: SP GeoAR
geoARParser = do
  ar  <- arParser <* char '-'
  geo <- geoParser
  return $ geoar2pair # (geo, ar)

isoGeoAR :: Iso' String (Maybe GeoAR)
isoGeoAR = iso readGeoAR (maybe "" (^. isoString))

-- ----------------------------------------
