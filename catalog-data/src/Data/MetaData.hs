{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Data.MetaData
  ( MetaKey
  , MetaValue
  , MetaTable
  , MetaTableText

  , someKeysMD
  , globKeysMD
  , allKeysMD

  , MetaData
  , metaDataAt
  , partMetaData
  , selectMetaData
  , selectByNames
  , selectByParser
  , lookupByNames
  , prettyMD

  , clearAccess
  , addNoDeleteAccess
  , addNoSortAccess
  , addNoWriteAccess
  , subNoDeleteAccess
  , subNoSortAccess
  , subNoWriteAccess
  , getAccess
  , isWriteable
  , isSortable
  , isRemovable

  , getCreateMeta
  , getFileName
  , getGPSposDeg
  , getImageGeoOri
  , getImageGeo
  , getOrientation

  , Rating
  , mkRating
  , getRating
  , isoRating
  , isoStars

  , getEXIFUpdateTime
  , setEXIFUpdateTime

  , compareByName
  , compareByCreateDate

  , filterMetaData

  , parseTime
  , parseDate
  , isoDateInt

  , AttrGroup
  , allAttrGroups
  , allAttrKeys

    -- metadata keys
  , fileDirectory
  , fileFileSize
  , fileFileModifyDate
  , fileFileName
  , fileMIMEType
  , fileRefRaw
  , fileRefImg
  , fileRefJpg

  , exifArtist
  , exifBitsPerSample
  , exifCopyright
  , exifCreateDate
  , exifExposureCompensation
  , exifExposureMode
  , exifExposureProgram
  , exifExposureTime
  , exifFlash
  , exifFNumber
  , exifFocalLength
  , exifFocalLengthIn35mmFormat
  , exifGPSVersionID
  , exifImageHeight
  , exifImageWidth
  , exifISO
  , exifMake
  , exifMaxApertureValue
  , exifMeteringMode
  , exifModel
  , exifOrientation
  , exifUserComment
  , exifWhiteBalance

  , makerNotesColorSpace
  , makerNotesDaylightSavings
  , makerNotesFocusDistance
  , makerNotesFocusMode
  , makerNotesQuality
  , makerNotesSerialNumber
  , makerNotesShootingMode
  , makerNotesShutterCount
  , makerNotesTimeZone

  , compositeAperture
  , compositeAutoFocus
  , compositeCircleOfConfusion
  , compositeDOF
  , compositeFlash
  , compositeFocalLength35efl
  , compositeFOV
  , compositeGPSAltitude
  , compositeGPSLatitude
  , compositeGPSLongitude
  , compositeGPSPosition
  , compositeHyperfocalDistance
  , compositeImageSize
  , compositeLensID
  , compositeLensSpec
  , compositeLightValue
  , compositeMegapixels
  , compositeShutterSpeed
  , compositeSubSecCreateDate
  , compositeSubSecDateTimeOriginal

  , quickTimeDuration
  , quickTimeImageWidth
  , quickTimeImageHeight
  , quickTimeVideoFrameRate

  , xmpGPSLatitude
  , xmpGPSLongitude
  , xmpGPSAltitude
  , xmpFormat
  , xmpRawFileName
  , xmpRating

  , descrTitle
  , descrSubtitle
  , descrTitleEnglish
  , descrTitleLatin
  , descrLocation
  , descrKeywords
  , descrWeb
  , descrWikipedia
  , descrGoogleMaps
  , descrComment
  , descrCreateDate
  , descrOrderedBy
  , descrAccess
  , descrDuration
  , descrRating
  , descrGPSPosition
  , descrCatalogVersion
  , descrCatalogWrite

  , imgRating
  , imgEXIFUpdate
  )
where

import           Data.Prim
import           Data.Bits           (bit, (.|.), testBit, setBit, clearBit)
import           Data.Maybe          (mapMaybe)
import qualified Data.Aeson          as J
import           Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict  as IM
import qualified Data.List           as L
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Data.Scientific     as SC
import qualified Text.SimpleParser   as SP
import           Text.SimpleParser
import           Text.Printf         ( printf )
-- import Debug.Trace

-- ----------------------------------------

newtype MetaData = MD J.Object

-- ----------------------------------------

deriving instance Eq   MetaData
deriving instance Show MetaData

instance Semigroup MetaData where
  (<>) = mergeMD

instance Monoid MetaData where
  mempty  = MD HM.empty
  mappend = (<>)

instance IsEmpty MetaData where
  isempty (MD md) = HM.null md
  {-# INLINE isempty #-}

instance ToJSON MetaData where
  toJSON (MD m) = J.toJSON [m]
  {-# INLINE toJSON #-}

instance FromJSON MetaData where
  parseJSON = J.withArray "MetaData" $ \ v ->
    case V.length v of
      1 -> J.withObject "MetaData" (return . shareAttrKeys . MD) (V.head v)
      _ -> mzero


-- share keys of MD maps deserialized in FromJSON instance
-- unknown keys are removed
--
-- this is a space optimization, in all MD maps the keys are
-- all build with the key values defined in this module
-- removing shareAttrKeys has no effect on the semantics

shareAttrKeys :: MetaData -> MetaData
shareAttrKeys (MD m) = MD $ HM.foldlWithKey' ins HM.empty m
  where
    ins m' k v
      | Just k' <- HM.lookup k allAttrKeys = HM.insert k' v m'
      | otherwise                          =                m'


foldWithKeyMD :: (Name -> Text -> a -> a) -> a -> MetaData -> a
foldWithKeyMD f acc m@(MD hm) =
  foldl' f' acc keys
  where
    keys :: [Name]
    keys = map (isoText #) (HM.keys hm)

    f' acc' n' = f n' (m ^. metaDataAt n') acc'

-- merging of meta data
-- default: entries of m1 overwrite entries of m2
-- if an attr value in m1 is "-", the attribute is deleted in the result
-- if an attr value in m1 is "", the m2 value is taken
-- in keywords the "," separated list of keywords is computed
-- and the union of the sets of keywords is taken,
-- but all keywords in m1 starting with a "-" are removed

mergeMD :: MetaData -> MetaData -> MetaData
mergeMD m1 m2 =
  foldWithKeyMD mergeAttr m2 m1
  where
    mergeAttr :: Name -> Text -> MetaData -> MetaData
    mergeAttr n v acc
      | v == "-"  = acc & metaDataAt n .~ ""   -- remove attr
      | T.null v  = acc                        -- attribute not changed
      | n == descrKeywords
                  = acc & metaDataAt n %~ mergeKeywords v
      | n == descrRating
                  = acc & metaDataAt n .~ (v ^. isoString . isoRating
                                           .
                                           to (\ r -> if r == 0
                                                      then ""
                                                      else show r
                                              )
                                           .
                                           isoText
                                          )
      | n == descrGPSPosition
                  = let gps = v ^. isoString . isoGoogleMapsDegree . isoText in
                      if T.null gps
                      then acc
                      else acc & metaDataAt n .~ gps

      | otherwise = acc & metaDataAt n .~ v

mergeKeywords :: Text -> Text -> Text
mergeKeywords t1 t2
  -- clear all keywords
  | ["-"] <- T.words t1 = ""

  -- split keywords at ','
  -- remove keywords starting with a '-'
  -- union other keywords
  -- intercalate keywords with ", "
  | otherwise =
      T.intercalate ", " newKWs
  where
    (rmv, ins) = partKWs $ splitKW t1
    oldKWs     =           splitKW t2
    newKWs     = sort $
                 (oldKWs L.\\ rmv) `L.union` nub ins

    -- split keywords
    splitKW =
      filter (not . T.null)       -- remove empty words
      .
      map (T.unwords . T.words)   -- normalize whitespace
      .
      T.split (== ',')            -- split at ","

    -- partition keywords in (to be removed, added)
    partKWs =
      first (map (T.drop 1))
      .
      partition ((== "-"). T.take 1)

-- ----------------------------------------
--
-- MetaData lenses

metaDataAt :: Name -> Lens' MetaData Text
metaDataAt key = md2obj . at (key ^. isoText) . val2text
  where
    md2obj :: Iso' MetaData J.Object
    md2obj = iso (\ (MD m) -> m) MD
    {-# INLINE md2obj #-}

    {-# INLINE val2text #-}
    val2text :: Iso' (Maybe J.Value) Text
    val2text = iso totext fromtext
      where
        totext (Just (J.String t)) = t
        totext (Just (J.Number n)) = isoString # showSc n
        totext _                   = mempty

        fromtext t
          | isempty t = Nothing
          | otherwise = Just (J.String t)

        showSc n =
          either showF showI $ SC.floatingOrInteger n
          where
            showF :: Double -> String
            showF _ = show n

            showI :: Integer -> String
            showI = show


partMetaData :: (Name -> Bool) -> Iso' MetaData (MetaData, MetaData)
partMetaData predicate = iso part (uncurry mappend)
  where
    part (MD m) = (MD *** MD) $ HM.foldrWithKey pf (HM.empty, HM.empty) m
      where
        pf k v (m1, m2)
          | predicate (isoText # k) =
              (HM.insert k v m1, m2)
          | otherwise =
              (m1, HM.insert k v m2)
{-# INLINE partMetaData #-}

selectMetaData :: (Name -> Bool) -> Lens' MetaData MetaData
selectMetaData p = partMetaData p . _1
{-# INLINE selectMetaData #-}

selectByParser :: SP String -> Lens' MetaData MetaData
selectByParser ps = selectMetaData p
  where
    p n = matchP ps (n ^. isoString)
{-# INLINE selectByParser #-}

selectByNames :: [Name] -> Lens' MetaData MetaData
selectByNames ns = selectMetaData (`elem` ns)
{-# INLINE selectByNames #-}

-- lookup a sequence of fields and take first value found
lookupByNames :: [Name] -> MetaData -> Text
lookupByNames ns md =
  head (vs ++ [T.empty])
  where
    vs = filter (not . isempty) $
         map (\ n -> md ^. metaDataAt n) ns

-- ----------------------------------------

{-
someKeysMD :: (Name -> Bool) -> [Name]
someKeysMD p = filter p allKeysMD

globKeysMD :: SP String -> [Name]
globKeysMD gp = someKeysMD p
  where
    p n = matchP gp (n ^. isoString)

allKeysMD :: [Name]
allKeysMD = map (isoText #) . sort . HM.keys $ allAttrKeys
-- -}

keysMD :: MetaData -> [Name]
keysMD (MD mp) = map (isoText #) . HM.keys $ mp

prettyMD :: MetaData -> [String]
prettyMD md = sort $ zipWith (<:>) keys' attrs
  where
    keys  = keysMD md
    keys' = fillRightList ' ' . map (^.isoString) $ keys
    attrs = map (\ k ->  md ^. metaDataAt k . isoString) $ keys

    xs <:> ys = xs ++ " : " ++ ys


-- ----------------------------------------
--
-- manipulate the access attributes in MetaData

modifyAccess :: (Text -> Text) -> MetaData -> MetaData
modifyAccess f md =
  md & metaDataAt descrAccess %~ f

setAccess :: [Text] -> MetaData -> MetaData
setAccess ts = modifyAccess (const $ T.unwords ts)

allowAccess :: [Text] -> MetaData -> MetaData
allowAccess ts = modifyAccess f
  where
    f= T.unwords . filter (`notElem` ts) . T.words

restrAccess :: [Text] -> MetaData -> MetaData
restrAccess ts = modifyAccess f
  where
    f = T.unwords . nub . (ts ++) . T.words

clearAccess
  , addNoWriteAccess
  , addNoSortAccess
  , addNoDeleteAccess
  , subNoWriteAccess
  , subNoSortAccess
  , subNoDeleteAccess :: MetaData -> MetaData

clearAccess       = setAccess []
addNoWriteAccess  = restrAccess [no'write]
addNoSortAccess   = restrAccess [no'sort]
addNoDeleteAccess = restrAccess [no'delete]
subNoWriteAccess  = allowAccess [no'write]
subNoSortAccess   = allowAccess [no'sort]
subNoDeleteAccess = allowAccess [no'delete]

getAccess :: ([Text] -> Bool) -> MetaData -> Bool
getAccess f md =
  md ^. metaDataAt descrAccess . to (f . T.words)

isWriteable
  , isSortable, isRemovable :: MetaData -> Bool
isWriteable = getAccess (no'write  `notElem`)
isSortable  = getAccess (no'sort   `notElem`)
isRemovable = getAccess (no'delete `notElem`)

-- ----------------------------------------

getCreateMeta :: (String -> res) -> MetaData -> res
getCreateMeta parse' md =
  parse' cd
  where
    cd = lookupByNames
      [ compositeSubSecCreateDate
      , exifCreateDate
      ] md
      ^. isoString

getFileName :: MetaData -> Maybe Text
getFileName md =
  md ^. metaDataAt fileFileName . isoMaybe
{-# INLINE getFileName #-}

getImageGeoOri :: MetaData -> (Geo, Int)
getImageGeoOri = getImageGeo &&& getOrientation

getImageGeo :: MetaData -> Geo
getImageGeo md =
  md ^. metaDataAt compositeImageSize . to toGeo
  where
    toGeo sz = fromMaybe geo'org (readGeo'' $ sz ^. isoString)

getOrientation :: MetaData -> Int
getOrientation md =
  md ^. metaDataAt exifOrientation . to toOri
  where
    toOri :: Text -> Int
    toOri t
      | t == "Rotate 90 CW"           = 1
      | "Rotate 180" `T.isPrefixOf` t = 2
      | t == "Rotate 90 CCW"          = 3
      | t == "Rotate 270 CW"          = 3
      | otherwise                     = 0

type Rating = Int -- 0 .. 5

ratingMax :: Rating
ratingMax = 5

getRating :: MetaData -> Rating
getRating md =
  lookupByNames
  [ descrRating     -- descr:Rating has priority over
  , xmpRating       -- XMP:Rating from LR
  ] md
  ^. isoString . isoRating

isoRating :: Iso' String Rating
isoRating = iso fromS show
  where
    fromS s = min ratingMax $ i1 `max` i2
      where
        i1 = max 0 . fromMaybe 0 . readMaybe $ s
        i2 = isoStars # s

isoStars :: Iso' Rating String
isoStars = iso (flip replicate '*')
               (min ratingMax . length . filter (== '*'))

mkRating :: Rating -> MetaData
mkRating r = mempty & metaDataAt descrRating .~ (r ^. isoString . isoText)

getEXIFUpdateTime :: MetaData -> TimeStamp
getEXIFUpdateTime md =
  isoText # (md ^. metaDataAt imgEXIFUpdate)

setEXIFUpdateTime :: TimeStamp -> MetaData -> MetaData
setEXIFUpdateTime ts md =
    md & metaDataAt imgEXIFUpdate .~ (ts ^. isoText)

getGPSposDeg :: MetaData -> Maybe GPSposDeg
getGPSposDeg md
  | isempty t = Nothing
  | otherwise = t ^? isoString . prismString
  where
    t = lookupByNames [descrGPSPosition, compositeGPSPosition] md

-- ----------------------------------------
--
-- compare function on meta data

compareByCreateDate :: MetaData -> MetaData -> Ordering
compareByCreateDate =
  compareBy [ compareJust' `on` getCreateMeta parseDateTime
            , compare      `on` getFileName
            ]
{-# INLINE compareByCreateDate #-}

compareByName :: MetaData -> MetaData -> Ordering
compareByName =
  compareBy [ compare `on` getFileName
            ]
{-# INLINE compareByName #-}

-- ----------------------------------------
--
-- filter meta data enries by image type

filterMetaData :: ImgType -> MetaData -> MetaData
filterMetaData ty md =
  md ^. selectByParser ps
  where
    ps | isShowablePartOrRaw ty = psRaw
       | isMeta              ty = psXmp
       | otherwise              = mzero

-- ----------------------------------------
--
-- meta data parsers

type YMD = (String, String, String)
type HMS = (String, String, String, String)
type YMD'HMS = (YMD, HMS)

parseDateTime :: String -> Maybe YMD'HMS
parseDateTime = parseMaybe dateTimeParser

-- take the day part from a date/time input
parseDate :: String -> Maybe (String, String, String)
parseDate = parseMaybe (fst <$> dateTimeParser)
{-# INLINE parseDate #-}

isoDateInt :: Iso' (String, String, String) Int
isoDateInt = iso toInt frInt
  where
    toInt (y, m, d) =
      (read y * 100 + read m) * 100 + read d

    frInt i = ( printf "%04d" y
              , printf "%02d" m
              , printf "%02d" d
              )
      where
        (my, d) = i  `divMod` 100
        (y,  m) = my `divMod` 100

-- take the time part of a full date/time input
parseTime :: String -> Maybe (String, String, String, String)
parseTime = parseMaybe (snd <$> dateTimeParser)
{-# INLINE parseTime #-}

timeParser :: SP HMS
timeParser = do
  h  <-             count 2 digitChar
  m  <- char ':' *> count 2 digitChar
  s  <- char ':' *> count 2 digitChar
  ms <- SP.option ".0" $
        char '.' *> some    digitChar
  let (h', m', s') = (read h, read m, read s) :: (Int, Int, Int)
  if h' >= 0 && h' <= 24
     &&
     m' >= 0 && m' <  60
     &&
     s' >= 0 && s' <  60
    then return (h, m, s, ms)
    else mzero

dateParser :: SP YMD
dateParser = do
  y <-               count 4 digitChar
  m <- oneOf' del *> count 2 digitChar
  d <- oneOf' del *> count 2 digitChar
  let (y', m', d') = (read y, read m, read d) :: (Int, Int, Int)
  if y' >= 1800 && y' < 3001
     &&
     m' >= 1    && m' <= 12
     &&
     d' >= 1    && d' <= 31
    then return (y, m, d)
    else mzero
  where
    del = "-:"

dateTimeParser :: SP YMD'HMS
dateTimeParser = do
  ymd <- dateParser
  hms <- some spaceChar *> timeParser <* anyString  -- maybe followed by time zone
  return (ymd, hms)

-- ----------------------------------------

type AttrGroup = (Text, [Text])

psRaw, psXmp :: SP String

psRaw = attrGroupsParser
  [ attrExif
  , attrComposite
  , attrMaker
  , attrFile
  , attrQuickTime
  ]

psXmp = attrGroupsParser
  [ attrComposite
  , attrXmp
  ]

attrGroupsParser :: [AttrGroup] -> SP String
attrGroupsParser =
  foldr1 (<|>) . map toP
  where
    toP :: AttrGroup -> SP String
    toP (px, attr) =
      try ( string (px ^. isoString)
            <++>
            string ":"
            <++>
            foldr1 (<|>) (map toA attr)
          )

    toA :: Text -> SP String
    toA a = try (string (a ^. isoString) <* eof)

-- ----------------------------------------

attrGroup2attrName :: AttrGroup -> [Name]
attrGroup2attrName (px, as) = map f as
  where
    f a = isoText # (px <> ":" <> a)

allAttrGroups :: [AttrGroup]
allAttrGroups =
  [ attrFile
  , attrExif
  , attrMaker
  , attrComposite
  , attrQuickTime
  , attrXmp
  , attrCol
  , attrImg
  ]

-- used for optimizing space for MetaData tables,
-- with a lookup all keys can share the same Text value
-- see FromJSON instance for MetaData

allAttrKeys :: HashMap Text Text
allAttrKeys = foldl' ins HM.empty $
  concat
  [ keysAttrFile
  , keysAttrExif
  , keysAttrMaker
  , keysAttrComposite
  , keysAttrQuickTime
  , keysAttrXmp
  , keysAttrCol
  , keysAttrImg
  ]
  where
    ins m k = HM.insert k' k' m
      where
        k' = k ^. isoText

attrFile :: AttrGroup
attrFile =
  ( "File"
  , [ "Directory"
    , "FileSize"
    , "FileModifyDate"
    , "FileName"
    , "MIMEType"
    , "RefRaw"
    , "RefImg"
    , "RefJpg"
    ]
  )

fileDirectory
  , fileFileSize
  , fileFileModifyDate
  , fileFileName
  , fileMIMEType
  , fileRefRaw
  , fileRefImg
  , fileRefJpg :: Name

keysAttrFile :: [Name]
keysAttrFile@
  [ fileDirectory
  , fileFileSize
  , fileFileModifyDate
  , fileFileName
  , fileMIMEType
  , fileRefRaw
  , fileRefImg
  , fileRefJpg
  ] = attrGroup2attrName attrFile


attrExif :: AttrGroup
attrExif =
  ( "EXIF"
  , [ "Artist"
    , "BitsPerSample"
    , "Copyright"
    , "CreateDate"
    , "ExposureCompensation"
    , "ExposureMode"
    , "ExposureProgram"
    , "ExposureTime"
    , "Flash"
    , "FNumber"
    , "FocalLength"
    , "FocalLengthIn35mmFormat"
    , "GPSVersionID"
    , "ImageHeight"
    , "ImageWidth"
    , "ISO"
    , "Make"
    , "MaxApertureValue"
    , "MeteringMode"
    , "Model"
    , "Orientation"
    , "UserComment"
    , "WhiteBalance"
    ]
  )

exifArtist
  , exifBitsPerSample
  , exifCopyright
  , exifCreateDate
  , exifExposureCompensation
  , exifExposureMode
  , exifExposureProgram
  , exifExposureTime
  , exifFlash
  , exifFNumber
  , exifFocalLength
  , exifFocalLengthIn35mmFormat
  , exifGPSVersionID
  , exifImageHeight
  , exifImageWidth
  , exifISO
  , exifMake
  , exifMaxApertureValue
  , exifMeteringMode
  , exifModel
  , exifOrientation
  , exifUserComment
  , exifWhiteBalance :: Name

keysAttrExif :: [Name]
keysAttrExif@
  [ exifArtist
  , exifBitsPerSample
  , exifCopyright
  , exifCreateDate
  , exifExposureCompensation
  , exifExposureMode
  , exifExposureProgram
  , exifExposureTime
  , exifFlash
  , exifFNumber
  , exifFocalLength
  , exifFocalLengthIn35mmFormat
  , exifGPSVersionID
  , exifImageHeight
  , exifImageWidth
  , exifISO
  , exifMake
  , exifMaxApertureValue
  , exifMeteringMode
  , exifModel
  , exifOrientation
  , exifUserComment
  , exifWhiteBalance
  ] = attrGroup2attrName attrExif


attrMaker :: AttrGroup
attrMaker =
  ( "MakerNotes"
  , [ "ColorSpace"
    , "DaylightSavings"
    , "FocusDistance"
    , "FocusMode"
    , "Quality"
    , "SerialNumber"
    , "ShootingMode"
    , "ShutterCount"
    , "TimeZone"
    ]
  )

makerNotesColorSpace
  , makerNotesDaylightSavings
  , makerNotesFocusDistance
  , makerNotesFocusMode
  , makerNotesQuality
  , makerNotesSerialNumber
  , makerNotesShootingMode
  , makerNotesShutterCount
  , makerNotesTimeZone :: Name

keysAttrMaker :: [Name]
keysAttrMaker@
  [ makerNotesColorSpace
  , makerNotesDaylightSavings
  , makerNotesFocusDistance
  , makerNotesFocusMode
  , makerNotesQuality
  , makerNotesSerialNumber
  , makerNotesShootingMode
  , makerNotesShutterCount
  , makerNotesTimeZone
  ] = attrGroup2attrName attrMaker


attrComposite :: AttrGroup
attrComposite =
  ( "Composite"
  , [ "Aperture"
    , "AutoFocus"
    , "CircleOfConfusion"
    , "DOF"
    , "Flash"
    , "FocalLength35efl"
    , "FOV"
    , "GPSAltitude"
    , "GPSLatitude"
    , "GPSLongitude"
    , "GPSPosition"
    , "HyperfocalDistance"
    , "ImageSize"
    , "LensID"
    , "LensSpec"
    , "LightValue"
    , "Megapixels"
    , "ShutterSpeed"
    , "SubSecCreateDate"
    , "SubSecDateTimeOriginal"
    ]
  )

compositeAperture
  , compositeAutoFocus
  , compositeCircleOfConfusion
  , compositeDOF
  , compositeFlash
  , compositeFocalLength35efl
  , compositeFOV
  , compositeGPSAltitude
  , compositeGPSLatitude
  , compositeGPSLongitude
  , compositeGPSPosition
  , compositeHyperfocalDistance
  , compositeImageSize
  , compositeLensID
  , compositeLensSpec
  , compositeLightValue
  , compositeMegapixels
  , compositeShutterSpeed
  , compositeSubSecCreateDate
  , compositeSubSecDateTimeOriginal :: Name

keysAttrComposite :: [Name]
keysAttrComposite@
  [ compositeAperture
  , compositeAutoFocus
  , compositeCircleOfConfusion
  , compositeDOF
  , compositeFlash
  , compositeFocalLength35efl
  , compositeFOV
  , compositeGPSAltitude
  , compositeGPSLatitude
  , compositeGPSLongitude
  , compositeGPSPosition
  , compositeHyperfocalDistance
  , compositeImageSize
  , compositeLensID
  , compositeLensSpec
  , compositeLightValue
  , compositeMegapixels
  , compositeShutterSpeed
  , compositeSubSecCreateDate
  , compositeSubSecDateTimeOriginal
  ] = attrGroup2attrName attrComposite


attrQuickTime :: AttrGroup
attrQuickTime =
  ("QuickTime"
  , [ "Duration"
    , "ImageWidth"
    , "ImageHeight"
    , "VideoFrameRate"
    ]
  )

quickTimeDuration
  , quickTimeImageWidth
  , quickTimeImageHeight
  , quickTimeVideoFrameRate :: Name

keysAttrQuickTime :: [Name]
keysAttrQuickTime@
  [ quickTimeDuration
  , quickTimeImageWidth
  , quickTimeImageHeight
  , quickTimeVideoFrameRate
  ] = attrGroup2attrName attrQuickTime

attrXmp :: AttrGroup
attrXmp =
  ("XMP"
  , [ "GPSLatitude"
    , "GPSLongitude"
    , "GPSAltitude"
    , "Format"
    , "RawFileName"
    , "Rating"
    ]
  )

xmpGPSLatitude
  , xmpGPSLongitude
  , xmpGPSAltitude
  , xmpFormat
  , xmpRawFileName
  , xmpRating :: Name

keysAttrXmp :: [Name]
keysAttrXmp@
  [ xmpGPSLatitude
  , xmpGPSLongitude
  , xmpGPSAltitude
  , xmpFormat
  , xmpRawFileName
  , xmpRating
  ] = attrGroup2attrName attrXmp


attrCol :: AttrGroup
attrCol =
  ( "descr"
  , [ "Title"
    , "Subtitle"
    , "TitleEnglish"
    , "TitleLatin"
    , "Location"
    , "Keywords"
    , "Web"
    , "Wikipedia"
    , "GoogleMaps"
    , "Comment"
    , "CreateDate"
    , "OrderedBy"
    , "Access"
    , "Duration"
    , "Rating"
    , "GPSPosition"
    , "CatalogVersion"
    , "CatalogWrite"
    ]
  )

descrTitle
  , descrSubtitle
  , descrTitleEnglish
  , descrTitleLatin
  , descrLocation
  , descrKeywords
  , descrWeb
  , descrWikipedia
  , descrGoogleMaps
  , descrComment
  , descrCreateDate
  , descrOrderedBy
  , descrAccess
  , descrDuration
  , descrRating
  , descrGPSPosition
  , descrCatalogVersion
  , descrCatalogWrite :: Name

keysAttrCol :: [Name]
keysAttrCol@
  [ descrTitle
  , descrSubtitle
  , descrTitleEnglish
  , descrTitleLatin
  , descrLocation
  , descrKeywords
  , descrWeb
  , descrWikipedia
  , descrGoogleMaps
  , descrComment
  , descrCreateDate
  , descrOrderedBy
  , descrAccess
  , descrDuration
  , descrRating
  , descrGPSPosition
  , descrCatalogVersion
  , descrCatalogWrite
  ] = attrGroup2attrName attrCol


attrImg :: AttrGroup
attrImg =
  ( "Img"
  , [ "Rating"
    , "EXIFUpdate"
    ]
  )

imgRating
  , imgEXIFUpdate :: Name

keysAttrImg :: [Name]
keysAttrImg@
  [ imgRating
  , imgEXIFUpdate
  ] = attrGroup2attrName attrImg

-- ----------------------------------------

-- lookup a sequence of fields and take first value found
lookupByKeys :: [MetaKey] -> MetaTable -> MetaValue
lookupByKeys ns mt =
  foldr f mempty $ map (flip lookupMT mt) ns
  where
    f mv r
      | isempty mv = r
      | otherwise  = mv

-- ----------------------------------------

newtype MetaTable  = MT (IM.IntMap MetaValue)

type MetaTableText = HM.HashMap Text Text

data MetaKey
  = Composite'Aperture
  | Composite'AutoFocus
  | Composite'CircleOfConfusion
  | Composite'DOF
  | Composite'FOV
  | Composite'Flash
  | Composite'FocalLength35efl
  | Composite'GPSAltitude
  | Composite'GPSLatitude
  | Composite'GPSLongitude
  | Composite'GPSPosition
  | Composite'HyperfocalDistance
  | Composite'ImageSize
  | Composite'LensID
  | Composite'LensSpec
  | Composite'LightValue
  | Composite'Megapixels
  | Composite'ShutterSpeed
  | Composite'SubSecCreateDate
  | Composite'SubSecDateTimeOriginal
  | Descr'Access
  | Descr'CatalogVersion
  | Descr'CatalogWrite
  | Descr'Comment
  | Descr'CreateDate
  | Descr'Duration
  | Descr'GPSPosition
  | Descr'GoogleMaps
  | Descr'Keywords
  | Descr'Location
  | Descr'OrderedBy
  | Descr'Rating
  | Descr'Subtitle
  | Descr'Title
  | Descr'TitleEnglish
  | Descr'TitleLatin
  | Descr'Web
  | Descr'Wikipedia
  | EXIF'Artist
  | EXIF'BitsPerSample
  | EXIF'Copyright
  | EXIF'CreateDate
  | EXIF'ExposureCompensation
  | EXIF'ExposureMode
  | EXIF'ExposureProgram
  | EXIF'ExposureTime
  | EXIF'FNumber
  | EXIF'Flash
  | EXIF'FocalLength
  | EXIF'FocalLengthIn35mmFormat
  | EXIF'GPSVersionID
  | EXIF'ISO
  | EXIF'ImageHeight
  | EXIF'ImageWidth
  | EXIF'Make
  | EXIF'MaxApertureValue
  | EXIF'MeteringMode
  | EXIF'Model
  | EXIF'Orientation
  | EXIF'UserComment
  | EXIF'WhiteBalance
  | File'Directory
  | File'FileModifyDate
  | File'FileName
  | File'FileSize
  | File'MIMEType
  | File'RefImg
  | File'RefJpg
  | File'RefRaw
  | Img'EXIFUpdate
  | Img'Rating
  | MakerNotes'ColorSpace
  | MakerNotes'DaylightSavings
  | MakerNotes'FocusDistance
  | MakerNotes'FocusMode
  | MakerNotes'Quality
  | MakerNotes'SerialNumber
  | MakerNotes'ShootingMode
  | MakerNotes'ShutterCount
  | MakerNotes'TimeZone
  | QuickTime'Duration
  | QuickTime'ImageHeight
  | QuickTime'ImageWidth
  | QuickTime'VideoFrameRate
  | XMP'Format
  | XMP'GPSAltitude
  | XMP'GPSLatitude
  | XMP'GPSLongitude
  | XMP'Rating
  | XMP'RawFileName
  | Key'Unknown          -- must be the last value


data MetaValue
  = MText Text
  | MInt  Int
  | MRat  Int            -- rating: 0..5
  | MOri  Int            -- orientation: 0..3 <-> 0, 90, 180, 270 degrees CW
  | MKeyw [Text]         -- keywords
  | MAcc  Access         -- access restrictions
  | MNull

data AccessRestr = NO'write | NO'delete | NO'sort

type Access = Int

-- --------------------

deriving instance Show    AccessRestr
deriving instance Eq      AccessRestr
deriving instance Ord     AccessRestr
deriving instance Enum    AccessRestr
deriving instance Bounded AccessRestr

accessNames :: [Text]
accessNames@
  [no_write, no_sort, no_delete] = map fst accessMap

accessMap :: [(Text, AccessRestr)]
accessMap =
  map (\ a -> (toT a, a)) [minBound .. maxBound]
  where
    toT :: AccessRestr -> Text
    toT = T.pack . map f . show
      where
        f '\'' = '-'
        f c    = toLower c

no''restr
  , no''change
  , no''delete, no''sort, no''write
  , no''wrtdel, no''wrtsrt :: Access

[no''write, no''sort, no''delete] = map toA [minBound .. maxBound]
  where
    toA :: AccessRestr -> Access
    toA = bit . fromEnum

no''restr = 0
no''change = no''delete .|. no''sort .|. no''write
no''wrtdel = no''delete .|.              no''write
no''wrtsrt =                no''sort .|. no''write


-- read or set an access restriction

accessRestr :: AccessRestr -> Lens' Access Bool
accessRestr r k a =
  (\ b -> ( if b
            then setBit
            else clearBit
          ) a (fromEnum r)
  ) <$>
  k (testBit a (fromEnum r))

-- --------------------
--
-- instances for MetaTable

instance IsEmpty MetaTable where
  isempty (MT m) = IM.null m

instance Semigroup MetaTable where
  (<>) = unionMT

instance Monoid MetaTable where
  mempty = MT IM.empty

instance ToJSON MetaTable where
  toJSON m = J.toJSON [m ^. isoMTT]

instance FromJSON MetaTable where
  parseJSON = J.withArray "MetaData" $ \ v ->
    case V.length v of
      1 -> parseTable (V.head v)
      _ -> mzero
    where
      parseTable o = (isoMTT #) <$> parseJSON o

-- lens combining insertMT and lookupMT
metaTableAt :: MetaKey -> Lens' MetaTable MetaValue
metaTableAt mk k mt = (\ v -> insertMT mk v mt) <$> k (lookupMT mk mt)

insertMT :: MetaKey -> MetaValue -> MetaTable -> MetaTable
insertMT k v mt@(MT m)
  | isempty k   = mt                               -- no redundant stuff in metatable
  | isempty v   = MT $ IM.delete (fromEnum k)   m  -- dto
  | otherwise   = MT $ IM.insert (fromEnum k) v m

lookupMT :: MetaKey -> MetaTable -> MetaValue
lookupMT k (MT m) = fromMaybe mempty $ IM.lookup (fromEnum k) m

-- <> for meta tables
unionMT :: MetaTable -> MetaTable -> MetaTable
unionMT m1 m2 = foldWithKeyMT mergeMV m2 m1   -- fold over m1
  where
    mergeMV k1 v1 acc = insertMT k1 (v1 <> v2) acc
      where
        v2 = lookupMT k1 m2

foldWithKeyMT :: (MetaKey -> MetaValue -> a -> a) -> a -> MetaTable -> a
foldWithKeyMT f acc (MT m) =
  IM.foldlWithKey' f' acc m
  where
    f' acc' k' mv' = f (toEnum k') mv' acc'

-- --------------------

isoMTT :: Iso' MetaTable MetaTableText
isoMTT = iso mt2tt (flip editMT mempty)

mt2tt :: MetaTable -> MetaTableText
mt2tt (MT m) = IM.foldlWithKey' ins HM.empty m
  where
    ins acc i mv =
      HM.insert (metaKeyTextLookup $ toEnum i) (metaValueToText mv) acc


editMT :: MetaTableText -> MetaTable -> MetaTable
editMT m mt = HM.foldlWithKey' ins mt m
  where
    ins acc k0 v0
      | otherwise = insertMT k v acc

      where
        k = metaKeyLookup k0
        v | v0 == "-" = mempty        -- "-" is used for removing entry
          | k  == Descr'Keywords
                      = metaValueFromText k v0 &
                        metaKeywords %~ flip mergeKW mempty
          | otherwise = metaValueFromText k v0

-- --------------------
--
-- instances and basic ops for MetaValue

deriving instance Show MetaValue

instance IsEmpty MetaValue where   -- default values are redundant
  isempty (MText "") = True
  isempty (MOri 0)   = True
  isempty (MRat 0)   = True
  isempty (MKeyw []) = True
  isempty  MNull     = True
  isempty _          = False

instance Semigroup MetaValue where
  MNull          <> mv2      = mv2
  mv1            <> MNull    = mv1

  mv1@(MText _)  <> MText _  = mv1     -- 1. wins

  mv1@(MInt _)   <> MInt _   = mv1     -- 1. wins
  mv1@(MRat _)   <> MRat _   = mv1     -- 1. wins
  mv1@(MOri _)   <> MOri _   = mv1     -- 1. wins
  mv1@(MAcc _)   <> MAcc _   = mv1     -- 1. wins
  MKeyw w1       <> MKeyw w2 = MKeyw $ unionKW w1 w2

  _              <> mv2      = mv2     -- mixing types, no effect

instance Monoid MetaValue where
  mempty = MNull

-- MetaKey determines the MetaValue representation
-- no instance of FromJSON, due to decoding dependency on the key

metaKeywords :: Iso' MetaValue [Text]
metaKeywords = iso
  (\ x -> case x of
            MKeyw kw -> kw
            _        -> mempty
  )
  MKeyw

metaKeywordsText :: Iso' MetaValue Text
metaKeywordsText = metaKeywords . isoKeywText

metaRating :: Iso' MetaValue Int
metaRating = iso
  (\ x -> case x of
            MRat i -> i
            _      -> 0
  )
  (\ i -> MRat $ (i `max` 0) `min` 5)

metaRatingText :: Iso' MetaValue Text
metaRatingText = metaRating . isoIntStr . isoText

metaOri :: Iso' MetaValue Int
metaOri = iso
  (\ x -> case x of
            MOri i -> i
            _      -> 0
  )
  (\ i -> MOri $ (i `max` 0) `min` 3)

metaOriText :: Iso' MetaValue Text
metaOriText = metaOri . isoOriText

metaAccess :: Iso' MetaValue [AccessRestr]
metaAccess = iso
  (\ x -> case x of
            MAcc a -> toS a
            _      -> mempty
  )
  (\ rs -> MAcc $ foldl' sb no''restr rs)
  where
    sb :: Access -> AccessRestr -> Access
    sb a r = a & accessRestr r .~ True

    toS :: Access -> [AccessRestr]
    toS a = foldr add [] [minBound .. maxBound]
      where
        add r acc
          | a ^. accessRestr r = r : acc
          | otherwise          =     acc

metaAccessText :: Iso' MetaValue Text
metaAccessText = metaAccess . isoAccText

isoKeywText :: Iso' [Text] Text
isoKeywText = iso toT frT
  where
    toT = T.intercalate ","
    frT =
      filter (not . T.null)       -- remove empty words
      .
      map (T.unwords . T.words)   -- normalize whitespace
      .
      T.split (== ',')            -- split at ","

isoOriText :: Iso' Int Text
isoOriText = iso toT frT
  where
    toT o = case o of
      1 -> d90
      2 -> d180
      3 -> d270
      _ -> d0

    frT t
      | d90  == t              = 1
      | d180 `T.isPrefixOf` t  = 2
      | d270 == t || d271 == t = 3
      | otherwise              = 0

    d0   = "Horizontal"
    d90  = "Rotate 90 CW"
    d180 = "Rotate 180" -- `T.isPrefixOf` t = 2
    d270 = "Rotate 90 CCW"
    d271 = "Rotate 270 CW"

isoAccText :: Iso' [AccessRestr] Text
isoAccText = iso toT frT
  where
    toT :: [AccessRestr] -> Text
    toT = T.unwords . map (\ r -> accessNames !! fromEnum r)

    frT :: Text -> [AccessRestr]
    frT = mapMaybe (flip lookup accessMap) . T.words

isoIntStr :: Iso' Int String
isoIntStr = iso show (fromMaybe 0 . readMaybe)

isoIntText :: Iso' Int Text
isoIntText = isoIntStr . isoText

-- --------------------
--
-- parse meta values

metaValueFromText :: MetaKey -> Text -> MetaValue
metaValueFromText mk t = case mk of
  Descr'Access     -> metaAccessText   # t
  Descr'Keywords   -> metaKeywordsText # t
  Descr'Rating     -> metaRatingText   # t
  EXIF'Orientation -> metaOriText      # t
  Img'Rating       -> metaRatingText   # t
  XMP'Rating       -> metaRatingText   # t
  Key'Unknown      -> MNull
  _ | isempty t    -> MNull
    | otherwise    -> MText t

-- --------------------
--
-- convert to text for JSON

metaValueToText :: MetaValue -> Text
metaValueToText mv = case mv of
  MText t -> t
  MInt  i -> show i ^. isoText
  MRat  r -> show r ^. isoText
  MOri  o -> show o ^. isoText
  MAcc  _ -> mv ^. metaAccessText
  MNull   -> mempty

unionKW :: [Text] -> [Text] -> [Text]
unionKW ws1 ws2 = nub (ws1 ++ ws2)

mergeKW :: [Text] -> [Text] -> [Text]
mergeKW ws1 ws2 = ws
  where
    ws         = nub ins L.\\ rmv
    (rmv, ins) = partKWs $ ws1 ++ ws2

    -- partition keywords in (to be removed, added)
    partKWs =
      first (map (T.drop 1))
      .
      partition ((== "-"). T.take 1)

-- --------------------
--
-- instances and basic ops for MetaKey

deriving instance Bounded MetaKey
deriving instance Enum    MetaKey
deriving instance Eq      MetaKey
deriving instance Ord     MetaKey
deriving instance Show    MetaKey

instance IsoText MetaKey where
  isoText = iso metaKeyTextLookup metaKeyLookup
  {-# INLINE isoText #-}

instance IsEmpty MetaKey where
  isempty Key'Unknown = True
  isempty _           = False

-- instance ToJSON MetaKey where
--   toJSON = J.toJSON . metaKeyTextLookup

-- instance FromJSON MetaKey where
--   parseJSON v = metaKeyLookup <$> parseJSON v

metaKeyLookup :: Text -> MetaKey
metaKeyLookup t =
  fromMaybe Key'Unknown $
  HM.lookup (T.toLower t) metaKeyLookupTable
{-# INLINE metaKeyLookup #-}

metaKeyTextLookup :: MetaKey -> Text
metaKeyTextLookup k =
  fromMaybe mempty $ IM.lookup (fromEnum k) metaKeyToTextTable

-- --------------------

allKeysMD :: [Name]
allKeysMD = map (isoText #) $ IM.elems metaKeyToTextTable

someKeysMD :: (Name -> Bool) -> [Name]
someKeysMD p = filter p allKeysMD

globKeysMD :: SP String -> [Name]
globKeysMD gp = someKeysMD p
  where
    p n = matchP gp (n ^. isoString)

-- --------------------
--
-- mother's little helpers

metaKeyToText :: MetaKey -> Text
metaKeyToText Key'Unknown = ""
metaKeyToText k           = T.pack . map toColon . show $ k
  where
    toColon '\'' = ':'
    toColon c    = c
{-# INLINE metaKeyToText #-}

allMetaKeys :: [Text]
allMetaKeys = map metaKeyToText [minBound :: MetaKey .. maxBound]

type MetaKeyLookupTable = HM.HashMap Text MetaKey

metaKeyLookupTable :: MetaKeyLookupTable
metaKeyLookupTable =
  HM.fromList $
  zip (map T.toLower allMetaKeys)
      [minBound .. maxBound]

type MetaKeyToTextTable = IM.IntMap Text

metaKeyToTextTable :: MetaKeyToTextTable
metaKeyToTextTable =
  IM.fromList $
  map (\ k -> (fromEnum k, metaKeyToText k)) [minBound .. pred maxBound]


-- ----------------------------------------
