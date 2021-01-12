{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Data.MetaData
  ( MetaKey
  , MetaValue
  , MetaData
  , MetaDataT
  , MetaDataText(..)
  , Rating

  , module Data.Access
  , module Data.Bits

  , isoMetaDataMDT
  , metaDataAt
  , metaTextAt

  , metaAcc
  , metaCheckSum
  , metaImgType
  , metaName
  , metaTimeStamp

  , editMD
  , splitMDT
  , filterByImgType
  , filterKeysMD
  , splitMetaData
  , cleanupMetaData

  , someKeysMD
  , globKeysMD
  , allKeysMD
  , prettyMD

  , clearAccess
  , addNoWriteAccess
  , subNoWriteAccess

  , isWriteable
  , isSortable
  , isRemovable
  , isAUserCol

  , lookupByKeys
  , lookupCreate
  , lookupGeo
  , lookupGeoOri
  , lookupOri
  , lookupRating
  , lookupGPSposDeg
  , mkRating

  , parseTime
  , parseDate
  , parseDateTime
  , isoDateInt
  , isoStars

    -- metadata keys
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

  , fileCheckSum
  , fileDirectory
  , fileFileSize
  , fileFileModifyDate
  , fileImgType
  , fileName
  , fileRefImg
  , fileRefJpg
  , fileRefRaw
  , fileTimeStamp

  , imgRating
  , imgEXIFUpdate

  , makerNotesColorSpace
  , makerNotesDaylightSavings
  , makerNotesFocusDistance
  , makerNotesFocusMode
  , makerNotesQuality
  , makerNotesSerialNumber
  , makerNotesShootingMode
  , makerNotesShutterCount
  , makerNotesTimeZone

  , quickTimeDuration
  , quickTimeImageWidth
  , quickTimeImageHeight
  , quickTimeVideoFrameRate

  , xmpGPSLatitude
  , xmpGPSLongitude
  , xmpGPSAltitude
  , xmpRating
  )
where

import           Data.Prim
import           Data.Access         ( Access
                                     , AccessRestr(..)
                                     , all'restr
                                     , no'restr
                                     , accessRestr
                                     , no'delete
                                     , no'sort
                                     , no'user
                                     , no'write
                                     , isoAccessRestr
                                     , isoAccText
                                     )
import           Data.Bits           ( (.|.), (.&.), complement )

import qualified Data.Aeson          as J
import           Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict  as IM
import qualified Data.List           as L
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.Text.Fill      as T
import qualified Data.Vector         as V
import qualified Text.SimpleParser   as SP
import           Text.SimpleParser
import           Text.Printf         ( printf )
-- import Debug.Trace

-- ----------------------------------------
--
-- meta data parsers

type YMD = (String, String, String)
type HMS = (String, String, String, String)
type YMD'HMS = (YMD, HMS)

parseDateTime :: Text -> Maybe YMD'HMS
parseDateTime = parseMaybe dateTimeParser . (isoText #)

-- take the day part from a date/time input
parseDate :: Text -> Maybe (String, String, String)
parseDate = parseMaybe (fst <$> dateTimeParser) . (isoText #)
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
parseTime :: Text -> Maybe (String, String, String, String)
parseTime = parseMaybe (snd <$> dateTimeParser) . (isoText #)
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
--
-- meta keys

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
  , compositeSubSecDateTimeOriginal :: MetaKey

keysAttrComposite :: [MetaKey]
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
  ] = [Composite'Aperture .. Composite'SubSecDateTimeOriginal]

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
  , descrCatalogWrite :: MetaKey

keysAttrDescr :: [MetaKey]
keysAttrDescr@
  [ descrAccess
  , descrCatalogVersion
  , descrCatalogWrite
  , descrComment
  , descrCreateDate
  , descrDuration
  , descrGPSPosition
  , descrGoogleMaps
  , descrKeywords
  , descrLocation
  , descrOrderedBy
  , descrRating
  , descrSubtitle
  , descrTitle
  , descrTitleEnglish
  , descrTitleLatin
  , descrWeb
  , descrWikipedia
  ] = [Descr'Access .. Descr'Wikipedia]

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
  , exifWhiteBalance :: MetaKey

keysAttrExif :: [MetaKey]
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
  ] = [EXIF'Artist .. EXIF'WhiteBalance]

fileCheckSum
  , fileDirectory
  , fileFileSize
  , fileFileModifyDate
  , fileImgType
  , fileName
  , fileRefImg
  , fileRefJpg
  , fileRefRaw
  , fileTimeStamp :: MetaKey

keysAttrFile :: [MetaKey]
keysAttrFile@
  [ fileCheckSum
  , fileDirectory
  , fileFileModifyDate
  , fileFileSize
  , fileImgType
  , fileName
  , fileRefImg
  , fileRefJpg
  , fileRefRaw
  , fileTimeStamp
  ] = [File'CheckSum .. File'TimeStamp]

imgRating
  , imgEXIFUpdate :: MetaKey

keysAttrImg :: [MetaKey]
keysAttrImg@
  [ imgEXIFUpdate
  , imgRating
  ] = [Img'EXIFUpdate .. Img'Rating]

makerNotesColorSpace
  , makerNotesDaylightSavings
  , makerNotesFocusDistance
  , makerNotesFocusMode
  , makerNotesQuality
  , makerNotesSerialNumber
  , makerNotesShootingMode
  , makerNotesShutterCount
  , makerNotesTimeZone :: MetaKey

keysAttrMaker :: [MetaKey]
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
  ] = [MakerNotes'ColorSpace .. MakerNotes'TimeZone]

quickTimeDuration
  , quickTimeImageWidth
  , quickTimeImageHeight
  , quickTimeVideoFrameRate :: MetaKey

keysAttrQuickTime :: [MetaKey]
keysAttrQuickTime@
  [ quickTimeDuration
  , quickTimeImageWidth
  , quickTimeImageHeight
  , quickTimeVideoFrameRate
  ] = [QuickTime'Duration .. QuickTime'VideoFrameRate]

xmpGPSAltitude
  , xmpGPSLatitude
  , xmpGPSLongitude
  , xmpRating :: MetaKey

keysAttrXmp :: [MetaKey]
keysAttrXmp@
  [ xmpGPSAltitude
  , xmpGPSLatitude
  , xmpGPSLongitude
  , xmpRating
  ] = [XMP'GPSAltitude .. XMP'Rating]

-- ----------------------------------------
--
-- split metadata in image metadata and part specific metadata

splitMetaData :: ImgType -> MetaData' a -> (MetaData' a, MetaData' a)
splitMetaData ty md =
  partitionMD (not <$> snd (keysByImgType ty)) md

-- filter relevant metadata from exiftool output
cleanupMetaData :: ImgType -> MetaData' a -> MetaData' a
cleanupMetaData ty =
  filterKeysMD (i'keys .||. p'keys)  -- all interesting stuff
  where
    (i'keys, p'keys) = keysByImgType ty

keysByImgType :: ImgType -> (MetaKeySet, MetaKeySet)
keysByImgType ty
  | isJpg ty
    ||
    isImg   ty = (ks'cexm,  ks'part)
  | isMovie ty = (ks'cexmq, ks'part)
  | isMeta  ty = (ks'gpsr,  ks'file)
  | isTxt   ty = (ks'descr, ks'file)
  | isRaw   ty = (ks'cexm,  ks'part)
  | otherwise  = (ks'all,   ks'part)
  where
    ks'all   = const True
    ks'part  = ks'file  .||. ks'geo .||. ks'rat

    ks'comp  = (`elem` keysAttrComposite)
    ks'descr = (`elem` keysAttrDescr)
    ks'exif  = (`elem` keysAttrExif)
    ks'file  = (`elem` keysAttrFile)
    ks'maker = (`elem` keysAttrMaker)
    ks'qtime = (`elem` keysAttrQuickTime)

    ks'cexm  = ks'comp  .||. ks'descr .||. ks'exif .||. ks'maker
    ks'cexmq = ks'cexmq .||. ks'qtime
    ks'gps   = (`elem` [ compositeGPSAltitude
                       , compositeGPSLatitude
                       , compositeGPSLongitude
                       , compositeGPSPosition
                       , descrGPSPosition
                       , xmpGPSAltitude
                       , xmpGPSLatitude
                       , xmpGPSLongitude
                       ]
               )
    ks'rat   = (== descrRating)

    ks'gpsr  = ks'gps             -- gps data in .xmp files
               .||.
               (== xmpRating)     -- rating in .xmp files

    ks'geo   = (`elem` [ compositeImageSize
                       , compositeMegapixels
                       , exifImageHeight
                       , exifImageWidth
                       , exifOrientation
                       , makerNotesColorSpace
                       ]
               )

-- ----------------------------------------
--
-- filter meta data enries by image type

filterByImgType :: ImgType -> MetaData' a -> MetaData' a
filterByImgType ty =
  filterKeysMD (`elem` ks)
  where
    ks | isShowablePartOrRaw ty = ksRaw
       | isMeta              ty = ksXmp
       | otherwise              = mzero

    ksRaw = mconcat
      [ keysAttrComposite
      , keysAttrDescr
      , keysAttrExif
      , keysAttrFile
      , keysAttrImg
      , keysAttrMaker
      , keysAttrQuickTime
      , keysAttrXmp
      ]

    ksXmp = mconcat
      [ keysAttrComposite
      , keysAttrXmp
      ]

-- lookup a sequence of keys
-- combine MetaValues with <>
-- in most cases: take the first defined value

lookupByKeys :: [MetaKey] -> MetaData -> MetaValue
lookupByKeys ns mt =
  mconcat $ map (flip lookupMD mt) ns

-- access the create date of an image
-- usually formated as YYYY:MM:DD hh:mm:ss(.ss)

lookupCreate :: (Text -> res) -> MetaData -> res
lookupCreate p mt = p (v ^. isoMetaValueText exifCreateDate)
  where
    v = lookupByKeys
        [ compositeSubSecCreateDate
        , exifCreateDate
        ] mt

-- access image geometry and orientation
lookupGeoOri :: MetaData -> (Geo, Int)
lookupGeoOri = lookupGeo &&& lookupOri

lookupGeo :: MetaData -> Geo
lookupGeo mt =
  toGeo $ mt ^. metaTextAt compositeImageSize
  where
    toGeo sz = fromMaybe geo'org (readGeo'' $ sz ^. isoString)

lookupRating :: MetaData -> Rating
lookupRating mt =
  lookupByKeys      -- imgRating is used in genPages and has type Text
  [ descrRating     -- descr:Rating has priority over
  , xmpRating       -- XMP:Rating from LR, image attr
  ] mt ^. metaRating

mkRating :: Rating -> MetaData -> MetaData
mkRating r md = md & metaDataAt descrRating .~ metaRating # r

lookupOri :: MetaData -> Int
lookupOri mt = mt ^. metaDataAt exifOrientation . metaOri

lookupGPSposDeg :: MetaData -> Text
lookupGPSposDeg =
  (^. metaGPSDegText) . lookupByKeys [descrGPSPosition, compositeGPSPosition]

-- ----------------------------------------

newtype MetaData' a = MD (IM.IntMap a)

class IsoValueText v where
  isoValueText :: MetaKey -> Iso' v Text

instance IsoValueText MetaValue where
  isoValueText = isoMetaValueText

instance IsoValueText Text where
  isoValueText = const id

isoMetaDataMDT :: IsoValueText a
               => Iso' (MetaData' a) MetaDataText
isoMetaDataMDT = iso toMDT frMDT
  where
    toMDT (MD m) = MDT m'
      where
        m' = IM.foldrWithKey' ins M.empty m
          where
            ins k0 v
              | isempty k = id
              | otherwise = M.insert (metaKeyToText k) (v ^. isoValueText k)
              where
                k = toEnum k0

    frMDT (MDT m) = MD m'
      where
        m' = M.foldrWithKey' ins IM.empty m
          where
            ins k0 v
              | isempty k = id
              | otherwise = IM.insert (fromEnum k) (isoValueText k # v)
              where
                k = metaKeyLookup k0

instance IsoValueText a => FromJSON (MetaData' a) where
  parseJSON o = (isoMetaDataMDT #) <$> parseJSON o

instance IsoValueText a => ToJSON (MetaData' a) where
  toJSON m = toJSON $ m ^. isoMetaDataMDT

-- --------------------

type MetaData  = MetaData' MetaValue
type MetaDataT = MetaData' Text

newtype MetaDataText = MDT (M.Map Text Text)

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
  | EXIF'ImageHeight         -- TODO: redundant?
  | EXIF'ImageWidth          --  "        "
  | EXIF'ISO
  | EXIF'Make
  | EXIF'MaxApertureValue
  | EXIF'MeteringMode
  | EXIF'Model
  | EXIF'Orientation
  | EXIF'UserComment
  | EXIF'WhiteBalance
  | File'CheckSum
  | File'Directory
  | File'FileModifyDate
  | File'FileSize
  | File'ImgType
  | File'Name
  | File'RefImg
  | File'RefJpg
  | File'RefRaw
  | File'TimeStamp
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
  | XMP'GPSAltitude
  | XMP'GPSLatitude
  | XMP'GPSLongitude
  | XMP'Rating
  | Key'Unknown          -- must be the last value

type MetaKeySet = MetaKey -> Bool

data MetaValue
  = MText !Text
  | MNm   !Name
  | MInt  !Int
  | MRat  !Int            -- rating: 0..5
  | MOri  !Int            -- orientation: 0..3 <-> 0, 90, 180, 270 degrees CW
  | MKeyw ![Text]         -- keywords
  | MAcc  !Access         -- access restrictions
  | MTs   !TimeStamp      -- time stamp
  | MGps  !GPSposDec      -- GPS coordinate
  | MCS   !CheckSum       -- checksum
  | MTyp  !ImgType        -- image (or file) type
  | MNull                 -- missing or default value

-- invariant for MetaValue:
-- isempty mv => mv == MNull
--
-- the invariant will be guaranteed by the setter parts
-- of "meta***" iso's

type Rating = Int -- 0 .. 5

-- --------------------
--
-- ops for access rights in metadata

modifyAccess :: (Access -> Access) -> MetaData -> MetaData
modifyAccess f mt =
  mt & metaDataAt Descr'Access . metaAcc %~ f

allowAccess
  , restrAccess :: [AccessRestr] -> MetaData -> MetaData

allowAccess rs = modifyAccess (.&. complement (isoAccessRestr # rs))
restrAccess rs = modifyAccess (.|. (isoAccessRestr # rs))

clearAccess
  , addNoWriteAccess
  , subNoWriteAccess :: MetaData -> MetaData

clearAccess       = allowAccess [minBound .. maxBound]
addNoWriteAccess  = restrAccess [NO'write]
subNoWriteAccess  = allowAccess [NO'write]

isWriteable
  , isSortable
  , isRemovable
  , isAUserCol :: MetaData -> Bool
isWriteable = doesn'tHave NO'write
isSortable  = doesn'tHave NO'sort
isRemovable = doesn'tHave NO'delete
isAUserCol  = doesn'tHave NO'user

doesn'tHave :: AccessRestr -> MetaData -> Bool
doesn'tHave r mt = not $ mt ^. metaDataAt Descr'Access . metaAcc . accessRestr r

-- --------------------
--
-- instances for MetaDataText

instance FromJSON MetaDataText where
  parseJSON j =
    ( \ o -> MDT <$> parseJSON o    -- parse a {...} as Map Text Text
    ) j
    <|>                             -- old version: parse a [{...}] as Map Text Text
                                    -- this is a wart, but donwards compatible
    ( J.withArray "[MetaDataText]" $ \ v ->
        case V.length v of
          1 -> parseJSON (V.head v)
          _ -> mzero
    ) j

instance ToJSON MetaDataText where
  toJSON (MDT m) = J.toJSON m

-- --------------------
--
-- instances for MetaData

deriving instance Eq   MetaData   -- used in Catalog.MetaData.Exif
deriving instance Show MetaData

instance IsEmpty MetaData where
  isempty (MD m) = IM.null m

instance Semigroup MetaData where
  (<>) = unionMD

instance Monoid MetaData where
  mempty = MD IM.empty

-- instance ToJSON MetaData where
--  toJSON m = J.toJSON (m ^. isoMDT)

-- instance FromJSON MetaData where
--  parseJSON o = (isoMDT #) <$> parseJSON o

-- lens combining insertMD and lookupMD
metaDataAt :: MetaKey -> Lens' MetaData MetaValue
metaDataAt mk k mt = (\ v -> insertMD mk v mt) <$> k (lookupMD mk mt)

metaTextAt :: MetaKey -> Lens' MetaData Text
metaTextAt k = metaDataAt k . isoMetaValueText k

insertMD :: MetaKey -> MetaValue -> MetaData -> MetaData
insertMD k v mt@(MD m)
  | isempty k   = mt                               -- no redundant stuff in metatable
  | isempty v   = MD $ IM.delete (fromEnum k)   m  -- dto
  | otherwise   = MD $ IM.insert (fromEnum k) v m

lookupMD :: MetaKey -> MetaData -> MetaValue
lookupMD k (MD m) = fromMaybe mempty $ IM.lookup (fromEnum k) m

-- <> for meta tables
unionMD :: MetaData -> MetaData -> MetaData
unionMD (MD m1) (MD m2) = MD $ IM.unionWith (<>) m1 m2

toListMD :: MetaData -> [(MetaKey, MetaValue)]
toListMD (MD m) = map (first toEnum) $ IM.toAscList m

filterKeysMD :: MetaKeySet -> MetaData' a -> MetaData' a
filterKeysMD p (MD m) = MD $ IM.filterWithKey (\ i _v -> p $ toEnum i) m

partitionMD :: MetaKeySet -> MetaData' a -> (MetaData' a, MetaData' a)
partitionMD ks (MD m) = (MD m1, MD m2)
  where
    (m1, m2) = IM.partitionWithKey (\ i _v -> ks $ toEnum i) m

-- --------------------
{- old stuff: unse isoMetaDataMDT

isoMDT :: Iso' MetaData MetaDataText
isoMDT = iso mt2tt (flip editMD mempty)

mt2tt :: MetaData -> MetaDataText
mt2tt (MD m) = MDT $ IM.foldlWithKey' ins M.empty m
  where
    ins acc i v =
      M.insert (metaKeyTextLookup k) (v ^. isoMetaValueText k) acc
      where
        k = toEnum i
-- -}

editMD :: MetaDataT -> MetaData -> MetaData
editMD (MD m) mt = IM.foldlWithKey' ins mt m
  where
    ins acc k0 v0
      | v0 == "-" =                      -- remove key from metadata
          acc & metaDataAt k .~ mempty

      | k == descrKeywords =             -- add and remove keywords
          acc & metaDataAt k . metaKeywords %~ mergeKW (isoKeywText # v0)

      | otherwise =                      -- insert or set new value
          acc & metaDataAt k .~ isoMetaValueText k # v0
      where
        k = toEnum k0

splitMDT :: MetaDataText -> (MetaDataT, MetaDataT)
splitMDT mdt = splitMetaData IMGother (isoMetaDataMDT # mdt)

-- --------------------
--
-- instances and basic ops for MetaValue

deriving instance Eq   MetaValue
deriving instance Show MetaValue

instance IsEmpty MetaValue where   -- default values are redundant
  isempty (MText t)  = isempty t
  isempty (MNm  n)   = isempty n
  isempty (MInt _)   = False       -- no default value for Int
  isempty (MOri o)   = o == 0
  isempty (MRat r)   = r == 0
  isempty (MKeyw ws) = null ws
  isempty (MAcc a)   = a == no'restr
  isempty (MTs t)    = t == mempty
  isempty (MCS cs)   = isempty cs
  isempty (MTyp t)   = isempty t
  isempty (MGps _)   = False       -- no default value for GPS data
  isempty  MNull     = True

instance Semigroup MetaValue where
  MNull          <> mv2      = mv2     -- 1. arg null: take the other one
  mv1            <> MNull    = mv1     -- 2. arg null: take the other one

  mv1@(MText _)  <> MText _  = mv1     -- 1. wins
  mv1@(MNm  _)   <> MNm  _   = mv1     -- 1. wins
  mv1@(MInt _)   <> MInt _   = mv1     -- 1. wins
  mv1@(MRat _)   <> MRat _   = mv1     -- 1. wins
  mv1@(MOri _)   <> MOri _   = mv1     -- 1. wins
  mv1@(MAcc _)   <> MAcc _   = mv1     -- 1. wins
  mv1@(MGps _)   <> MGps _   = mv1     -- 1. wins
  mv1@(MCS  _)   <> MCS  _   = mv1     -- 1. wins
  mv1@(MTyp _)   <> MTyp _   = mv1     -- 1. wins
  MTs   t1       <> MTs   t2 = MTs   $ t1 <> t2       -- newest wins
  MKeyw w1       <> MKeyw w2 = MKeyw $ unionKW w1 w2  -- keywords are collected

  _              <> mv2      = mv2     -- mixing types, no effect

instance Monoid MetaValue where
  mempty = MNull

-- MetaKey determines the MetaValue representation
-- no instance of FromJSON, due to decoding dependency on the key
--
-- getter and setter for MataValue

metaText :: Iso' MetaValue Text
metaText = iso
  (\ case
      MText t -> t
      _       -> mempty
  )
  (\ t -> if isempty t
          then mempty
          else MText t
  )
{-# INLINE metaText #-}

metaName :: Iso' MetaValue Name
metaName = iso
  (\ case
      MNm n -> n
      _     -> mempty
  )
  (\ n -> if isempty n
          then mempty
          else MNm n
  )
{-# INLINE metaName #-}

metaInt :: Iso' MetaValue (Maybe Int)
metaInt = iso
  (\ case
      MInt i -> Just i
      _      -> Nothing
  )
  (maybe mempty MInt)
{-# INLINE metaInt #-}

metaRating :: Iso' MetaValue Int
metaRating = iso
  (\case
      MRat i -> i
      _      -> 0
  )
  (\ i -> let i' = (i `max` 0) `min` ratingMax
          in if i' == 0
             then mempty
             else MRat i'
  )
{-# INLINE metaRating #-}

metaOri :: Iso' MetaValue Int
metaOri = iso
  (\ case
      MOri i -> i
      _      -> 0
  )
  (\ i -> let i' = (i `max` 0) `min` 3
          in if i' == 0
             then mempty
             else MOri i'
  )
{-# INLINE metaOri #-}

metaKeywords :: Iso' MetaValue [Text]
metaKeywords = iso
  (\ case
      MKeyw kw -> kw
      _        -> []
  )
  (\ ws -> if null ws
           then mempty
           else MKeyw ws
  )
{-# INLINE metaKeywords #-}

metaAcc :: Iso' MetaValue Access
metaAcc = iso
  (\ case
      MAcc a -> a
      _      -> no'restr
  )
  (\ a -> if a == no'restr
          then mempty
          else MAcc a
  )
{-# INLINE metaAcc #-}

metaAccess :: Iso' MetaValue [AccessRestr]
metaAccess = metaAcc . isoAccessRestr
{-# INLINE metaAccess #-}

metaTimeStamp :: Iso' MetaValue TimeStamp
metaTimeStamp = iso
  (\ case
      MTs t -> t
      _     -> mempty
  )
  (\ t -> if isempty t
          then mempty
          else MTs t
  )
{-# INLINE metaTimeStamp #-}

metaGPS :: Iso' MetaValue (Maybe GPSposDec)  -- no default for GPS
metaGPS = iso
  (\ x -> case x of
            MGps p -> Just p
            _      -> Nothing
  )
  (maybe mempty MGps)
{-# INLINE metaGPS #-}

metaCheckSum :: Iso' MetaValue CheckSum
metaCheckSum = iso
  (\ case
      MCS cs -> cs
      _      -> mempty
  )
  (\ c -> if isempty c
          then mempty
          else MCS c
  )
{-# INLINE metaCheckSum #-}

metaImgType :: Iso' MetaValue ImgType
metaImgType = iso
  (\ case
      MTyp t -> t
      _      -> mempty
  )
  (\ t -> if isempty t
          then mempty
          else MTyp t
  )
{-# INLINE metaImgType #-}

-- --------------------
--
-- MetaValue <-> Text

metaIntText :: Iso' MetaValue Text
metaIntText = metaInt . isoMaybeIntText
{-# INLINE metaIntText #-}

metaRatingText :: Iso' MetaValue Text
metaRatingText =
  metaRating . iso (^. isoIntText) frT
  where
    frT :: Text -> Int
    frT t = (isoIntText # t)    -- rating as number           "5"   -> 5
            `max`               -- illegal text               "?"   -> 0
            (isoStars # t)      -- rating as sequence of *'s  "***" -> 3

metaGPSDecText :: Iso' MetaValue Text
metaGPSDecText = metaGPS . isoGPSDec . isoText
  where
    isoGPSDec :: Iso' (Maybe GPSposDec) String
    isoGPSDec = iso toS frS
      where
        toS = maybe mempty (& (prismString #))
        frS = (^? googleMapsGPSdec)

metaGPSDegText :: Iso' MetaValue Text
metaGPSDegText = metaGPS . isoGPSDeg . isoText
  where
    isoGPSDeg :: Iso' (Maybe GPSposDec) String
    isoGPSDeg = iso toS frS
      where
        toS v = maybe mempty (\ p -> prismString # (isoDegDec # p)) v
        frS s = (^. isoDegDec) <$> (s ^? prismString)

-- --------------------
--
-- auxiliary iso's for conversion of MetaData <-> Text

{-# INLINE isoKeywText #-}
isoKeywText :: Iso' [Text] Text
isoKeywText = iso toT frT
  where
    toT :: [Text] -> Text
    toT = T.intercalate ", "

    frT :: Text -> [Text]
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
      1 ->  d90
      2 -> d180
      3 -> d270
      _ ->   d0

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
{-# INLINE isoOriText #-}


-- 0 acts as default value
-- 0 is mapped to the empty Text
-- strings representing numbers are converted into the number
-- all other strings are mapped to 0

isoIntText :: Iso' Int Text
isoIntText = isoIntStr . isoText
  where
    isoIntStr :: Iso' Int String
    isoIntStr =
      iso
      (\ i -> if i == 0
              then mempty
              else show i
      )
      (fromMaybe 0 . readMaybe)
{-# INLINE isoIntText #-}

isoMaybeIntText :: Iso' (Maybe Int) Text
isoMaybeIntText =
  iso (maybe mempty show) readMaybe . isoText
{-# INLINE isoMaybeIntText #-}

-- --------------------
--
-- mapping of MetaValue variants to MetaKey's

isoMetaValueText :: MetaKey -> Iso' MetaValue Text
isoMetaValueText k = case k of
  Composite'GPSPosition -> metaGPSDecText
  Descr'Access          -> metaAccess    . isoAccText
  Descr'GPSPosition     -> metaGPSDecText
  Descr'Keywords        -> metaKeywords  . isoKeywText
  Descr'Rating          -> metaRatingText
  EXIF'ImageHeight      -> metaIntText
  EXIF'ImageWidth       -> metaIntText
  EXIF'Orientation      -> metaOri       . isoOriText
  File'CheckSum         -> metaCheckSum  . isoText
  File'ImgType          -> metaImgType   . isoText
  File'Name             -> metaName      . isoText
  File'TimeStamp        -> metaTimeStamp . isoText
  Img'Rating            -> metaText          -- used in genPages
  Img'EXIFUpdate        -> metaTimeStamp . isoText
  QuickTime'ImageHeight -> metaIntText
  QuickTime'ImageWidth  -> metaIntText
  XMP'Rating            -> metaRatingText
  Key'Unknown           -> iso (const mempty) (const mempty)
  _                     -> metaText

-- --------------------

unionKW :: [Text] -> [Text] -> [Text]
unionKW ws1 ws2 = nub (ws1 ++ ws2)
{-# INLINE unionKW #-}

-- used when editing metadata: add/rem keywords
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
-- rating ops

ratingMax :: Rating
ratingMax = 5

isoStars :: Iso' Rating Text
isoStars = isoStars' . isoText
  where
    isoStars' = iso (flip replicate '*')
                (min ratingMax . length . filter (== '*'))
{-# INLINE isoStars #-}

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

metaKeyLookup :: Text -> MetaKey
metaKeyLookup t =
  fromMaybe Key'Unknown $
  HM.lookup (T.toLower t) metaKeyLookupTable
{-# INLINE metaKeyLookup #-}

metaKeyTextLookup :: MetaKey -> Text
metaKeyTextLookup k =
  fromMaybe mempty $ IM.lookup (fromEnum k) metaKeyToTextTable

-- --------------------

allKeysMD :: [MetaKey]
allKeysMD = [minBound .. pred maxBound]

someKeysMD :: (Text -> Bool) -> [MetaKey]
someKeysMD p = filter (p . metaKeyToText) allKeysMD

globKeysMD :: SP String -> [MetaKey]
globKeysMD gp = someKeysMD p
  where
    p t = matchP gp (t ^. isoString)

prettyMD :: MetaData -> [Text]
prettyMD mt = zipWith (<:>) ks vs
  where
    kvs = toListMD mt
    ks  = T.fillRightList ' ' $ map (^. _1 . isoText) kvs
    vs  = map (\ (k, v) -> v ^. isoMetaValueText k) kvs

    xs <:> ys = xs <> " : " <> ys

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

type MetaKeyLookupTable = HashMap Text MetaKey

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
