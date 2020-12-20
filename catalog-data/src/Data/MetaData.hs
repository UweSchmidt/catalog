{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Data.MetaData
  ( MetaKey
  , MetaValue
  , MetaData
  , MetaDataText
  , Rating
  , AccessRestr

  , metaTableAt
{-
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
-}
  , clearAccess
  , addNoDeleteAccess
  , addNoSortAccess
  , addNoWriteAccess
  , subNoDeleteAccess
  , subNoSortAccess
  , subNoWriteAccess

  , isWriteable
  , isSortable
  , isRemovable

  , lookupMetaText
  , lookupCreate
  , lookupFileName
  , lookupGPSposDeg
  , lookupGeoOri
  , lookupGeo
  , lookupOri

--  , mkRating
--  , getRating
--  , isoRating

{-
  , getEXIFUpdateTime
  , setEXIFUpdateTime

  , compareByName
  , compareByCreateDate

  , filterMetaData
-}
  , parseTime
  , parseDate
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

  , fileDirectory
  , fileFileSize
  , fileFileModifyDate
  , fileFileName
  , fileMIMEType
  , fileRefRaw
  , fileRefImg
  , fileRefJpg

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
  , xmpFormat
  , xmpRawFileName
  , xmpRating
  )
where

import           Data.Prim
import           Data.Bits           ( bit, (.|.), (.&.), complement
                                     , testBit, setBit, clearBit
                                     )
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
import qualified Text.Pretty         as T
import           Text.Printf         ( printf )
-- import Debug.Trace

-- ----------------------------------------
-- ----------------------------------------
--
-- compare function on meta data
{-
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
-}
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
--
-- meta keys

fileDirectory
  , fileFileSize
  , fileFileModifyDate
  , fileFileName
  , fileMIMEType
  , fileRefRaw
  , fileRefImg
  , fileRefJpg :: MetaKey

keysAttrFile :: [MetaKey]
keysAttrFile@
  [ fileDirectory
  , fileFileSize
  , fileFileModifyDate
  , fileFileName
  , fileMIMEType
  , fileRefRaw
  , fileRefImg
  , fileRefJpg
  ] = [File'Directory .. File'RefJpg]

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

xmpGPSLatitude
  , xmpGPSLongitude
  , xmpGPSAltitude
  , xmpFormat
  , xmpRawFileName
  , xmpRating :: MetaKey

keysAttrXmp :: [MetaKey]
keysAttrXmp@
  [ xmpGPSLatitude
  , xmpGPSLongitude
  , xmpGPSAltitude
  , xmpFormat
  , xmpRawFileName
  , xmpRating
  ] = [XMP'GPSLatitude .. XMP'Rating]

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

keysAttrCol :: [MetaKey]
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
  ] = [Descr'Title .. Descr'CatalogWrite]


imgRating
  , imgEXIFUpdate :: MetaKey

keysAttrImg :: [MetaKey]
keysAttrImg@
  [ imgRating
  , imgEXIFUpdate
  ] = [ Img'Rating .. Img'EXIFUpdate]

-- ----------------------------------------

partByKey :: (MetaKey -> Bool) -> Iso' MetaData (MetaData, MetaData)
partByKey p = iso part (uncurry (<>))
  where
    part = foldWithKeyMT f (mempty, mempty)
      where
        f k v acc
          | p k       = acc & _1 %~ insertMT k v
          | otherwise = acc & _2 %~ insertMT k v

selectByKeys :: (MetaKey -> Bool) -> Lens' MetaData MetaData
selectByKeys p = partByKey p . _1


-- filter meta data enries by image type

filterByImgType :: ImgType -> MetaData -> MetaData
filterByImgType ty md =
  md ^. selectByKeys (`elem` ks)
  where
    ks | isShowablePartOrRaw ty = ksRaw
       | isMeta              ty = ksXmp
       | otherwise              = mzero

    ksRaw = mconcat
      [ keysAttrComposite
      , keysAttrCol
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

lookupByKeys :: [MetaKey] -> MetaData -> MetaValue
lookupByKeys ns mt =
  mconcat $ map (flip lookupMT mt) ns
{-
  foldr f mempty $ map (flip lookupMT mt) ns
  where
    f mv r
      | isempty mv = r
      | otherwise  = mv
-- -}

lookupCreate :: (Text -> res) -> MetaData -> res
lookupCreate p mt = p cd
  where
    cd = metaValueToText $
         lookupByKeys
         [ compositeSubSecCreateDate
         , exifCreateDate
         ] mt

lookupFileName :: MetaData -> Maybe Text
lookupFileName mt
  | isempty n = Nothing
  | otherwise  = Just n
  where
   n = metaValueToText $ mt ^. metaTableAt File'FileName

lookupGeoOri :: MetaData -> (Geo, Int)
lookupGeoOri = lookupGeo &&& lookupOri

lookupGeo :: MetaData -> Geo
lookupGeo mt =
  toGeo . metaValueToText $ mt ^. metaTableAt Composite'ImageSize
  where
    toGeo sz = fromMaybe geo'org (readGeo'' $ sz ^. isoString)


lookupOri :: MetaData -> Int
lookupOri mt = mt ^. metaTableAt EXIF'Orientation . metaOri

lookupRating :: MetaData -> Rating
lookupRating mt =
  lookupByKeys
  [ Descr'Rating     -- descr:Rating has priority over
  , XMP'Rating       -- XMP:Rating from LR
  ] mt ^. metaRating

lookupUpdateTime :: MetaData -> TimeStamp
lookupUpdateTime mt =
  mt ^. metaTableAt Img'EXIFUpdate . metaTimeStamp

setUpdateTime :: TimeStamp -> MetaData -> MetaData
setUpdateTime ts mt =
  mt & metaTableAt Img'EXIFUpdate . metaTimeStamp .~ ts

lookupGPSposDeg :: MetaData -> Text
lookupGPSposDeg =
  (^. metaGPSDegText) . lookupByKeys [descrGPSPosition, compositeGPSPosition]

-- ----------------------------------------

newtype MetaData  = MT (IM.IntMap MetaValue)

type MetaDataText = HM.HashMap Text Text

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
  | MTs   TimeStamp      -- time stamp
  | MGps  GPSposDec      -- GPS coordinate
  | MNull

data AccessRestr = NO'write | NO'delete | NO'sort

type Access = Int

type Rating = Int -- 0 .. 5

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


-- indexed access to a single restriction

accessRestr :: AccessRestr -> Lens' Access Bool
accessRestr r k a =
  (\ b -> ( if b
            then setBit
            else clearBit
          ) a (fromEnum r)
  ) <$>
  k (testBit a (fromEnum r))


isoAccessRestr :: Iso' Access [AccessRestr]
isoAccessRestr = iso toS frS
  where
    toS :: Access -> [AccessRestr]
    toS a = foldr add [] [minBound .. maxBound]
      where
        add r acc
          | a ^. accessRestr r = r : acc
          | otherwise          =     acc

    frS rs = foldl' sb no''restr rs
      where
        sb :: Access -> AccessRestr -> Access
        sb a r = a & accessRestr r .~ True



modifyAccess :: (Access -> Access) -> MetaData -> MetaData
modifyAccess f mt =
  mt & metaTableAt Descr'Access . metaAcc %~ f

setAccess
  , allowAccess
  , restrAccess :: [AccessRestr] -> MetaData -> MetaData

setAccess   rs = modifyAccess (.&. complement (isoAccessRestr # rs))
allowAccess rs = modifyAccess (const $ isoAccessRestr # rs)
restrAccess rs = modifyAccess ((isoAccessRestr # rs) .|.)

clearAccess
  , addNoWriteAccess
  , addNoSortAccess
  , addNoDeleteAccess
  , subNoWriteAccess
  , subNoSortAccess
  , subNoDeleteAccess :: MetaData -> MetaData

clearAccess       = setAccess   []
addNoWriteAccess  = restrAccess [NO'write]
addNoSortAccess   = restrAccess [NO'sort]
addNoDeleteAccess = restrAccess [NO'delete]
subNoWriteAccess  = allowAccess [NO'write]
subNoSortAccess   = allowAccess [NO'sort]
subNoDeleteAccess = allowAccess [NO'delete]

isWriteable
  , isSortable, isRemovable :: MetaData -> Bool
isWriteable = isAccessable NO'write
isSortable  = isAccessable NO'sort
isRemovable = isAccessable NO'delete

isAccessable :: AccessRestr -> MetaData -> Bool
isAccessable r mt = not $ mt ^. metaTableAt Descr'Access . metaAcc . accessRestr r

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

-- --------------------
--
-- instances for MetaData

deriving instance Show MetaData

instance IsEmpty MetaData where
  isempty (MT m) = IM.null m

instance Semigroup MetaData where
  (<>) = unionMT

instance Monoid MetaData where
  mempty = MT IM.empty

instance ToJSON MetaData where
  toJSON m = J.toJSON [m ^. isoMTT]

instance FromJSON MetaData where
  parseJSON = J.withArray "MetaData" $ \ v ->
    case V.length v of
      1 -> parseTable (V.head v)
      _ -> mzero
    where
      parseTable o = (isoMTT #) <$> parseJSON o

-- lens combining insertMT and lookupMT
metaTableAt :: MetaKey -> Lens' MetaData MetaValue
metaTableAt mk k mt = (\ v -> insertMT mk v mt) <$> k (lookupMT mk mt)

lookupMetaText :: MetaKey -> MetaData -> Text
lookupMetaText k mt = metaValueToText $ lookupMT k mt

insertMT :: MetaKey -> MetaValue -> MetaData -> MetaData
insertMT k v mt@(MT m)
  | isempty k   = mt                               -- no redundant stuff in metatable
  | isempty v   = MT $ IM.delete (fromEnum k)   m  -- dto
  | otherwise   = MT $ IM.insert (fromEnum k) v m

lookupMT :: MetaKey -> MetaData -> MetaValue
lookupMT k (MT m) = fromMaybe mempty $ IM.lookup (fromEnum k) m

-- <> for meta tables
unionMT :: MetaData -> MetaData -> MetaData
unionMT (MT m1) (MT m2) = MT $ IM.unionWith (<>) m1 m2
{-
  foldWithKeyMT mergeMV m2 m1   -- fold over m1
  where
    mergeMV k1 v1 acc = insertMT k1 (v1 <> v2) acc
      where
        v2 = lookupMT k1 m2
-- -}

foldWithKeyMT :: (MetaKey -> MetaValue -> a -> a) -> a -> MetaData -> a
foldWithKeyMT f acc (MT m) =
  IM.foldlWithKey' f' acc m
  where
    f' acc' k' mv' = f (toEnum k') mv' acc'

keysMT :: MetaData -> [MetaKey]
keysMT (MT m) = map toEnum $ IM.keys m

toListMT :: MetaData -> [(MetaKey, MetaValue)]
toListMT (MT m) = map (first toEnum) $ IM.toAscList m

-- --------------------

isoMTT :: Iso' MetaData MetaDataText
isoMTT = iso mt2tt (flip editMT mempty)

mt2tt :: MetaData -> MetaDataText
mt2tt (MT m) = IM.foldlWithKey' ins HM.empty m
  where
    ins acc i mv =
      HM.insert (metaKeyTextLookup $ toEnum i) (metaValueToText mv) acc


editMT :: MetaDataText -> MetaData -> MetaData
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
  isempty (MAcc a)   = a == no''restr
  isempty (MTs t)    = t == mempty
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
  mv1@(MTs  _)   <> MTs  _   = mv1     -- 1. wins
  mv1@(MGps _)   <> MGps _   = mv1     -- 1. wins
  MKeyw w1       <> MKeyw w2 = MKeyw $ unionKW w1 w2

  _              <> mv2      = mv2     -- mixing types, no effect

instance Monoid MetaValue where
  mempty = MNull

-- MetaKey determines the MetaValue representation
-- no instance of FromJSON, due to decoding dependency on the key

metaText :: Iso' MetaValue Text
metaText = iso
  (\ x -> case x of
            MText t -> t
            _       -> mempty
  )
  MText

metaInt :: Iso' MetaValue Int
metaInt = iso
  (\ x -> case x of
            MInt i -> i
            _      -> 0
  )
  MInt

metaIntText :: Iso' MetaValue Text
metaIntText = metaInt . isoIntText

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
  (\ i -> MRat $ (i `max` 0) `min` ratingMax)

metaRatingText :: Iso' MetaValue Text
metaRatingText = metaRating . isoIntText'
  where
    isoIntText' = iso (^. isoIntText) frT
    frT t = (isoIntText # t)    -- rating as number
            `max`
            (isoStars # t)      -- rating as sequence of *'s

metaOri :: Iso' MetaValue Int
metaOri = iso
  (\ x -> case x of
            MOri i -> i
            _      -> 0
  )
  (\ i -> MOri $ (i `max` 0) `min` 3)

metaOriText :: Iso' MetaValue Text
metaOriText = metaOri . isoOriText

metaTimeStamp :: Iso' MetaValue TimeStamp
metaTimeStamp = iso
  (\ x -> case x of
            MTs t -> t
            _     -> mempty
  )
  MTs

metaTimeStampText :: Iso' MetaValue Text
metaTimeStampText = metaTimeStamp . isoText

metaGPS :: Iso' MetaValue (Maybe GPSposDec)
metaGPS = iso
  (\ x -> case x of
            MGps p -> Just p
            _      -> Nothing
  )
  (maybe mempty MGps)

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

metaAcc :: Iso' MetaValue Access
metaAcc = iso
  (\ x -> case x of
            MAcc a -> a
            _      -> no''restr
  )
  MAcc

metaAccess :: Iso' MetaValue [AccessRestr]
metaAccess = metaAcc . isoAccessRestr

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
  Descr'Access          -> metaAccessText    # t
  Composite'GPSPosition -> metaGPSDecText    # t
  Descr'GPSPosition     -> metaGPSDecText    # t
  Descr'Keywords        -> metaKeywordsText  # t
  Descr'Rating          -> metaRatingText    # t
  EXIF'Orientation      -> metaOriText       # t
  Img'Rating            -> metaRatingText    # t
  Img'EXIFUpdate        -> metaTimeStampText # t
  XMP'Rating            -> metaRatingText    # t
  Key'Unknown           -> MNull
  _ | isempty t         -> MNull
    | otherwise         -> MText t

-- --------------------
--
-- convert to text for JSON

metaValueToText :: MetaValue -> Text
metaValueToText mv = case mv of
  MText t -> t
  MInt  i -> mv ^. metaIntText
  MRat  _ -> mv ^. metaRatingText
  MOri  _ -> mv ^. metaOriText
  MAcc  _ -> mv ^. metaAccessText
  MKeyw _ -> mv ^. metaKeywordsText
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

allKeysMT :: [MetaKey]
allKeysMT = [minBound .. pred maxBound]

someKeysMT :: (Text -> Bool) -> [MetaKey]
someKeysMT p = filter (p . metaKeyToText) allKeysMT

globKeysMT :: SP String -> [MetaKey]
globKeysMT gp = someKeysMT p
  where
    p t = matchP gp (t ^. isoString)

prettyMT :: MetaData -> [Text]
prettyMT mt = zipWith (<:>) ks vs
  where
    kvs = toListMT mt
    ks  = T.fillRightList ' ' $ map (^. _1 . isoText) kvs
    vs  = map (metaValueToText . snd) kvs

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
