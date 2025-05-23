{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE InstanceSigs #-}

-- ----------------------------------------

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
  , metaGPS
  , metaImgType
  , metaMimeType
  , metaName
  , metaTimeStamp

  , theImgEXIFUpdate

  , addFileMetaData
  , addMetaGPSurl
  , editMetaData
  , splitMDT
  , colMDT
  , filterByImgType
  , filterKeysMetaData
  , splitMetaData
  , normMetaData
  , cleanupMetaData
  , cleanupOldMetaData
  , cleanupOutdatedMeta

  , someKeysMetaData
  , globKeysMetaData
  , allKeysMetaData
  , prettyMetaData

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
  , lookupMimeType
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
  , descrTitle1
  , descrSubtitle
  , descrTitleEnglish
  , descrTitleLatin
  , descrAddress
  , descrLocation
  , descrKeywords
  , descrWeb
  , descrWikipedia
  , descrAudio
  , descrGoogleMaps
  , descrComment
  , descrCommentImg
  , descrCreateDate
  , descrOrderedBy
  , descrAccess
  , descrDuration
  , descrRating
  , descrRatingImg
  , descrGPSAltitude
  , descrGPSPosition
  , descrGPSPositionDeg
  , descrGPSurl
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
  , exifISO
  , exifMake
  , exifMaxApertureValue
  , exifMeteringMode
  , exifModel
  , exifOrientation
  , exifUserComment
  , exifWhiteBalance

  , fileCheckSum
  , fileFileSize
  , fileImgType
  , fileMimeType
  , fileFileModifyDate
  , fileName
  , fileDirName
  , fileRefImg
  , fileRefJpg
  , fileRefMedia
  , fileRefRaw
  , fileDateTime
  , fileTimeStamp

  , gifAnimationIterations
  , gifDuration
  , gifFrameCount

  , imgEXIFUpdate
  , imgNameRaw

  , makerNotesColorSpace
  , makerNotesDaylightSavings
  , makerNotesFocusDistance
  , makerNotesFocusMode
  , makerNotesQuality
  , makerNotesSerialNumber
  , makerNotesShootingMode
  , makerNotesShutterCount
  , makerNotesTimeZone

  , quickTimeCreateDate
  , quickTimeDuration
  , quickTimeImageWidth
  , quickTimeImageHeight
  , quickTimeVideoFrameRate

  , xmpGPSAltitude
  , xmpRating
  )
where

import Data.Prim
       ( CheckSum
       , Name
       , Path
       , TimeStamp
       , ImgType
       , MimeType
       , Geo
       , GPSposDec
       , initPath
       , tailPath
       , snocPath
       , isGifMT
       , isImgMT
       , isJpgMT
       , isMetaMT
       , isMovieMT
       , isRawMT
       , isShowablePartOrRawMT
       , isTxtMT
       , wackelGif
       , geo'org
       , readGeo''
       , googleMapsGPSdec
       , isoDegDec
       )
import Data.Prim.Prelude

import Data.Access
       ( Access
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
import Data.Bits
       ( (.|.)
       , (.&.)
       , complement
       )
import Data.HashMap.Strict
       ( HashMap )
import Text.Printf
       ( printf )
import Text.SimpleParser
       (SP
       , anyString
       , char
       , count
       , digitChar
       , matchP
       , oneOf'
       , parseMaybe
       , spaceChar
       )

import qualified Data.Aeson          as J
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict  as IM
import qualified Data.List           as L
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.Text.Fill      as T
import qualified Data.Vector         as V
import qualified Text.SimpleParser   as SP


-- import Debug.Trace

-- --------------------

newtype MetaData' a = MD (IM.IntMap a)

type    MetaData    = MetaData' MetaValue
type    MetaDataT   = MetaData' Text

newtype MetaDataText = MDT (M.Map Text Text)

data MetaKey
  = Composite'Aperture
  | Composite'AutoFocus
  | Composite'CircleOfConfusion
  | Composite'DOF
  | Composite'FOV
  | Composite'Flash
  | Composite'FocalLength35efl
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
  | Descr'Address
  | Descr'Audio
  | Descr'CatalogVersion
  | Descr'CatalogWrite
  | Descr'Comment
  | Descr'CommentImg
  | Descr'CreateDate
  | Descr'Duration
  | Descr'GPSAltitude
  | Descr'GPSPosition
  | Descr'GPSPositionDeg
  | Descr'GPSurl
  | Descr'GoogleMaps
  | Descr'Keywords
  | Descr'Location
  | Descr'OrderedBy
  | Descr'Rating
  | Descr'RatingImg
  | Descr'Subtitle
  | Descr'Title
  | Descr'Title1
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
  | EXIF'Flash
  | EXIF'FNumber
  | EXIF'FocalLength
  | EXIF'FocalLengthIn35mmFormat
  | EXIF'ISO
  | EXIF'Make
  | EXIF'MaxApertureValue
  | EXIF'MeteringMode
  | EXIF'Model
  | EXIF'Orientation
  | EXIF'UserComment
  | EXIF'WhiteBalance
  | File'CheckSum
  | File'FileSize
  | File'ImgType
  | File'MimeType
  | File'FileModifyDate
  | File'Name
  | File'DirName
  | File'RefImg
  | File'RefJpg
  | File'RefMedia
  | File'RefRaw
  | File'DateTime
  | File'TimeStamp
  | GIF'AnimationIterations
  | GIF'Duration
  | GIF'FrameCount
  | Img'EXIFUpdate
  | Img'NameRaw
  | MakerNotes'ColorSpace
  | MakerNotes'DaylightSavings
  | MakerNotes'FocusDistance
  | MakerNotes'FocusMode
  | MakerNotes'Quality
  | MakerNotes'SerialNumber
  | MakerNotes'ShootingMode
  | MakerNotes'ShutterCount
  | MakerNotes'TimeZone
  | QuickTime'CreateDate
  | QuickTime'Duration
  | QuickTime'ImageHeight
  | QuickTime'ImageWidth
  | QuickTime'VideoFrameRate
  | XMP'GPSAltitude
  | XMP'Rating
  | Key'Unknown          -- must be the last value

type MetaKeySet = MetaKey -> Bool

data MetaValue
  = MText !Text
  | MTSet ![Text]         -- list of text
  | MNm   !Name
  | MInt  !Int
  | MRat  !Int            -- rating: 0..5
  | MOri  !Int            -- orientation: 0..3 <-> 0, 90, 180, 270 degrees CW
  | MAcc  !Access         -- access restrictions
  | MTs   !TimeStamp      -- time stamp
  | MGps  !GPSposDec      -- GPS coordinate
  | MCS   !CheckSum       -- checksum
  | MTyp  !ImgType        -- image (or file) type
  | MMime !MimeType       -- image mime type
  | MNull                 -- missing or default value

-- invariant for MetaValue:
-- isEmpty mv => mv == MNull
--
-- the invariant will be guaranteed by the setter parts
-- of "meta***" iso's

type Rating = Int -- 0 .. 5

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
keysAttrComposite@[
  compositeAperture
  , compositeAutoFocus
  , compositeCircleOfConfusion
  , compositeDOF
  , compositeFOV
  , compositeFlash
  , compositeFocalLength35efl
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
  , descrTitle1
  , descrSubtitle
  , descrTitleEnglish
  , descrTitleLatin
  , descrAddress
  , descrLocation
  , descrKeywords
  , descrWeb
  , descrWikipedia
  , descrAudio
  , descrGoogleMaps
  , descrComment
  , descrCommentImg
  , descrCreateDate
  , descrOrderedBy
  , descrAccess
  , descrDuration
  , descrRating
  , descrRatingImg
  , descrGPSAltitude
  , descrGPSPosition
  , descrGPSPositionDeg
  , descrGPSurl
  , descrCatalogVersion
  , descrCatalogWrite :: MetaKey

keysAttrDescr :: [MetaKey]
keysAttrDescr@[
  descrAccess
  , descrAddress
  , descrAudio
  , descrCatalogVersion
  , descrCatalogWrite
  , descrComment
  , descrCommentImg
  , descrCreateDate
  , descrDuration
  , descrGPSAltitude
  , descrGPSPosition
  , descrGPSPositionDeg
  , descrGPSurl
  , descrGoogleMaps
  , descrKeywords
  , descrLocation
  , descrOrderedBy
  , descrRating
  , descrRatingImg
  , descrSubtitle
  , descrTitle
  , descrTitle1
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
  , exifISO
  , exifMake
  , exifMaxApertureValue
  , exifMeteringMode
  , exifModel
  , exifOrientation
  , exifUserComment
  , exifWhiteBalance :: MetaKey

keysAttrExif :: [MetaKey]
keysAttrExif@[
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
  , fileFileSize
  , fileImgType
  , fileMimeType
  , fileFileModifyDate
  , fileName
  , fileDirName
  , fileRefImg
  , fileRefJpg
  , fileRefMedia
  , fileRefRaw
  , fileDateTime
  , fileTimeStamp :: MetaKey

keysAttrFile :: [MetaKey]
keysAttrFile@[
  fileCheckSum
  , fileFileSize
  , fileImgType
  , fileMimeType
  , fileFileModifyDate
  , fileName
  , fileDirName
  , fileRefImg
  , fileRefJpg
  , fileRefMedia
  , fileRefRaw
  , fileDateTime
  , fileTimeStamp
  ] = [File'CheckSum .. File'TimeStamp]

gifAnimationIterations
  , gifDuration
  , gifFrameCount :: MetaKey

keysAttrGIF :: [MetaKey]
keysAttrGIF@[
  gifAnimationIterations
  , gifDuration
  , gifFrameCount
  ] = [GIF'AnimationIterations .. GIF'FrameCount]

imgEXIFUpdate
  , imgNameRaw :: MetaKey

keysAttrImg :: [MetaKey]
keysAttrImg@[
  imgEXIFUpdate
  , imgNameRaw
  ] = [Img'EXIFUpdate .. Img'NameRaw]

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
keysAttrMaker@[
  makerNotesColorSpace
  , makerNotesDaylightSavings
  , makerNotesFocusDistance
  , makerNotesFocusMode
  , makerNotesQuality
  , makerNotesSerialNumber
  , makerNotesShootingMode
  , makerNotesShutterCount
  , makerNotesTimeZone
  ] = [MakerNotes'ColorSpace .. MakerNotes'TimeZone]

quickTimeCreateDate
  , quickTimeDuration
  , quickTimeImageWidth
  , quickTimeImageHeight
  , quickTimeVideoFrameRate :: MetaKey

keysAttrQuickTime :: [MetaKey]
keysAttrQuickTime@[
  quickTimeCreateDate
  , quickTimeDuration
  , quickTimeImageWidth
  , quickTimeImageHeight
  , quickTimeVideoFrameRate
  ] = [QuickTime'CreateDate .. QuickTime'VideoFrameRate]

xmpGPSAltitude
  , xmpRating :: MetaKey

keysAttrXmp :: [MetaKey]
keysAttrXmp@[
  xmpGPSAltitude
  , xmpRating
  ] = [XMP'GPSAltitude .. XMP'Rating]

-- ----------------------------------------
--
-- instances and basic functions for MetaData

instance (AsEmpty a, IsoValueText a) => FromJSON (MetaData' a) where
  parseJSON o = (isoMetaDataMDT #) <$> parseJSON o

instance (AsEmpty a, IsoValueText a) => ToJSON (MetaData' a) where
  toJSON m = toJSON $ m ^. isoMetaDataMDT

deriving instance Eq   a => Eq   (MetaData' a)   -- used in Catalog.MetaData.Exif
deriving instance Show a => Show (MetaData' a)

instance AsEmpty (MetaData' a) where
  _Empty = nearly (MD IM.empty) (\ (MD m) -> IM.null m)

instance Semigroup a => Semigroup (MetaData' a) where
  (<>) = unionMD

instance Semigroup a => Monoid (MetaData' a) where
  mempty = MD IM.empty

-- conversions to/from MetaDataText

class IsoValueText v where
  isoValueText :: MetaKey -> Iso' v Text

instance IsoValueText MetaValue where
  isoValueText = isoMetaValueText

instance IsoValueText Text where
  isoValueText _k = iso id id

isoMetaDataMDT :: (AsEmpty a, IsoValueText a)
               => Iso' (MetaData' a) MetaDataText
isoMetaDataMDT = iso toMDT frMDT
  where
    toMDT (MD m) = MDT m'
      where
        m' = IM.foldrWithKey' ins M.empty m
          where
            ins k0 v
              | isEmpty k = id
              | otherwise = M.insert (metaKeyToText k) (v ^. isoValueText k)
              where
                k = toEnum k0

    frMDT (MDT m) = md
      where
        md = M.foldrWithKey' ins (MD mempty) m
          where
            ins k0 v0 = insertMD k v
              where
                k = metaKeyLookup k0
                v = isoValueText k # v0

--------------------
--
-- basic MetaData ops and lenses


-- lens combining insertMD and lookupMD

metaDataAt :: MetaKey -> Lens' MetaData MetaValue
metaDataAt mk k mt = (\ v -> insertMD mk v mt) <$> k (lookupMD mk mt)

-- lens with conversion of MetaValue to Text

metaTextAt :: MetaKey -> Lens' MetaData Text
metaTextAt k = metaDataAt k . isoMetaValueText k

addFileMetaData :: Path -> Name -> MetaData -> MetaData
addFileMetaData p nm md =
  md
  & metaTextAt fileDirName .~ dirp ^. isoText
  & metaTextAt fileRefImg  .~ imgp ^. isoText
  & metaTextAt fileRefRaw  .~ rawp ^. isoText
  where
    p'   = tailPath p              -- remove /archive
    dirp = initPath p'
    imgp = snocPath dirp nm
    rnm  = md ^. metaDataAt imgNameRaw . metaName
    rawp | isEmpty rnm = mempty
         | otherwise   = snocPath dirp rnm

insertMD :: AsEmpty a => MetaKey -> a -> MetaData' a -> MetaData' a
insertMD k v mt@(MD m)
  | isEmpty k   = mt                               -- no redundant stuff in metatable
  | isEmpty v   = MD $ IM.delete (fromEnum k)   m  -- dto
  | otherwise   = MD $ IM.insert (fromEnum k) v m

lookupMD :: Monoid a => MetaKey -> MetaData' a -> a
lookupMD k (MD m) = fromMaybe mempty $ IM.lookup (fromEnum k) m

-- <> for meta tables
unionMD :: Semigroup a => MetaData' a -> MetaData' a -> MetaData' a
unionMD (MD m1) (MD m2) = MD $ IM.unionWith (<>) m1 m2

toListMD :: MetaData' a -> [(MetaKey, a)]
toListMD (MD m) = map (first toEnum) $ IM.toAscList m

filterKeysMetaData :: MetaKeySet -> MetaData' a -> MetaData' a
filterKeysMetaData p (MD m) = MD $ IM.filterWithKey (\ i _v -> p $ toEnum i) m

partitionMD :: MetaKeySet -> MetaData' a -> (MetaData' a, MetaData' a)
partitionMD ks (MD m) = (MD m1, MD m2)
  where
    (m1, m2) = IM.partitionWithKey (\ i _v -> ks $ toEnum i) m

-- --------------------
--
-- instances for MetaDataText

instance FromJSON MetaDataText where
  parseJSON j =
    ( fmap MDT . parseJSON          -- parse a {...} as Map Text Text
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
-- instances and basic ops for MetaValue

deriving instance Eq   MetaValue
deriving instance Show MetaValue

instance AsEmpty MetaValue where
  _Empty :: Prism' MetaValue ()
  _Empty = nearly MNull ise
    where
      ise (MText t)  = isEmpty t
      ise (MTSet ws) = null ws
      ise (MNm n)    = isEmpty n
      ise (MInt _)   = False -- no default value for Int
      ise (MOri o)   = o == 0
      ise (MRat r)   = r == 0
      ise (MAcc a)   = a == no'restr
      ise (MTs t)    = t == mempty
      ise (MCS cs)   = isEmpty cs
      ise (MTyp t)   = isEmpty t
      ise (MMime m)  = isEmpty m
      ise (MGps _)   = False -- no default value for GPS data
      ise MNull      = True

instance Semigroup MetaValue where
  (<>) :: MetaValue -> MetaValue -> MetaValue
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
  mv1@(MMime _)  <> MMime _  = mv1     -- 1. wins
  MTs   t1       <> MTs   t2 = MTs   $ t1 <> t2       -- newest wins
  MTSet w1       <> MTSet w2 = MTSet $ unionTS w1 w2  -- union of words

  _              <> mv2      = mv2     -- mixing types, no effect

instance Monoid MetaValue where
  mempty :: MetaValue
  mempty = MNull

-- MetaKey determines the MetaValue representation
-- no instance of FromJSON, due to decoding dependency on the key
--
-- getter and setter for MataValue

-- --------------------
--
-- mapping of MetaValue variants to MetaKey's

isoMetaValueText :: MetaKey -> Iso' MetaValue Text
isoMetaValueText k = case k of
  Composite'GPSPosition -> metaGPSDecText
  Descr'Access          -> metaAccess    . isoAccText
  Descr'GPSPosition     -> metaGPSDecText
  Descr'Keywords        -> metaTS        . isoKeywText
  Descr'Rating          -> metaRatingText
  Descr'RatingImg       -> metaRatingText
  Descr'Web             -> metaTS        . isoTSetText
  EXIF'Orientation      -> metaOri       . isoOriText
  File'CheckSum         -> metaCheckSum  . isoText
  File'ImgType          -> metaImgType   . isoText
  File'MimeType         -> metaMimeType  . isoText
  File'Name             -> metaName      . isoText
  File'TimeStamp        -> metaTimeStamp . isoText
  Img'NameRaw           -> metaName      . isoText
  Img'EXIFUpdate        -> metaTimeStamp . isoText
  QuickTime'ImageHeight -> metaIntText
  QuickTime'ImageWidth  -> metaIntText
  XMP'Rating            -> metaRatingText
  Key'Unknown           -> iso (const mempty) (const mempty)
  _                     -> metaText

-- --------------------

metaText :: Iso' MetaValue Text
metaText = iso
  (\ case
      MText t -> t
      _       -> mempty
  )
  (\ t -> if isEmpty t
          then mempty
          else MText t
  )
{-# INLINE metaText #-}

metaTS :: Iso' MetaValue [Text]
metaTS =
  iso
    ( \case
        MTSet kw -> kw
        _ -> []
    )
    ( \ws ->
        if null ws
          then mempty
          else MTSet ws
    )
{-# INLINE metaTS #-}

metaName :: Iso' MetaValue Name
metaName = iso
  (\ case
      MNm n -> n
      _     -> mempty
  )
  (\ n -> if isEmpty n
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
  (\ t -> if isEmpty t
          then mempty
          else MTs t
  )
{-# INLINE metaTimeStamp #-}

metaGPS :: Iso' MetaValue (Maybe GPSposDec)  -- no default for GPS
metaGPS = iso
  (\ case
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
  (\ c -> if isEmpty c
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
  (\ t -> if isEmpty t
          then mempty
          else MTyp t
  )
{-# INLINE metaImgType #-}

metaMimeType :: Iso' MetaValue MimeType
metaMimeType = iso
  (\ case
      MMime m -> m
      _       -> mempty
  )
  (\ t -> if isEmpty t
          then mempty
          else MMime t
  )
{-# INLINE metaMimeType #-}

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
        toS = maybe mempty (& (ppString #))
        frS = (^? googleMapsGPSdec)

metaGPSDegText :: Iso' MetaValue Text
metaGPSDegText = metaGPS . isoGPSDeg . isoText
  where
    isoGPSDeg :: Iso' (Maybe GPSposDec) String
    isoGPSDeg = iso toS frS
      where
        toS v = maybe mempty (\ p -> ppString # (isoDegDec # p)) v
        frS s = (^. isoDegDec) <$> (s ^? ppString)

-- ----------------------------------------
--
-- split metadata in image metadata and part specific metadata

splitMetaData :: MimeType -> MetaData' a -> (MetaData' a, MetaData' a)
splitMetaData ty = partitionMD (not <$> snd (keysByMimeType ty))

-- filter relevant metadata from exiftool output
cleanupMetaData :: MimeType -> MetaData' a -> MetaData' a
cleanupMetaData ty =
  filterKeysMetaData (i'keys .||. p'keys)  -- all interesting stuff
  where
    (i'keys, p'keys) = keysByMimeType ty

cleanupOldMetaData :: MetaData' a -> MetaData' a
cleanupOldMetaData = filterKeysMetaData (`elem` keysAttrDescr)

cleanupOutdatedMeta :: MetaData -> MetaData
cleanupOutdatedMeta =
  filterKeysMetaData (not . (`elem` dateKeys))
  where
    -- remove all timestamps from meta
    -- those are recomputed during exif update
    dateKeys =
      [ exifCreateDate
      , fileFileModifyDate
      , fileDateTime
      , fileTimeStamp
      , imgEXIFUpdate
      , quickTimeCreateDate
      ]

normMetaData :: MimeType -> MetaData -> MetaData
normMetaData ty md
  | isRawMT  ty = md
                  & cpy fileName       imgNameRaw
                  -- add raw filename to img metadata

  | isMetaMT ty = md
                  & ins xmpGPSAltitude descrGPSAltitude
                  & cpy xmpRating      descrRatingImg
                  -- set the GPS position and altitute
                  -- of the img to the pos in .xmp from Lightroom
                  -- set img rating from LR

  | isGifMT ty && not (isEmpty anim)
                = md
                  & metaDataAt fileMimeType . metaMimeType .~ wackelGif
                  -- when animated gif
                  -- change MIME type to video/x-gif
                  -- animated gifs are handled similar to videos
                  -- (no conversion to .jpg)

  | otherwise   = md
                  & remqt

  where
    -- overwrite dst
    cpy src dst md' = md' & metaTextAt dst .~ (md' ^. metaTextAt src)

    -- insert, when not already set
    ins src dst md' = md' <> (mempty & metaTextAt dst .~ (md' ^. metaTextAt src))

    -- animation iterations metadata, used for checking wackelgif
    anim = md ^. metaTextAt gifAnimationIterations

    -- rem empty quicktime create date
    -- (QuickTime:CreateDate 0000:00:00 00:00:00 is useless)
    remqt md' = md' & metaTextAt quickTimeCreateDate %~ notEmptyDate
      where
        notEmptyDate d
          | "0000" `T.isPrefixOf` d = mempty
          | otherwise               = d

addMetaGPSurl :: MetaData -> MetaData
addMetaGPSurl md =
  md & metaTextAt descrGPSurl         .~ (gps & isoString . googleMapsGPSdec %~ id)
     & metaTextAt descrGPSPositionDeg .~ gps
  where
    gps = lookupGPSposDeg md

-- --------------------

editMetaData :: MetaDataT -> MetaData -> MetaData
editMetaData (MD m) mt = IM.foldlWithKey' ins mt m
  where
    ins acc k0 v0
      | v0 == "-" =                      -- remove key from metadata
          acc & metaDataAt k .~ mempty

      | k == descrKeywords =             -- add and remove keywords (',' separated)
          acc & metaDataAt k . metaTS %~ mergeTS (isoKeywText # v0)

      | k == descrWeb =                  -- add and remove URLs     ('|' separated)
          acc & metaDataAt k . metaTS %~ mergeTS (isoTSetText # v0)

      | otherwise =                      -- insert or set new value
          acc & metaDataAt k .~ isoMetaValueText k # v0
      where
        k = toEnum k0

splitMDT :: MetaDataText -> (MetaDataT, MetaDataT)
splitMDT mdt = splitMetaData mempty (isoMetaDataMDT # mdt)

colMDT :: MetaDataText -> MetaDataT
colMDT mdt = filterKeysMetaData (`elem` keysAttrDescr) (isoMetaDataMDT # mdt)


-- --------------------

keysByMimeType :: MimeType -> (MetaKeySet, MetaKeySet)
keysByMimeType ty
  | isJpgMT ty
    ||
    isImgMT   ty = (ks'cexm,  ks'part)
  | isMovieMT ty = (ks'cexmq, ks'part)
  | isMetaMT  ty = (ks'gpsr,  ks'file .||. ks'gps)
  | isTxtMT   ty = (ks'descr, ks'file)
  | isRawMT   ty = (ks'cexm,  ks'part)
  | otherwise    = (ks'all,   ks'part)
  where
    ks'all   = const True
    ks'part  = ks'file
               .||. ks'gif
               .||. ks'geo
               .||. ks'rat
               .||. ks'cpi

    ks'comp  = (`elem` keysAttrComposite)
    ks'descr = (`elem` keysAttrDescr)
    ks'exif  = (`elem` keysAttrExif)
    ks'file  = (`elem` keysAttrFile)
    ks'gif   = (`elem` keysAttrGIF)
    ks'maker = (`elem` keysAttrMaker)
    ks'qtime = (`elem` keysAttrQuickTime)

    ks'cexm  = ks'comp
               .||. ks'descr
               .||. ks'exif
               .||. ks'maker
               .||. (== imgNameRaw)

    ks'cexmq = ks'cexmq .||. ks'qtime
    ks'gps   = (`elem` [ compositeGPSPosition
                       , descrGPSAltitude
                       , descrGPSPosition
                       , xmpGPSAltitude
                       ]
               )
    ks'rat   = (== descrRating)
    ks'cpi   = (== descrCommentImg)    -- comment for a copy

    ks'gpsr  = ks'gps                  -- gps data in .xmp files
               .||. (== xmpRating)     -- rating in .xmp files

    ks'geo   = (`elem` [ compositeImageSize
                       , compositeMegapixels
                       , exifOrientation
                       , makerNotesColorSpace
                       ]
               )

-- ----------------------------------------
--
-- filter meta data entries by image type

filterByImgType :: MimeType -> MetaData' a -> MetaData' a
filterByImgType ty =
  filterKeysMetaData (`elem` ks)
  where
    ks | isShowablePartOrRawMT ty = ksRaw
       | isMetaMT              ty = ksXmp
       | otherwise                = mzero

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
  mconcat $ map (`lookupMD` mt) ns

-- access the create date of an image
-- usually formated as YYYY:MM:DD hh:mm:ss(.ss)

lookupCreate :: (Text -> res) -> MetaData -> res
lookupCreate p mt = p (v ^. isoMetaValueText exifCreateDate)
  where
    v = lookupByKeys
        [ compositeSubSecCreateDate
        , exifCreateDate
        , quickTimeCreateDate
        , fileFileModifyDate
        ] mt

-- access image geometry and orientation
lookupGeoOri :: MetaData -> (Geo, Int)
lookupGeoOri = lookupGeo &&& lookupOri

lookupGeo :: MetaData -> Geo
lookupGeo mt =
  toGeo $ mt ^. metaTextAt compositeImageSize
  where
    toGeo sz = fromMaybe geo'org (readGeo'' $ sz ^. isoString)

lookupMimeType :: MetaData -> MimeType
lookupMimeType md = md ^. metaDataAt fileMimeType . metaMimeType

lookupRating :: MetaData -> Rating
lookupRating mt =
  lookupByKeys
  [ descrRating     -- descr:Rating has priority over
  , descrRatingImg  -- rating from LR valid for all parts
  ] mt ^. metaRating

mkRating :: Rating -> MetaData -> MetaData
mkRating r md = md & metaDataAt descrRating .~ metaRating # r

lookupOri :: MetaData -> Int
lookupOri mt = mt ^. metaDataAt exifOrientation . metaOri

lookupGPSposDeg :: MetaData -> Text
lookupGPSposDeg =
  (^. metaGPSDegText) . lookupByKeys [descrGPSPosition, compositeGPSPosition]

theImgEXIFUpdate :: Lens' MetaData TimeStamp
theImgEXIFUpdate = metaDataAt imgEXIFUpdate . metaTimeStamp
{-# INLINE theImgEXIFUpdate #-}

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
-- auxiliary iso's for conversion of MetaData <-> Text

isoKeywText :: Iso' [Text] Text
isoKeywText = isoListText ((== ','), ", ")     -- comma separated list for keywords
{-# INLINE isoKeywText #-}

isoTSetText :: Iso' [Text] Text
isoTSetText = isoListText ((== '|'), " | ")   -- '|' separated list (for URLs)
{-# INLINE isoTSetText #-}


isoListText :: (Char -> Bool, Text) -> Iso' [Text] Text
isoListText (del, sep)= iso toT frT
  where
    toT :: [Text] -> Text
    toT = T.intercalate sep

    frT :: Text -> [Text]
    frT =
      filter (not . T.null)         -- remove empty words
      .
      map (T.unwords . T.words)     -- normalize whitespace
      .
      T.split del                   -- split at delimiter char
{-# INLINE isoListText #-}

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

unionTS :: [Text] -> [Text] -> [Text]
unionTS ws1 ws2 = nub (ws1 ++ ws2)
{-# INLINE unionTS #-}

-- used when editing metadata: add/rem keywords
mergeTS :: [Text] -> [Text] -> [Text]
mergeTS ws1 ws2 = ws
  where
    ws         = nub ins L.\\ rmv
    (rmv, ins) = partTS $ ws1 ++ ws2

    -- partition keywords in (to be removed, added)
    partTS =
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
    isoStars' = iso (`replicate` '*')
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
  isoText :: Iso' MetaKey Text
  isoText = iso metaKeyTextLookup metaKeyLookup
  {-# INLINE isoText #-}

instance AsEmpty MetaKey where
  _Empty :: Prism' MetaKey ()
  _Empty = only Key'Unknown
  {-# INLINE _Empty #-}

metaKeyLookup :: Text -> MetaKey
metaKeyLookup t =
  fromMaybe Key'Unknown $
  HM.lookup (T.toLower t) metaKeyLookupTable
{-# INLINE metaKeyLookup #-}

metaKeyTextLookup :: MetaKey -> Text
metaKeyTextLookup k =
  fromMaybe mempty $ IM.lookup (fromEnum k) metaKeyToTextTable

-- --------------------

allKeysMetaData :: [MetaKey]
allKeysMetaData = [minBound .. pred maxBound]

someKeysMetaData :: (Text -> Bool) -> [MetaKey]
someKeysMetaData p = filter (p . metaKeyToText) allKeysMetaData

globKeysMetaData :: SP String -> [MetaKey]
globKeysMetaData gp = someKeysMetaData p
  where
    p t = matchP gp (t ^. isoString)

prettyMetaData :: MetaData -> [Text]
prettyMetaData mt = zipWith (<:>) ks vs
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
