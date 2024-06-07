{-# LANGUAGE InstanceSigs #-}

module Data.Prim.ImageType
where

import Data.Prim.Name
       ( Name )

import Data.Prim.Prelude
       ( MonadPlus(mzero)
       , Text
       , fromMaybe
       , toLower
       , readMaybe
       , (^.)
       , iso
       , (#)
       , (.||.)
       , (&&&)
       , isEmpty
       , Map
       , Iso'
       , IsoString(..)
       , IsoText(..)
       , FromJSON(parseJSON)
       , ToJSON(toJSON)
       , AsEmpty(..)
       )

import qualified Data.Map.Strict     as M
import qualified Data.IntMap.Strict  as IM

-- ----------------------------------------

type NameMimeType = (Name, MimeType)

data MimeType
  = Application'json
  | Application'x_lightroom
  | Application'x_dxo
  | Application'x_hugin
  | Application'x_affinity_photo

  | Image'gif
  | Image'jpg
  | Image'png
  | Image'tiff

  | Image'x_adobe_dng
  | Image'x_canon_cr2        -- raw image
  | Image'x_nikon_nef        --  "    "
  | Image'x_panasonic_rw2    --  "    "

  | Image'x_portable_bitmap  -- portable *** maps
  | Image'x_portable_greymap
  | Image'x_portable_pixmap

  | Text'plain
  | Text'x_markdown

  | Video'mp4                   -- .mp4 video
  | Video'quicktime             -- .mov video
  | Video'x_gif                 -- Wackelbilder, handled as videos
  | Video'x_m4v                 -- apple .mp4 video

  | Unknown'mime_type

deriving instance Eq      MimeType
deriving instance Ord     MimeType
deriving instance Show    MimeType
deriving instance Bounded MimeType
deriving instance Enum    MimeType

instance IsoText MimeType where
  isoText :: Iso' MimeType Text
  isoText = iso mimeTextLookup mimeLookup

instance Semigroup MimeType where
  (<>) :: MimeType -> MimeType -> MimeType
  t1 <> t2
    | isEmpty t1 = t2
    | otherwise  = t1

  {-# INLINE (<>) #-}

instance Monoid MimeType where
  mempty :: MimeType
  mempty  = Unknown'mime_type
  {-# INLINE mempty #-}

instance AsEmpty MimeType

instance ToJSON MimeType where
  toJSON m = toJSON (m ^. isoText)
  {-# INLINE toJSON #-}

instance FromJSON MimeType where
  parseJSON o = (isoText #) <$> parseJSON o
  {-# INLINE parseJSON #-}

wackelGif :: MimeType
wackelGif = Video'x_gif

-- ----------------------------------------
--
-- is .jpg file

isJpgMT :: MimeType -> Bool
isJpgMT = (== Image'jpg)
{-# INLINE isJpgMT #-}

-- is .png, .gif, .tiff or other image file
isImgMT :: MimeType -> Bool
isImgMT = (`elem` [ Image'gif
                  , Image'png
                  , Image'tiff
                  , Image'x_portable_bitmap
                  , Image'x_portable_greymap
                  , Image'x_portable_pixmap
                  ])

isTiffMT :: MimeType -> Bool
isTiffMT = (== Image'tiff)
{-# INLINE isTiffMT #-}


-- is .txt or .md file
isTxtMT :: MimeType -> Bool
isTxtMT = (== Text'plain)
          .||.
          (== Text'x_markdown)
{-# INLINE isTxtMT #-}

isMovieMT :: MimeType -> Bool
isMovieMT = (`elem` [ Video'mp4
                    , Video'x_gif
                    ])
{-# INLINE isMovieMT #-}

isGifMT :: MimeType -> Bool
isGifMT = (== Image'gif)
{-# INLINE isGifMT #-}

isWackelGifMT :: MimeType -> Bool
isWackelGifMT = (== Video'x_gif)
{-# INLINE isWackelGifMT #-}

isRawMT :: MimeType -> Bool
isRawMT = (`elem` [ Image'x_canon_cr2
                  , Image'x_nikon_nef
                  , Image'x_panasonic_rw2
                  ])

-- is meta
isMetaMT :: MimeType -> Bool
isMetaMT = (== Application'x_lightroom)
{-# INLINE isMetaMT #-}

-- is other file
isOtherMT :: MimeType -> Bool
isOtherMT = (`elem` [ Application'json
                    , Application'x_affinity_photo
                    , Application'x_dxo
                    , Application'x_hugin
                    , Image'x_adobe_dng
                    , Video'quicktime
                    , Video'x_m4v
                    , Unknown'mime_type
                    ])

-- combined predicates

-- file for extracting metadata
isRawMetaMT :: MimeType -> Bool
isRawMetaMT = isRawMT
              .||.
              isMovieMT
              .||.
              isMetaMT
{-# INLINE isRawMetaMT #-}


-- part can be shown
isShowablePartMT :: MimeType -> Bool
isShowablePartMT = isJpgMT
                   .||.
                   isImgMT
                   .||.
                   isMovieMT
                   .||.
                   isTxtMT
{-# INLINE isShowablePartMT #-}


-- file, that is stored as part of an image entry
isAnImgPartMT :: MimeType -> Bool
isAnImgPartMT = isRawMetaMT
                .||.
                isShowablePartMT
{-# INLINE isAnImgPartMT #-}

-- part of an image entry which can be shown in a collection
-- or a raw image, no meta files
isShowablePartOrRawMT :: MimeType -> Bool
isShowablePartOrRawMT = isShowablePartMT
                        .||.
                        isRawMT
{-# INLINE isShowablePartOrRawMT #-}

hasSizeMT :: MimeType -> Bool
hasSizeMT =  isJpgMT
             .||.
             isImgMT
             .||.
             isMovieMT
             .||.
             isRawMT

-- files which are ignored when syncing with filesystem
isBoringMT :: MimeType -> Bool
isBoringMT = isEmpty
{-# INLINE isBoringMT #-}

-- ----------------------------------------

imgMimeExt' :: [(MimeType, [String])]
imgMimeExt' =
  [ (Application'json,             [".json"])
  , (Application'x_lightroom,      [".xmp"])
  , (Application'x_dxo,            [".dop", ".dxo"])
  , (Application'x_hugin,          [".pto"])
  , (Application'x_affinity_photo, [".aphoto"])
  , (Image'gif,                    [".gif"])
  , (Image'jpg,                    [".jpg", ".jpeg"])
  , (Image'png,                    [".png"])
  , (Image'tiff,                   [".tiff", ".tif"])
  , (Image'x_adobe_dng,            [".dng"])
  , (Image'x_canon_cr2,            [".cr2"])
  , (Image'x_nikon_nef,            [".nef"])
  , (Image'x_panasonic_rw2,        [".rw2"])
  , (Image'x_portable_bitmap,      [".pbm"])
  , (Image'x_portable_greymap,     [".pgm"])
  , (Image'x_portable_pixmap,      [".ppm"])
  , (Text'plain,                   [".txt"])
  , (Text'x_markdown,              [".md"])
  , (Video'mp4,                    [".mp4"])
  , (Video'quicktime,              [".mov"])
  , (Video'x_m4v,                  [".m4v"])
  , (Video'x_gif,                  []) -- in MetaData: img/gif --> video/x-gif
  , (Unknown'mime_type,            [])
  ]

-- --------------------
--
-- aux ops for conversion to/from Text

mimeLookup :: Text -> MimeType
mimeLookup t =
  fromMaybe Unknown'mime_type $
  M.lookup t mimeTable

mimeTable :: Map Text MimeType
mimeTable =
  M.fromList $  map (mt2txt &&& id) [minBound .. maxBound]

mimeTextLookup :: MimeType -> Text
mimeTextLookup m =
  fromMaybe mempty $ IM.lookup (fromEnum m) mimeTextTable

mimeTextTable :: IM.IntMap Text
mimeTextTable =
  IM.fromList $ map (fromEnum &&& mt2txt) [minBound .. maxBound]

mt2txt :: MimeType -> Text
mt2txt =
  (^.isoText) . map tos . show
  where
    tos c
      | c == '\'' = '/'
      | c == '_'  = '-'
      | otherwise = toLower c

-- ----------------------------------------
--
-- the old ImgType variant

type NameImgType = (Name, ImgType)

data ImgType =
  IMGraw    | IMGmeta   | IMGjpg | IMGimg |
  IMGimgdir | IMGjpgdir |
  IMGtxt    | IMGmovie  |
  IMGother  | IMGboring

deriving instance Eq   ImgType
deriving instance Ord  ImgType
deriving instance Show ImgType
deriving instance Read ImgType

instance IsoString ImgType where
  isoString = iso show (fromMaybe IMGboring . readMaybe)

instance IsoText ImgType

instance ToJSON ImgType where
  toJSON = toJSON . show
  {-# INLINE toJSON #-}

instance FromJSON ImgType where
  parseJSON o = parseJSON o >>= maybe mzero return
  {-# INLINE parseJSON #-}

instance Semigroup ImgType where
  (<>) :: ImgType -> ImgType -> ImgType
  t1 <> t2
    | isEmpty t1 = t2
    | otherwise  = t1

  {-# INLINE (<>) #-}

instance Monoid ImgType where
  mempty :: ImgType
  mempty  = IMGboring
  {-# INLINE mempty #-}

instance AsEmpty ImgType

-- {-
-- is .jpg file
isJpg :: ImgType -> Bool
isJpg = isJpgMT . it2mt

-- is .png, .gif, .tiff or other image file
isImg :: ImgType -> Bool
isImg = isImgMT . it2mt

-- is .txt or .md file
isTxt :: ImgType -> Bool
isTxt = isTxtMT . it2mt

isMovie :: ImgType -> Bool
isMovie = isMovieMT . it2mt

isRaw :: ImgType -> Bool
isRaw = isRawMT . it2mt

-- is other file
isOther :: ImgType -> Bool
isOther = isOtherMT . it2mt

-- file for extracting metadata
isRawMeta :: ImgType -> Bool
isRawMeta = isRawMetaMT . it2mt

-- file, that is stored as part of an image entry
isAnImgPart :: ImgType -> Bool
isAnImgPart = isAnImgPartMT . it2mt

isShowablePartOrRaw :: ImgType -> Bool
isShowablePartOrRaw = isShowablePartOrRawMT . it2mt

-- part can be shown
isShowablePart :: ImgType -> Bool
isShowablePart = isShowablePartMT . it2mt

-- is .xmp file with meta data from Lightroom (e.g. GPS or rating)
isMeta :: ImgType -> Bool
isMeta = isMetaMT . it2mt

-- files which are ignored when syncing with filesystem
isBoring :: ImgType -> Bool
isBoring = isBoringMT . it2mt

-- ----------------------------------------

it2mt :: ImgType -> MimeType
it2mt IMGraw    = Image'x_nikon_nef
it2mt IMGmeta   = Application'x_lightroom
it2mt IMGjpg    = Image'jpg
it2mt IMGimg    = Image'png
it2mt IMGtxt    = Text'x_markdown
it2mt IMGmovie  = Video'mp4
it2mt IMGother  = Application'x_dxo
it2mt IMGboring = Unknown'mime_type
it2mt _         = Unknown'mime_type

mt2it :: MimeType -> ImgType
mt2it Application'json             = IMGother
mt2it Application'x_lightroom      = IMGmeta
mt2it Application'x_dxo            = IMGother
mt2it Application'x_hugin          = IMGother
mt2it Application'x_affinity_photo = IMGother
mt2it Image'gif                    = IMGimg
mt2it Image'jpg                    = IMGjpg
mt2it Image'png                    = IMGimg
mt2it Image'tiff                   = IMGimg
mt2it Image'x_adobe_dng            = IMGother
mt2it Image'x_canon_cr2            = IMGraw
mt2it Image'x_nikon_nef            = IMGraw
mt2it Image'x_panasonic_rw2        = IMGraw
mt2it Image'x_portable_bitmap      = IMGimg
mt2it Image'x_portable_greymap     = IMGimg
mt2it Image'x_portable_pixmap      = IMGimg
mt2it Text'plain                   = IMGtxt
mt2it Text'x_markdown              = IMGtxt
mt2it Video'mp4                    = IMGmovie
mt2it Video'quicktime              = IMGother
mt2it Video'x_m4v                  = IMGother
mt2it Video'x_gif                  = IMGimg
mt2it Unknown'mime_type            = IMGboring

isoMimeType :: Iso' MimeType ImgType
isoMimeType = iso mt2it it2mt
-- -}
-- ----------------------------------------
