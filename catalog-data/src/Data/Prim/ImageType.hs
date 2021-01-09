{-# LANGUAGE StandaloneDeriving #-}

module Data.Prim.ImageType
where

import           Data.Prim.Name
import           Data.Prim.Prelude

-- ----------------------------------------

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
  t1 <> t2
    | isempty t1 = t2
    | otherwise  = t1

instance Monoid ImgType where
  mempty  = IMGboring
  mappend = (<>)

instance IsEmpty ImgType where
  isempty = (== mempty)

-- is .jpg file
isJpg :: ImgType -> Bool
isJpg IMGjpg = True
isJpg _      = False

-- is .png, .gif, .tiff or other image file
isImg :: ImgType -> Bool
isImg IMGimg = True
isImg _      = False

-- is .txt or .md file
isTxt :: ImgType -> Bool
isTxt IMGtxt = True
isTxt _      = False

-- is other file
isOther :: ImgType -> Bool
isOther IMGother  = True
isOther IMGboring = True
isOther _         = False

-- is sub dir with images developed as .jpg
isJpgSubDir :: ImgType -> Bool
isJpgSubDir IMGjpgdir = True
isJpgSubDir _         = False

-- is subdir to be traveresd for more images
isImgSubDir :: ImgType -> Bool
isImgSubDir IMGimgdir = True
isImgSubDir _         = False

-- file for extracting metadata
isRawMeta :: ImgType -> Bool
isRawMeta ty = ty `elem` [IMGraw, IMGmeta, IMGmovie]

-- file, that is stored as part of an image entry
isAnImgPart :: ImgType -> Bool
isAnImgPart ty = ty `elem` [ IMGraw, IMGmeta
                           , IMGjpg, IMGimg
                           , IMGtxt, IMGmovie
                           ]

-- part of an image entry which can be shown in a collection
-- or a raw image

isShowablePartOrRaw :: ImgType -> Bool
isShowablePartOrRaw ty =
  isShowablePart ty || ty == IMGraw

-- part can be shown
isShowablePart :: ImgType -> Bool
isShowablePart ty = ty `elem` [IMGjpg, IMGimg, IMGtxt, IMGmovie]

-- is image with camera EXIF info
-- isImgWithMeta :: ImgType -> Bool
-- isImgWithMeta ty = ty `elem` [IMGraw, IMGjpg, IMGimg, IMGmovie]

-- is .xmp file with meta data from Lightroom (e.g. GPS or rating)
isMeta :: ImgType -> Bool
isMeta IMGmeta = True
isMeta _       = False


-- files which are ignored when syncing with filesystem
isBoring :: ImgType -> Bool
isBoring IMGboring = True
isBoring _         = False

-- ----------------------------------------
