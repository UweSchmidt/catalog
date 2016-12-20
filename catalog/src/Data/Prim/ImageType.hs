{-# LANGUAGE StandaloneDeriving #-}

module Data.Prim.ImageType
where

import           Data.Prim.Name
import           Data.Prim.Prelude

-- ----------------------------------------

type NameImgType = (Name, ImgType)

data ImgType =
  IMGraw   | IMGmeta   | IMGjson   | IMGjpg    |
  IMGimg   | IMGcopy   | IMGimgdir | IMGjpgdir |
  IMGother | IMGboring | IMGhugin  | IMGdxo    |
  IMGtxt   | IMGdng

deriving instance Eq   ImgType
deriving instance Ord  ImgType
deriving instance Show ImgType
deriving instance Read ImgType

instance ToJSON ImgType where
  toJSON = toJSON . show
  {-# INLINE toJSON #-}

instance FromJSON ImgType where
  parseJSON o = read <$> parseJSON o

instance Monoid ImgType where
  mempty = IMGother
  IMGother `mappend` t2 = t2
  t1       `mappend` _  = t1

-- ----------------------------------------
