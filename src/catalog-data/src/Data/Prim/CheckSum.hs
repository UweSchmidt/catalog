module Data.Prim.CheckSum
       ( CheckSum
       , mkCheckSum
       , mkFileCheckSum
       )
where

import Data.Word
       ( Word64 )

import Data.Prim.Prelude
       ( (^.)
       , iso
       , (#)
       , IsEmpty(..)
       , IsoHex(..)
       , IsoInteger(..)
       , IsoString(..)
       , IsoText
       , FromJSON(parseJSON)
       , ToJSON(toJSON)
       )

import qualified Data.Digest.Murmur64 as MM

-- ----------------------------------------

newtype CheckSum  = CS Word64

zeroCheckSum :: CheckSum
zeroCheckSum = CS 0

mkCheckSum :: MM.Hashable64 a => a -> CheckSum
mkCheckSum = CS . MM.asWord64 . MM.hash64

fromCheckSum :: Integral a => CheckSum -> a
fromCheckSum (CS csum) = fromIntegral csum

toCheckSum :: Integer -> CheckSum
toCheckSum = CS . fromInteger

deriving instance Eq CheckSum

instance IsEmpty CheckSum where
  isempty = (== zeroCheckSum)

instance Semigroup CheckSum where
  c1 <> c2
    | isempty c1 = c2
    | otherwise  = c1

instance Monoid CheckSum where
  mempty  = zeroCheckSum
  mappend = (<>)

instance IsoInteger CheckSum where
  isoInteger = iso fromCheckSum toCheckSum
  {-# INLINE isoInteger #-}

instance IsoString CheckSum where
  isoString = iso showCheckSum readCheckSum
  {-# INLINE isoString #-}

instance IsoText CheckSum

instance Show CheckSum where
  show = ("0x" ++) . showCheckSum

showCheckSum :: CheckSum -> String
showCheckSum (CS w) =
  i ^. isoHex
  where
    i :: Int
    i = fromIntegral w

readCheckSum :: String -> CheckSum
readCheckSum s =
  CS $ fromIntegral i
  where
    i :: Int
    i = isoHex # s

instance IsoHex CheckSum where
  isoHex = iso showCheckSum readCheckSum
  {-# INLINE isoHex #-}

instance ToJSON CheckSum where
  toJSON = toJSON . showCheckSum
  {-# INLINE toJSON #-}

instance FromJSON CheckSum where
  parseJSON o = readCheckSum <$> parseJSON o
  {-# INLINE parseJSON #-}

-- ----------------------------------------

-- | compute the checksum for a simple file
-- does not work for directories

mkFileCheckSum :: FilePath -> IO CheckSum
mkFileCheckSum p = mkCheckSum <$> readFile p

-- ----------------------------------------
