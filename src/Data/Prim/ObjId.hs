{-# LANGUAGE StandaloneDeriving #-}

module Data.Prim.ObjId
where

import qualified Data.Digest.Murmur64 as MM
import           Data.Maybe
import           Data.Prim.Prelude
import           Data.Word
import           Numeric(readHex)
import           Text.Printf (printf)
import           Data.Aeson (withText)

-- ----------------------------------------

newtype ObjId     = ObjId Word64

mkObjId :: MM.Hashable64 a => a -> ObjId
mkObjId = ObjId . MM.asWord64 . MM.hash64
{-# INLINE mkObjId #-}

fromObjId :: ObjId -> Integer
fromObjId (ObjId w) = fromIntegral w
{-# INLINE fromObjId #-}

toObjId :: Integer -> ObjId
toObjId = ObjId . fromIntegral
{-# INLINE toObjId #-}

objId2integer :: Iso' ObjId Integer
objId2integer = iso fromObjId toObjId
{-# INLINE objId2integer #-}

objId2Int :: Iso' ObjId Int
objId2Int = iso (\ (ObjId w) -> fromIntegral w)
                (ObjId . fromIntegral)
{-# INLINE objId2Int #-}

objId2Maybe :: Iso' ObjId (Maybe ObjId)
objId2Maybe =
  iso (\ i -> if isempty i
              then Nothing
              else Just i
      )
      (fromMaybe mempty)
{-# INLINE objId2Maybe #-}

deriving instance Eq   ObjId
deriving instance Ord  ObjId
deriving instance Show ObjId

instance Semigroup ObjId where
  i1 <> i2
    | isempty i1 = i2
    | otherwise  = i1
  {-# INLINE (<>) #-}

instance Monoid ObjId where
  mempty  = toObjId 0
  mappend = (<>)

instance IsEmpty ObjId where
  isempty = (== mempty)
  {-# INLINE isempty #-}

instance ToJSON ObjId where
  -- old instance as decimals
  -- toJSON = toJSON . fromObjId

  toJSON = toJSON . oidToHex
  {-# INLINE toJSON #-}

instance FromJSON ObjId where
  parseJSON o =
    parseHex o     -- new: hex number as string
    <|>
    parseNumber o  -- old: decimal as number
    where
      -- the old parser for numbers in scientific format
      parseNumber n = toObjId <$> parseJSON n

      -- the new parser for hex string representation of ObjId's
      parseHex = withText "ObjId" $ \ xs ->
        maybe
          mzero
          pure
          (oidFromHex $ xs ^. isoString)


instance IsoString ObjId where
  isoString = objId2integer . isoString
  {-# INLINE isoString #-}

instance IsoHex ObjId where
  isoHex = objId2Int . isoHex

oidHex :: Prism' String ObjId
oidHex = prism' oidToHex oidFromHex

oidToHex :: ObjId -> String
oidToHex (ObjId w64) = printf "0x%016x" w64

oidFromHex :: String -> Maybe ObjId
oidFromHex ('0' : 'x' : xs)
  | [(i, "")] <- readHex xs = Just (ObjId i)
oidFromHex _                = Nothing

-- ----------------------------------------
