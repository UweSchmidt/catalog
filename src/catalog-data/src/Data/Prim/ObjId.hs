module Data.Prim.ObjId
  ( ObjId
  , mkObjId
  , objId2integer
  , objId2Int
  , objId2Maybe
  , noOfBitsUsedInKeys
  )
where

import Data.Prim.Prelude
       ( Alternative((<|>))
       , MonadPlus(mzero)
       , Iso'
       , IsEmpty(..)
       , IsoHex(..)
       , IsoString(..)
       , IsoText
       , FromJSON(parseJSON)
       , ToJSON(toJSON)
       , (^.)
       , fromMaybe
       , sort
       , iso
       )

import Data.Bits
       ( Bits (shiftL, (.&.)))

import Data.Word
       ( Word64 )

import Numeric
       ( readHex )

import Text.Printf
       ( printf )

import Data.Aeson
       ( withText )

import qualified Data.Digest.Murmur64 as MM

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
instance Show ObjId where
  show = oidToHex

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

instance IsoText ObjId

instance IsoHex ObjId where
  isoHex = objId2Int . isoHex

{-
oidHex :: Prism' String ObjId
oidHex = prism' oidToHex oidFromHex
-}
oidToHex :: ObjId -> String
oidToHex (ObjId w64) = printf "0x%016x" w64

oidFromHex :: String -> Maybe ObjId
oidFromHex ('0' : 'x' : xs)
  | [(i, "")] <- readHex xs = Just (ObjId i)
oidFromHex _                = Nothing

-- ----------------------------------------
--
-- compute the # of bits needed for identifying every value
-- in a list


noOfBitsUsedInKeys :: [ObjId] -> Int
noOfBitsUsedInKeys xs =
  searchN 32 32 $ map (\(ObjId i) -> i) xs

searchN :: Int -> Int -> [Word64] -> Int
searchN n m xs
  | m == 0       = n
  | uniqueN n xs = searchN (n - m'2) m'2 xs
  | otherwise    = searchN (n + m'2) m'2 xs
  where
    m'2 = m `div` 2

uniqueN :: Int -> [Word64] -> Bool
uniqueN n = unique . map (mask n)

unique :: [Word64] -> Bool
unique = all (uncurry (/=)) . (\ ys -> zip ys (drop 1 ys)) . sort

mask :: Int -> Word64 -> Word64
mask n i = i .&. m
  where
    m = shiftL 1 n - 1

{-# INLINE uniqueN #-}
{-# INLINE unique #-}
{-# INLINE mask #-}

-- ----------------------------------------
