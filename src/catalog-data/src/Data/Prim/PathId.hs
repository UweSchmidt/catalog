{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}

-- paths used as object ids
-- not space efficient, but god for testing

module Data.Prim.PathId
       ( ObjId
       , mkObjId
       , objId2path
       )
       where

import Data.Prim.Path
import Data.Prim.Prelude

-- ----------------------------------------

newtype ObjId = ObjId Path

mkObjId :: Path -> ObjId
mkObjId = ObjId
{-# INLINE mkObjId #-}

showObjId :: ObjId -> String
showObjId (ObjId p) = show p
{-# INLINE showObjId #-}

readObjId :: String -> ObjId
readObjId = ObjId . readPath
{-# INLINE readObjId #-}

objId2path :: Iso' ObjId Path
objId2path = iso (\ (ObjId p) -> p) mkObjId
{-# INLINE objId2path #-}

deriving instance Eq   ObjId
deriving instance Ord  ObjId

instance Semigroup ObjId where
  (<>) :: ObjId -> ObjId -> ObjId
  i1 <> i2
    | isEmpty i1 = i2
    | otherwise  = i1
  {-# INLINE (<>) #-}

instance Monoid ObjId where
  mempty :: ObjId
  mempty  = ObjId mempty
  {-# INLINE mempty #-}

instance AsEmpty ObjId

instance Show ObjId where
  show :: ObjId -> String
  show = showObjId
  {-# INLINE show #-}

instance ToJSON ObjId where
  toJSON = toJSON . showObjId
  {-# INLINE toJSON #-}

instance FromJSON ObjId where
  parseJSON o = readObjId <$> parseJSON o

instance IsoString ObjId where
  isoString :: Iso' ObjId String
  isoString = iso showObjId readObjId
  {-# INLINE isoString #-}

-- ----------------------------------------
