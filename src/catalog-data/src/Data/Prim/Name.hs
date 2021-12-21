{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Prim.Name
       ( Name
       , mkName
       , isNameSuffix
       , addNameSuffix
       , substNameSuffix
       )
where

import           Data.Prim.Prelude

import qualified Data.Aeson as J
import qualified Data.Text as T

-- ----------------------------------------
--
-- names are wrapped Text's
-- names don't start with a '.'
-- so ".", ".." and dot files are all mapped to the empty name

newtype Name = Name Text

emptyName :: Name
emptyName = mkName ""

-- illegal string -> empty Name
-- dot files  -> empty Name
-- empty text -> empty Name

mkName :: String -> Name
mkName ('.' : _) = emptyName
mkName xs        = Name . T.pack $ xs
{-# INLINE mkName #-}

toName :: Text -> Name
toName t
  | not (isempty t)
    &&
    T.head t /= '.' = Name t
  | otherwise       = emptyName

instance IsEmpty Name where
  isempty (Name n) = isempty n
  {-# INLINE isempty #-}

fromName :: Name -> String
fromName (Name fsn) = T.unpack $ fsn
{-# INLINE fromName #-}

isNameSuffix :: Name -> Name -> Bool
isNameSuffix (Name sx) (Name n) = sx `T.isSuffixOf` n

addNameSuffix :: Text -> Name -> Name
addNameSuffix sx n = n & isoText %~ (<> sx)

substNameSuffix :: Text -> Text -> Name -> Name
substNameSuffix os ns n'
  | os `T.isSuffixOf` n =
      n' & isoText %~ T.dropEnd (T.length os) . (<> ns)
  | otherwise =
      n'
  where
    n  = n'  ^. isoText

deriving instance Eq   Name
deriving instance Ord  Name

instance IsoString Name where
  isoString = iso fromName mkName
  {-# INLINE isoString #-}

instance IsoText Name where
  isoText = iso (\ (Name n) -> n) toName
  {-# INLINE isoText #-}

instance Semigroup Name where
  Name n1 <> Name n2 = Name $ n1 `T.append` n2

instance Monoid Name where
  mempty  = emptyName
  mappend = (<>)

instance IsString Name where
  fromString = mkName
  {-# INLINE fromString #-}

instance Show Name where
  show = fromName
  {-# INLINE show #-}

instance ToJSON Name where
  toJSON = toJSON . (^. isoText)
  {-# INLINE toJSON #-}

instance FromJSON Name where
  parseJSON (J.String t) = return $ isoText # t
  parseJSON _            = mzero

-- ----------------------------------------
