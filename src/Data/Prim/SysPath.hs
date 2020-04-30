{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Prim.SysPath
  ( SysPath
  , SysTextPath
  , isoFilePath
  , isoTextPath
  , emptySysPath
  , mkSysPath
  , mkSysTextPath
  )
where

import Data.Prim.Prelude

--
-- file paths for file system paths with mount point prefix

newtype SysPath' a = SP {_unSP :: a}
  deriving (Eq, Ord, Show, Functor)

type SysPath     = SysPath' FilePath
type SysTextPath = SysPath' Text

isoFilePath :: Iso' SysPath FilePath
isoFilePath = iso _unSP SP

isoTextPath :: Iso' SysTextPath Text
isoTextPath = iso _unSP SP

instance ToJSON a => ToJSON (SysPath' a) where
  toJSON = toJSON . _unSP

instance FromJSON a => FromJSON (SysPath' a) where
  parseJSON o = SP <$> parseJSON o

instance (Eq a, Monoid a) => IsEmpty (SysPath' a) where
  isempty sp = sp == emptySysPath

emptySysPath :: Monoid a => SysPath' a
emptySysPath = SP mempty

mkSysPath :: FilePath -> SysPath
mkSysPath = SP

mkSysTextPath :: Text -> SysTextPath
mkSysTextPath = SP

-- ----------------------------------------
