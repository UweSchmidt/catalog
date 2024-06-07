{-# LANGUAGE InstanceSigs #-}
module Data.Prim.SysPath
  ( SysPath
  , isoFilePath
  , emptySysPath
  , mkSysPath
  )
where

import Data.Prim.Prelude
    ( iso
    , Iso'
    , Prism'
    , AsEmpty(..)
    , FromJSON(parseJSON)
    , ToJSON(toJSON)
    )

--
-- file paths for file system paths with mount point prefix
--
-- SysPath' is needed for deriving fmap

newtype SysPath' a = SP {_unSP :: a}
  deriving (Eq, Ord, Show, Functor)

type SysPath     = SysPath' FilePath

isoFilePath :: Iso' (SysPath' a) a
isoFilePath = iso _unSP SP
{-# INLINE isoFilePath #-}

instance ToJSON a => ToJSON (SysPath' a) where
  toJSON = toJSON . _unSP

instance FromJSON a => FromJSON (SysPath' a) where
  parseJSON o = SP <$> parseJSON o

instance AsEmpty a => AsEmpty (SysPath' a) where
  _Empty :: AsEmpty a => Prism' (SysPath' a) ()
  _Empty = isoFilePath . _Empty
  {-# INLINE _Empty #-}

emptySysPath :: Monoid a => SysPath' a
emptySysPath = SP mempty

mkSysPath :: FilePath -> SysPath
mkSysPath = SP

-- ----------------------------------------
