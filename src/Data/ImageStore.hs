{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Data.ImageStore
       ( ImgStore
       , ImgStore'
       , theImgTree
       , theMountPath
       , mkImgStore
       , emptyImgStore
       , mapImgStore
       )
where

import           Control.Lens
import           Data.ImgTree
import           Data.Prim

import qualified Data.Aeson as J

-- ----------------------------------------

data ImgStore' ref = IS !(DirTree ImgNode' ref) !SysPath

type ImgStore  = ImgStore' ObjId

deriving instance (Show ref) => Show (ImgStore' ref)

instance (ToJSON ref) => ToJSON (ImgStore' ref) where
  toJSON (IS i mp) = J.object
    [ "ImgTree"   J..= i
    , "MountPath" J..= mp
    ]

instance (FromJSON ref, Ord ref) => FromJSON (ImgStore' ref) where
  parseJSON = J.withObject "ImgStore'" $ \ o ->
    IS
    <$> o J..: "ImgTree"
    <*> o J..: "MountPath"

theImgTree :: Lens' (ImgStore' ref) (DirTree ImgNode' ref)
theImgTree k (IS t p) = (\new -> IS new p) <$> k t
{-# INLINE theImgTree #-}

theMountPath :: Lens' (ImgStore' ref) SysPath
theMountPath k (IS t p) = (\new -> IS t new) <$> k p
{-# INLINE theMountPath #-}

-- almost a functor, the Ord constraint is the problem
mapImgStore :: (Ord ref') => (ref -> ref') -> ImgStore' ref -> ImgStore' ref'
mapImgStore f (IS i mp) =
  IS (mapRefTree f i) mp
{-# INLINE mapImgStore #-}

-- ----------------------------------------

mkImgStore :: ImgTree -> SysPath -> ImgStore
mkImgStore = IS
{-# INLINE mkImgStore #-}

emptyImgStore :: ImgStore
emptyImgStore =
  IS r emptySysPath
  where
    r = mkDirRoot mkObjId "" emptyImgRoot
{-# INLINE emptyImgStore #-}

-- ----------------------------------------
