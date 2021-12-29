-- ----------------------------------------

module Data.ImageStore
       ( ImgStore
       , ImgStore'
       , theImgTree
       , theCatMetaData
       , mkImgStore
       , emptyImgStore
       , mapImgStore
       )
where

import           Control.Lens
import Data.ImgTree
       ( mapRefTree
       , mkDirRoot
       , emptyImgRoot
       , DirTree
       , ImgNode'
       , ImgTree
       )
import Data.MetaData
       ( MetaData )

import Data.Prim
       ( FromJSON(parseJSON)
       , ToJSON(toJSON)
       , mkObjId
       , ObjId
       )

import qualified Data.Aeson as J

-- ----------------------------------------

data ImgStore' ref = IS !(DirTree ImgNode' ref) !MetaData

type ImgStore  = ImgStore' ObjId

deriving instance (Show ref) => Show (ImgStore' ref)

instance (ToJSON ref) => ToJSON (ImgStore' ref) where
  toJSON (IS i md) = J.object
    [ "ImgTree"  J..= i
    , "metadata" J..= md
    ]

instance (FromJSON ref, Ord ref) => FromJSON (ImgStore' ref) where
  parseJSON = J.withObject "ImgStore'" $ \ o ->
    IS
    <$>  o J..:  "ImgTree"
    <*> (o J..:? "metadata" J..!= mempty)

theImgTree :: Lens' (ImgStore' ref) (DirTree ImgNode' ref)
theImgTree k (IS t p) = (`IS` p) <$> k t
{-# INLINE theImgTree #-}

theCatMetaData :: Lens' (ImgStore' ref) MetaData
theCatMetaData k (IS t md) = IS t <$> k md
{-# INLINE theCatMetaData #-}

-- almost a functor, the Ord constraint is the problem
mapImgStore :: (Ord ref') => (ref -> ref') -> ImgStore' ref -> ImgStore' ref'
mapImgStore f (IS i mp) =
  IS (mapRefTree f i) mp
{-# INLINE mapImgStore #-}

-- ----------------------------------------

mkImgStore :: ImgTree -> MetaData -> ImgStore
mkImgStore = IS
{-# INLINE mkImgStore #-}

emptyImgStore :: ImgStore
emptyImgStore =
  IS r mempty
  where
    r = mkDirRoot mkObjId "" emptyImgRoot
{-# INLINE emptyImgStore #-}

-- ----------------------------------------
