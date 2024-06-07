{-# LANGUAGE InstanceSigs #-}
module Data.ColEntrySet
       ( ColEntrySet'
       , ColEntrySet
       , memberColEntrySet
       , singletonColEntrySet
       , fromListColEntrySet
       , fromSeqColEntrySet
       , toListColEntrySet
       , toSeqColEntrySet
       , diffColEntrySet
       , intersectColEntrySet
       )
where

import Data.Prim
       ( intercalate
       , (^.)
       , Set
       , IsoString(isoString)
       , ObjId
       , AsEmpty(..)
       , nearly
       , Prism'
       )
import Data.ImgNode
       ( colEntry
       , ColEntries'
       , ColEntry'
       )

import qualified Data.Set        as S
import qualified Data.Sequence   as Seq

-- ----------------------------------------

newtype ColEntrySet' ref = CES (Set (ColEntry' ref))

type    ColEntrySet      = ColEntrySet' ObjId

instance Show ref => Show (ColEntrySet' ref) where
  show (CES cs) = fmt $ S.toList cs
    where
      fmt xs = "{" <> intercalate ", " (map fmtCE xs) <> "}"
      fmtCE  =
        colEntry
          (\ r n -> "(" <> show r <> ", " <> n ^. isoString <> ")")
          show

instance Ord ref => Semigroup (ColEntrySet' ref) where
  (<>) :: Ord ref => ColEntrySet' ref -> ColEntrySet' ref -> ColEntrySet' ref
  CES s1 <> CES s2 = CES $ s1 `S.union` s2
  {-# INLINE (<>) #-}

instance Ord ref => Monoid (ColEntrySet' ref) where
  mempty :: Ord ref => ColEntrySet' ref
  mempty  = CES S.empty
  {-# INLINE mempty #-}

instance AsEmpty (ColEntrySet' ref) where
  _Empty :: Prism' (ColEntrySet' ref) ()
  _Empty = nearly (CES S.empty) (\ (CES s) -> S.null s)
  {-# INLINE _Empty #-}

singletonColEntrySet :: ColEntry' ref -> ColEntrySet' ref
singletonColEntrySet = CES . S.singleton
{-# INLINE singletonColEntrySet  #-}

fromListColEntrySet :: Ord ref => [ColEntry' ref] -> ColEntrySet' ref
fromListColEntrySet = CES . S.fromList
{-# INLINE fromListColEntrySet  #-}

fromSeqColEntrySet :: Ord ref => ColEntries' ref -> ColEntrySet' ref
fromSeqColEntrySet cs = CES $ foldMap S.singleton cs
{-# INLINE fromSeqColEntrySet  #-}

toListColEntrySet :: ColEntrySet' ref -> [ColEntry' ref]
toListColEntrySet (CES s) = S.toList s
{-# INLINE toListColEntrySet  #-}

toSeqColEntrySet :: ColEntrySet' ref -> ColEntries' ref
toSeqColEntrySet (CES s) = foldMap Seq.singleton s
{-# INLINE toSeqColEntrySet  #-}

memberColEntrySet :: Ord ref
                  => ColEntry' ref
                  -> ColEntrySet' ref -> Bool
memberColEntrySet ce (CES s) = ce `S.member` s
{-# INLINE memberColEntrySet  #-}

diffColEntrySet :: Ord ref
                => ColEntrySet' ref
                -> ColEntrySet' ref
                -> ColEntrySet' ref
CES s1 `diffColEntrySet` CES s2 = CES $ s1 `S.difference` s2
{-# INLINE diffColEntrySet #-}

intersectColEntrySet :: Ord ref
                     => ColEntrySet' ref
                     -> ColEntrySet' ref
                     -> ColEntrySet' ref
CES s1 `intersectColEntrySet` CES s2 = CES $ s1 `S.intersection` s2
{-# INLINE intersectColEntrySet #-}

-- ----------------------------------------
