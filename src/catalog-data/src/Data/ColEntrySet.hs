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
       , IsEmpty(..)
       , IsoString(isoString)
       , ObjId
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
  CES s1 <> CES s2 = CES $ s1 `S.union` s2

instance Ord ref => Monoid (ColEntrySet' ref) where
  mempty  = CES S.empty
  mappend = (<>)

instance IsEmpty (ColEntrySet' ref) where
  isempty (CES s) = S.null s

singletonColEntrySet :: ColEntry' ref -> ColEntrySet' ref
singletonColEntrySet = CES . S.singleton

fromListColEntrySet :: Ord ref => [ColEntry' ref] -> ColEntrySet' ref
fromListColEntrySet = CES . S.fromList

fromSeqColEntrySet :: Ord ref => ColEntries' ref -> ColEntrySet' ref
fromSeqColEntrySet cs = CES $ foldMap S.singleton cs

toListColEntrySet :: ColEntrySet' ref -> [ColEntry' ref]
toListColEntrySet (CES s) = S.toList s

toSeqColEntrySet :: ColEntrySet' ref -> ColEntries' ref
toSeqColEntrySet (CES s) = foldMap Seq.singleton s

memberColEntrySet :: Ord ref
                  => ColEntry' ref
                  -> ColEntrySet' ref -> Bool
memberColEntrySet ce (CES s) = ce `S.member` s

diffColEntrySet :: Ord ref
                => ColEntrySet' ref
                -> ColEntrySet' ref
                -> ColEntrySet' ref
CES s1 `diffColEntrySet` CES s2 = CES $ s1 `S.difference` s2

intersectColEntrySet :: Ord ref
                     => ColEntrySet' ref
                     -> ColEntrySet' ref
                     -> ColEntrySet' ref
CES s1 `intersectColEntrySet` CES s2 = CES $ s1 `S.intersection` s2

-- ----------------------------------------
