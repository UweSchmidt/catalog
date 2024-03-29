module Data.Prim.Prelude
       ( ByteString
       , LazyByteString
       , Map
       , Seq
       , Set
       , Text
       , LazyText
       , Vector
       , IsEmpty(..)
       , IsString(..)

         -- * Data.Aeson
       , ToJSON(..)
       , FromJSON(..)
       , (.=?!)
       , (.:?!)

         -- * basic Data modules
       , module Data.Either
       , module Data.Maybe
       , module Data.Functor

         -- * Control.Monad
       , module Control.Applicative
       , module Control.Monad

         -- * Data.List
       , intercalate
       , isPrefixOf
       , isSuffixOf
       , partition
       , sort
       , sortBy
       , nub
       , unfoldr
       , zip3
       , zip4
       , zip5
       , zip6

         -- * Data.Char
       , module Data.Char
       , module Data.Foldable
       , module Data.Semigroup

         -- * Data.Function
       , on

         -- * Data.Read
       , readMaybe

         -- * System.FilePath
       , FilePath
       , (</>)
       , takeFileName
       , takeDirectory

         -- * Control.Arrow
       , first, second, (&&&), (***)

         -- * this module
       , compareBy
       , compareJust
       , compareJust'
       , partBy
       , toText

         -- * lens stuff
       , module Control.Lens
       , IsoString(..)
       , IsoText(..)
       , IsoInteger(..)
       , IsoHex(..)
       , IsoMaybe(..)
       , PrismString(..)
       , PrismText(..)
       , take1st
       , isoMapElems
       , isoMapList
       , isoSetList
       , isoSeqList
       , isoUpperLower

         -- * utilities
       , (.||.)
       , partitionBy
       , divideAt

         -- * Monad ops
       , whenM
       , unlessM
       , partitionM
       , filterSeqM

         -- * pretty printing helper
       , fillLeft
       , fillRight
       , fillLeftList
       , fillRightList
       )
where

import Control.Applicative
import Control.Arrow
       ( (***)
       , first
       , second
       , (&&&)
       )
import Control.Lens
import Control.Monad

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Char
import Data.ByteString
       ( ByteString )

import Data.Either
import Data.Foldable

import Data.Function
       ( on )

import Data.Functor

import Data.List
       ( intercalate
       , isPrefixOf
       , isSuffixOf
       , nub
       , partition
       , sort
       , sortBy
       , unfoldr
       , zip4
       , zip5
       , zip6
       )
import Data.Map.Strict
       ( Map )

import Data.Maybe

import Data.Semigroup

import Data.Set
       ( Set )

import Data.Sequence
       ( Seq )

import Data.String
       ( IsString(..) )

import Data.Text
       ( Text )

import Data.Vector
       ( Vector )

import Numeric
       ( readHex )

import System.FilePath
       ( (</>)
       , takeDirectory
       , takeFileName
       )
import Text.Printf
       ( printf
       , PrintfArg
       )
import Text.Read
       ( readMaybe )

import qualified Data.Aeson                as J
import qualified Data.Aeson.Types          as J
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LB
import qualified Data.ByteString.Lazy.UTF8 as LBU
import qualified Data.ByteString.UTF8      as BU
import qualified Data.Map.Strict           as M
import qualified Data.Sequence             as Seq
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as LT

-- ----------------------------------------

type LazyByteString = LB.ByteString
type LazyText       = LT.Text

-- ----------------------------------------

class IsEmpty a where
  isempty :: a -> Bool


instance IsEmpty [a] where
  isempty = null
  {-# INLINE isempty #-}

instance IsEmpty (Maybe a) where
  isempty Nothing = True
  isempty _       = False
  {-# INLINE isempty #-}

instance IsEmpty Text where
  isempty = T.null
  {-# INLINE isempty #-}

instance IsEmpty ByteString where
  isempty = BS.null
  {-# INLINE isempty #-}

instance IsEmpty LazyByteString where
  isempty = LB.null
  {-# INLINE isempty #-}

instance IsEmpty (Set a) where
  isempty = S.null
  {-# INLINE isempty #-}

instance IsEmpty (Seq a) where
  isempty = Seq.null
  {-# INLINE isempty #-}

instance IsEmpty (Map k v) where
  isempty = M.null
  {-# INLINE isempty #-}

-- ----------------------------------------
--
-- the save version of conversion to/from String
-- when parsing is unsafe

class PrismString a where
  prismString :: Prism' String a

  default prismString :: (Read a, Show a) => Prism' String a
  prismString = prism' show readMaybe


-- the safe version of conversion to/from Text

class PrismText a where
  textPrism :: Prism' Text a

  default textPrism :: (PrismString a) => Prism' Text a
  textPrism = isoString . prismString

-- ----------------------------------------

class IsoString a where
  isoString :: Iso' a String

  default isoString :: (Read a, Show a) => Iso' a String
  isoString = iso show read
  {-# INLINE isoString #-}

instance IsoString String where
  isoString = iso id id
  {-# INLINE isoString #-}

instance IsoString Text where
  isoString = iso T.unpack T.pack
  {-# INLINE isoString #-}

instance IsoString LazyText where
  isoString = iso LT.unpack LT.pack
  {-# INLINE isoString #-}

instance IsoString ByteString where
  isoString = iso BU.toString BU.fromString
  {-# INLINE isoString #-}

instance IsoString LazyByteString where
  isoString = iso LBU.toString LBU.fromString
  {-# INLINE isoString #-}

-- unsafe conversions
instance IsoString Int
instance IsoString Integer

-- ----------------------------------------

class IsoText a where
  isoText :: Iso' a Text

  default isoText :: (IsoString a) => Iso' a Text
  isoText = isoString . isoText
  {-# INLINE isoText #-}

instance IsoText Text where
  isoText = iso id id
  {-# INLINE isoText #-}

instance IsoText String where
  isoText = from isoString
  {-# INLINE isoText #-}

-- unsafe conversions
instance IsoText Int
instance IsoText Integer

toText :: Show a => a -> Text
toText x = show x ^. isoText
{-# INLINE toText #-}

-- ----------------------------------------

class IsoInteger a where
  isoInteger :: Iso' a Integer

  default isoInteger :: (Integral a) => Iso' a Integer
  isoInteger = iso toInteger fromInteger
  {-# INLINE isoInteger #-}

-- ----------------------------------------

class IsoHex a where
  isoHex :: Iso' a String

  default isoHex :: (Integral a, PrintfArg a) => Iso' a String
  isoHex = iso toHex frHex
    where
      toHex   = printf "%016x"
      frHex s
        | [(i, "")] <- ps = i
        | otherwise       = 0
        where
          ps = readHex s

instance IsoHex Int

-- ----------------------------------------

class IsoMaybe a where
  isoMaybe :: Iso' a (Maybe a)

  default isoMaybe :: (IsEmpty a, Monoid a) => Iso' a (Maybe a)
  isoMaybe = iso toM fromM
    where
      toM xs
        | isempty xs = Nothing
        | otherwise  = Just xs
      fromM Nothing   = mempty
      fromM (Just xs) = xs
  {-# INLINE isoMaybe #-}

instance IsoMaybe String
instance IsoMaybe Text
instance IsoMaybe ByteString
instance IsoMaybe LazyByteString

take1st :: (IsEmpty a, Monoid a) => [a] -> a
take1st = fromMaybe mempty . listToMaybe . take 1 . filter (not . isempty)

-- ----------------------------------------

-- an iso for converting a list of elemets into a map,
-- the key function extracts the keys of the elements

isoMapElems :: Ord k => (e -> k) -> Iso' (Map k e) [e]
isoMapElems key = iso M.elems (M.fromList . map (\ e -> (key e, e)))
{-# INLINE isoMapElems #-}

-- an iso for converting between maps and list of pairs

isoMapList :: Ord a => Iso' (Map a b) [(a, b)]
isoMapList = iso M.toList M.fromList
{-# INLINE isoMapList #-}

isoSetList :: Ord a => Iso' (Set a) [a]
isoSetList = iso S.toList S.fromList
{-# INLINE isoSetList #-}

isoSeqList :: Iso' (Seq a) [a]
isoSeqList = iso toList Seq.fromList
{-# INLINE isoSeqList #-}

isoUpperLower :: Iso' String String
isoUpperLower = iso (& _head %~ toUpper) (& _head %~ toLower)
{-# INLINE isoUpperLower #-}


{-
-- my first traversal, but Edward already made this, it's named each
-- works on various other structures

all3 :: Traversal (a, a, a) (b, b, b) a b
all3 inj (x1, x2, x3) = (,,) <$> inj x1 <*> inj x2 <*> inj x3
-- -}

-- ----------------------------------------
--
-- mothers little helper for en/decoding optional fileds

(.=?!) :: (IsEmpty v, J.KeyValue e a, ToJSON v)
       => J.Key -> v -> [a]
t .=?! x
  | isempty x = []
  | otherwise = [t J..= x]
{-# INLINE (.=?!) #-}

(.:?!) :: (FromJSON v, Monoid v)
       => J.Object -> J.Key -> J.Parser v
o .:?! t =
  o J..:? t J..!= mempty
{-# INLINE (.:?!) #-}

-- ----------------------------------------

infixr 2 .||.

-- | Lift boolean 'or' over predicates.

(.||.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
p .||. q = (||) <$> p <*> q


-- | group a list of entries by a mapping the
-- elements to an ordered domain

partitionBy :: (Ord e) => (a -> e) -> [a] -> [[a]]
partitionBy f =
  M.elems
  . foldr (\ x m -> M.insertWith (++) (f x) [x]
                    m) M.empty

-- | divide a list into equal length parts

divideAt :: Int -> [a] -> [[a]]
divideAt n
  | n <= 1    = map (:[])
  | otherwise = go
  where
    go [] = []
    go xs = ys : go xs'
      where
        (ys, xs') = splitAt n xs

-- ----------------------------------------

-- put all elemnts of a, which have equal e values
-- into a sublist
--
-- partBy (`mod` 3) [0..9] = [[0,3,6,9],[1,4,7],[2,5,8]]

partBy :: (Ord e) => (a -> e) -> [a] -> [[a]]
partBy f =
  M.elems
  . foldr (\ x m -> M.insertWith (++) (f x) [x]
                    m) M.empty

-- ----------------------------------------

-- compare 2 values with a sequence of compare functions
-- first res /= EQ wins

compareBy :: [a -> a -> Ordering] -> a -> a -> Ordering
compareBy fs x1 x2 =
  mconcat $ map (\ cmp -> cmp x1 x2) fs
{-# INLINE compareBy #-}


-- compare only on Just values
--
-- useful in compareBy when Nothing values occur

compareJust :: Ord a => Maybe a -> Maybe a -> Ordering
compareJust (Just x1) (Just x2) = compare x1 x2
compareJust _         _         = EQ
{-# INLINE compareJust #-}


-- compare with Nothing as largest values
--
-- default with compare: Nothing is smallest

compareJust' :: Ord a => Maybe a -> Maybe a -> Ordering
compareJust' (Just x1) (Just x2) = compare x1 x2
compareJust' (Just _ ) _         = LT
compareJust' _         (Just _ ) = GT
compareJust' _         _         = EQ
{-# INLINE compareJust' #-}

-- ----------------------------------------

whenM :: (Monoid a, Monad m) => m Bool -> m a -> m a
whenM b c = do
  b' <- b
  if b' then c else return mempty

{-# INLINE whenM #-}

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b c = do
  b' <- b
  unless b' c
{-# INLINE unlessM #-}

partitionM :: Monad m
           => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p xs =
  partitionEithers <$> mapM toLR xs
  where
    toLR x = (\ b -> (if b then Left else Right) x) <$> p x

-- ----------------------------------------

-- a monadic filter for sequneces

filterSeqM :: Monad m => (a -> m Bool) -> Seq a -> m (Seq a)
filterSeqM p = foldM f mempty
  where
    f rs x = do
      b <- p x
      return
        ( if b
          then rs Seq.|> x
          else rs
        )
{-# INLINE filterSeqM #-}

-- ----------------------------------------
--
-- pretty printing stuff

fillLeft :: Char -> Int -> String -> String
fillLeft c i xs = replicate (i - length xs) c ++ xs

fillRight :: Char -> Int -> String -> String
fillRight c i xs = xs ++ replicate (i - length xs) c

fillLeftList :: Char -> [String] -> [String]
fillLeftList = fillList fillLeft

fillRightList :: Char -> [String] -> [String]
fillRightList = fillList fillRight


fillList :: (Char -> Int -> String -> String)
         -> Char -> [String] -> [String]
fillList ff c xs = map (ff c l) xs
  where
    l = maximum (0 : map length xs)

------------------------------------------------------------------------
