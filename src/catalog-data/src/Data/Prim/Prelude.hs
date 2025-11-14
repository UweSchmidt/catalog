{-# LANGUAGE InstanceSigs #-}
module Data.Prim.Prelude
  ( ByteString,
    LazyByteString,
    Map,
    Seq,
    Set,
    Text,
    LazyText,
    Vector,
    isEmpty,
    isn'tEmpty,
    IsString (..),

    -- * Data.Aeson
    JParser,
    JValue,
    ToJSON (..),
    FromJSON (..),
    (.=?!),
    (.:?!),

    -- * basic Data modules
    module Data.Either,
    module Data.Maybe,
    module Data.Functor,

    -- * Control.Monad
    module Control.Applicative,
    module Control.Monad,

    -- * Data.List
    intercalate,
    isPrefixOf,
    isSuffixOf,
    partition,
    sort,
    sortBy,
    nub,
    unfoldr,
    zip3,
    zip4,
    zip5,
    zip6,

    -- * Data.Char
    module Data.Char,
    module Data.Foldable,
    module Data.Semigroup,

    -- * Data.Function
    on,

    -- * Data.Read
    readMaybe,

    -- * Control.Arrow
    first,
    second,
    (&&&),
    (***),

    -- * this module
    compareBy,
    compareJust,
    compareJust',
    partBy,
    toText,

    -- * lens stuff
    module Control.Lens,
    IsoString (..),
    IsoText (..),
    IsoInteger (..),
    IsoHex (..),
    IsoMaybe (..),
    ParsePrintString (..),
    ParsePrintText (..),
    take1st,
    isoMapElems,
    isoMapList,
    isoSetList,
    isoSeqList,
    isoUpperLower,
    isoTextUrlPart,

    -- * utilities
    (.||.),
    partitionBy,
    divideAt,

    -- * Monad ops
    guarded,
    whenM,
    unlessM,
    partitionM,
    filterSeqM,

    -- * pretty printing helper
    fillLeft,
    fillRight,
    fillLeftList,
    fillRightList,

    prettyJSON,
    prettyJSONText,

    lbsToText,
    bsToText
  )
where

import Control.Applicative
import Control.Arrow
  ( first,
    second,
    (&&&),
    (***),
  )
import Control.Lens
import Control.Monad
import Data.Aeson (FromJSON (..), ToJSON (..))

import qualified Data.Aeson               as J
import qualified Data.Aeson.Types         as J
import qualified Data.Aeson.Encode.Pretty as J

import Data.ByteString
  ( ByteString,
  )
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LB
import qualified Data.ByteString.Lazy.UTF8 as LBU
import qualified Data.ByteString.UTF8      as BU

import Data.Char
import Data.Either
import Data.Foldable
import Data.Function
  ( on,
  )
import Data.Functor

import Data.List
  ( intercalate,
    isPrefixOf,
    isSuffixOf,
    nub,
    partition,
    sort,
    sortBy,
    unfoldr,
    zip4,
    zip5,
    zip6,
  )
import Data.Map.Strict
  ( Map,
  )
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Semigroup
import Data.Sequence
  ( Seq,
  )
import qualified Data.Sequence as Seq
import Data.Set
  ( Set,
  )
import qualified Data.Set as S
import Data.String
  ( IsString (..),
  )
import Data.Text
  ( Text,
  )
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Builder as LT

import Data.Text.Lens
  ( unpacked )

import Data.Vector
  ( Vector,
  )
import Network.HTTP.Types.URI
  ( urlEncode,
    urlDecode
  )
import Numeric
  ( readHex,
  )
import Text.Printf
  ( PrintfArg,
    printf,
  )
import Text.Read
  ( readMaybe,
  )

-- ----------------------------------------

type LazyByteString = LB.ByteString

type LazyText = LT.Text

type JValue = J.Value
type JParser = J.Parser

-- ----------------------------------------

isEmpty :: AsEmpty a => a -> Bool
isEmpty = has _Empty
{-# INLINE isEmpty #-}

isn'tEmpty :: (AsEmpty a) => a -> Bool
isn'tEmpty = isn't _Empty
{-# INLINE isn'tEmpty #-}

-- ----------------------------------------
--
-- the save version of conversion to/from String
-- when parsing is unsafe

class ParsePrintString a where
  -- parse (pretty) print String
  ppString :: Prism' String a
  default ppString :: (Read a, Show a) => Prism' String a
  ppString = prism' show readMaybe

instance ParsePrintString String

instance ParsePrintString Int

instance ParsePrintString Integer

instance ParsePrintString Bool

-- the safe version of conversion to/from Text

class ParsePrintText a where
  ppText :: Prism' Text a
  default ppText :: (ParsePrintString a) => Prism' Text a
  ppText = unpacked . ppString

-- ----------------------------------------

class IsoString a where
  isoString :: Iso' a String
  default isoString :: (Read a, Show a) => Iso' a String
  isoString = iso show read
  {-# INLINE isoString #-}

instance IsoString String where
  isoString :: Iso' String String
  isoString = iso id id
  {-# INLINE isoString #-}

instance IsoString Text where
  isoString :: Iso' Text String
  isoString = iso T.unpack T.pack
  {-# INLINE isoString #-}

instance IsoString LazyText where
  isoString :: Iso' LazyText String
  isoString = iso LT.unpack LT.pack
  {-# INLINE isoString #-}

instance IsoString ByteString where
  isoString :: Iso' ByteString String
  isoString = iso BU.toString BU.fromString
  {-# INLINE isoString #-}

instance IsoString LazyByteString where
  isoString :: Iso' LazyByteString String
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
  isoText :: Iso' Text Text
  isoText = iso id id
  {-# INLINE isoText #-}

instance IsoText String where
  isoText :: Iso' String Text
  isoText = from isoString
  {-# INLINE isoText #-}

-- unsafe conversions
instance IsoText Int

instance IsoText Integer

toText :: (Show a) => a -> Text
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
      toHex = printf "%016x"
      frHex s
        | [(i, "")] <- ps = i
        | otherwise = 0
        where
          ps = readHex s

instance IsoHex Int

-- ----------------------------------------

class IsoMaybe a where
  isoMaybe :: Iso' a (Maybe a)
  default isoMaybe :: (AsEmpty a, Monoid a) => Iso' a (Maybe a)
  isoMaybe = iso toM fromM
    where
      toM xs
        | isEmpty xs = Nothing
        | otherwise = Just xs
      fromM Nothing = mempty
      fromM (Just xs) = xs
  {-# INLINE isoMaybe #-}

instance IsoMaybe String

instance IsoMaybe Text

instance IsoMaybe ByteString

instance IsoMaybe LazyByteString

take1st :: (AsEmpty a, Monoid a) => [a] -> a
take1st = fromMaybe mempty . listToMaybe . take 1 . filter (not . isEmpty)

-- ----------------------------------------

-- an iso for converting a list of elemets into a map,
-- the key function extracts the keys of the elements

isoMapElems :: (Ord k) => (e -> k) -> Iso' (Map k e) [e]
isoMapElems key = iso M.elems (M.fromList . map (\e -> (key e, e)))
{-# INLINE isoMapElems #-}

-- an iso for converting between maps and list of pairs

isoMapList :: (Ord a) => Iso' (Map a b) [(a, b)]
isoMapList = iso M.toList M.fromList
{-# INLINE isoMapList #-}

isoSetList :: (Ord a) => Iso' (Set a) [a]
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

(.=?!) ::
  (AsEmpty v, J.KeyValue e a, ToJSON v) =>
  J.Key ->
  v ->
  [a]
t .=?! x
  | isEmpty x = []
  | otherwise = [t J..= x]
{-# INLINE (.=?!) #-}

(.:?!) ::
  (FromJSON v, Monoid v) =>
  J.Object ->
  J.Key ->
  J.Parser v
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
    . foldr
      ( \x m ->
          M.insertWith
            (++)
            (f x)
            [x]
            m
      )
      M.empty

-- | divide a list into equal length parts
divideAt :: Int -> [a] -> [[a]]
divideAt n
  | n <= 1 = map (: [])
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
    . foldr
      ( \x m ->
          M.insertWith
            (++)
            (f x)
            [x]
            m
      )
      M.empty

-- ----------------------------------------

-- compare 2 values with a sequence of compare functions
-- first res /= EQ wins

compareBy :: [a -> a -> Ordering] -> a -> a -> Ordering
compareBy fs x1 x2 =
  mconcat $ map (\cmp -> cmp x1 x2) fs
{-# INLINE compareBy #-}

-- compare only on Just values
--
-- useful in compareBy when Nothing values occur

compareJust :: (Ord a) => Maybe a -> Maybe a -> Ordering
compareJust (Just x1) (Just x2) = compare x1 x2
compareJust _ _ = EQ
{-# INLINE compareJust #-}

-- compare with Nothing as largest values
--
-- default with compare: Nothing is smallest

compareJust' :: (Ord a) => Maybe a -> Maybe a -> Ordering
compareJust' (Just x1) (Just x2) = compare x1 x2
compareJust' (Just _) _ = LT
compareJust' _ (Just _) = GT
compareJust' _ _ = EQ
{-# INLINE compareJust' #-}

-- ----------------------------------------

guarded :: (Alternative m) => (a -> Bool) -> a -> m a
guarded p x
  | p x       = pure x        -- Just x
  | otherwise = empty         -- Nothing


-- when and unless with monadic action for condition

whenM :: Monad m => m Bool -> m () -> m ()
whenM b c = b >>= flip when c
{-# INLINE whenM #-}

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM b c = b >>= flip unless c
{-# INLINE unlessM #-}

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p xs =
  partitionEithers <$> mapM toLR xs
  where
    toLR x = (\b -> (if b then Left else Right) x) <$> p x

-- ----------------------------------------

-- a monadic filter for sequneces

filterSeqM :: (Monad m) => (a -> m Bool) -> Seq a -> m (Seq a)
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

fillList :: (Char -> Int -> String -> String) -> Char -> [String] -> [String]
fillList ff c xs = map (ff c l) xs
  where
    l = maximum (0 : map length xs)

------------------------------------------------------------------------

prettyJSON :: (ToJSON a) => [Text] -> a -> LazyByteString
prettyJSON ks =
  J.encodePretty' (prettyJSONConfig ks)

prettyJSONText :: (ToJSON a) => [Text] -> a -> Text
prettyJSONText ks =
  LT.toStrict
  . LT.toLazyText
  . J.encodePrettyToTextBuilder' (prettyJSONConfig ks)

prettyJSONConfig :: [Text] -> J.Config
prettyJSONConfig ks =
  J.defConfig { J.confIndent  = J.Spaces 2
              , J.confCompare =
                  J.keyOrder ks
                  <>
                  compare
              }

------------------------------------------------------------------------

lbsToText :: LazyByteString -> Text
lbsToText = T.decodeUtf8Lenient . BS.concat . LB.toChunks

bsToText :: ByteString -> Text
bsToText = T.decodeUtf8Lenient

isoTextUrlPart :: Iso' Text Text
isoTextUrlPart = isoUrl . isoString . isoText
  where
    isoUrl :: Iso' Text ByteString
    isoUrl = iso (urlEncode False . T.encodeUtf8) (bsToText . urlDecode False)

------------------------------------------------------------------------
