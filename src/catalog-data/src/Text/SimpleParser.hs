{-# LANGUAGE InstanceSigs #-}
module Text.SimpleParser
  ( module Text.Megaparsec
  , module Text.Megaparsec.Char
  , module Text.SimpleParser
  )
where

import Data.Prim.Prelude
       ( Alternative((<|>), some, many)
       , Text
       , on
       , fromMaybe
       , isJust
       , toLower
       , toUpper
       )


import Data.Void
       ( Void )

import Text.Megaparsec
       ( Parsec
       , parseMaybe
       , count
       , eof
       , option
       , try
       , satisfy
       , single
       , anySingle
       , chunk
       )

import qualified Data.Text as T

-- Data.Prim.Prelude reexports lenses,
-- somewhere in lenses oneOf and noeOf are defined
-- hiding these in import for Prelude
-- gives a silly warning: Prelude does not export oneOf nor noneOf
--
-- so noneOf and oneOf parsers are imported qualified
-- and renamed to oneOf' and noeOf'
--
import qualified Text.Megaparsec as P -- (oneOf, noneOf)

-- not used locally but convenient for users
--
import Text.Megaparsec.Char

-- --------------------
--
-- simple String parser

type SP   = CP String
type TP   = CP Text

type CP s = Parsec Void s

type CPC s = (P.Stream s, P.Token s ~ Char, PackChars s, Semigroup s, Monoid s)

-- --------------------

infixr 5 <++>

(<++>) :: (Applicative f, Semigroup b) => f b -> f b -> f b
p1 <++> p2 = (<>) <$> p1 <*> p2

{-# INLINE (<++>) #-}

class PackChars t where
  packChars   :: [Char] -> t
  unpackChars :: t -> [Char]
  mapChars    :: (Char -> Char) -> (t -> t)
  consChar    :: Char -> t -> t
  concatChars :: [t] -> t

instance PackChars String where
  packChars :: [Char] -> String
  packChars = id

  unpackChars :: String -> [Char]
  unpackChars = id

  mapChars :: (Char -> Char) -> String -> String
  mapChars cf = map cf

  consChar :: Char -> String -> String
  consChar = (:)

  concatChars :: [String] -> String
  concatChars = concat

instance PackChars Text where
  packChars :: [Char] -> Text
  packChars = T.pack

  unpackChars :: Text -> [Char]
  unpackChars = T.unpack

  mapChars :: (Char -> Char) -> Text -> Text
  mapChars cf = T.map cf

  consChar :: Char -> Text -> Text
  consChar = T.cons

  concatChars :: [Text] -> Text
  concatChars = T.concat

-- ----------

-- substitute for regex ".*xxx"
--
-- example: split a file extension from a path
--
-- parseMaybe anyStringThen' (string ".jpg" <* eof) "abc.jpgf.jpg"
--    -> Just ("abc.jpgf", ".jpg")

anyStringThen' :: (CPC s) => CP s a -> CP s (s, a)
anyStringThen' p =
  try
    ( do
        ps <- p
        return (mempty, ps)
    )
    <|> do
      c <- anySingle
      (cs, ps) <- anyStringThen' p
      return (c `consChar` cs, ps)

anyStringThen :: (CPC s) => CP s s -> CP s s
anyStringThen p = uncurry (<>) <$> anyStringThen' p
{-# INLINE anyStringThen  #-}

splitSuffix :: CPC s => CP s s -> CP s (s, s)
splitSuffix p = anyStringThen' (p <* eof)
{-# INLINE splitSuffix  #-}

withSuffix :: CPC s => CP s s -> CP s s
withSuffix p = uncurry (<>) <$> splitSuffix p
{-# INLINE withSuffix  #-}

anyString :: CPC s => CP s s
anyString = packChars <$> many anySingle
{-# INLINE anyString  #-}

-- rename noneOf, it's defined somewhere in lens
noneOf' :: CPC s => String -> CP s Char
noneOf' = P.noneOf
{-# INLINE noneOf'  #-}

oneOf' :: (CPC s) => String -> CP s Char
oneOf' = P.oneOf
{-# INLINE oneOf'  #-}

ssp :: CPC s => CP s s
ssp = packChars <$> some (single ' ')
{-# INLINE ssp  #-}

msp :: (CPC s) => CP s s
msp = packChars <$> many (single ' ')
{-# INLINE msp  #-}

manyChars :: (CPC s) => CP s Char -> CP s s
manyChars cp = packChars <$> many cp
{-# INLINE manyChars  #-}

someChars :: (CPC s) => CP s Char -> CP s s
someChars cp = packChars <$> some cp
{-# INLINE someChars  #-}

digits :: (CPC s) => CP s s
digits = someChars digitChar
{-# INLINE digits #-}

letters :: (CPC s) => CP s s
letters = someChars letterChar
{-# INLINE letters #-}

word'' :: (CPC s) => (Char -> CP s Char) -> s -> CP s s
word'' charP w =
  try (packChars <$> traverse charP (unpackChars w))

lowerOrUpperCaseWord :: (CPC s) => s -> CP s s
lowerOrUpperCaseWord w =
  caseWord (mapChars toLower w)
  <|>
  caseWord (mapChars toUpper w)

caseWord :: CPC s => s -> CP s s
caseWord = word'' caseChar
{-# INLINE caseWord  #-}

noCaseWord :: (CPC s) => s -> CP s s
noCaseWord = word'' noCaseChar
{-# INLINE noCaseWord  #-}

caseChar :: CPC s => Char -> CP s Char
caseChar c = satisfy (charCase c)
{-# INLINE caseChar  #-}

noCaseChar :: (CPC s) => Char -> CP s Char
noCaseChar c = satisfy (charNoCase c)
{-# INLINE noCaseChar  #-}

ntimes :: Monad p => Int -> p a -> p [a]
ntimes n p = foldr f (return []) $ replicate n p
  where
    p' `f` ps' = (:) <$> p' <*> ps'

nChars :: (CPC s) => Int -> (Char -> Bool) -> CP s s
nChars n cp = packChars <$> ntimes n (satisfy cp)
{-# INLINE nChars  #-}

atleast'ntimes :: (Monad p, Alternative p) => Int -> p a -> p [a]
atleast'ntimes n p = (<>) <$> ntimes n p <*> many p

atleast'nChars :: CPC s => Int -> (Char -> Bool) -> CP s s
atleast'nChars n cp = packChars <$> atleast'ntimes n (satisfy cp)
{-# INLINE atleast'nChars  #-}

-- --------------------

-- noCaseWord:

-- >>> parseMaybe (noCaseWord "Emil") ("emil"::String)
-- Just "emil"

-- >>> parseMaybe (noCaseWord "Emil") ("emil"::Text)
-- Just "emil"

-- >>> parseMaybe (noCaseWord "Emil") ("EMIL"::Text)
-- Just "EMIL"

-- >>> parseMaybe (noCaseWord "Emil") ("Egon"::Text)
-- Nothing

-- nChars:

-- >>> parseMaybe (nChars 4 (== '*')) ("****" :: String)
-- Just "****"

-- >>> parseMaybe (nChars 4 (== '*')) ("***" :: String)
-- Nothing

-- >>> parseMaybe (nChars 4 (== '*')) ("*****" :: String)
-- Nothing

-- >>> parseMaybe (nChars 4 (== '*')) ("123" :: String)
-- Nothing

-- >>> parseMaybe (nChars 4 (== '*')) ("****" :: Text)
-- Just "****"


-- atleast'nChars

-- >>> parseMaybe (atleast'nChars 4 (== '*')) ("****" :: String)
-- Just "****"

-- >>> parseMaybe (atleast'nChars 4 (== '*')) ("***" :: String)
-- Nothing

-- >>> parseMaybe (atleast'nChars 4 (== '*')) ("*****" :: String)
-- Just "*****"

-- --------------------

sedParser :: (CPC s, Monoid s) => (a -> s) -> CP s a -> CP s s
sedParser edit p = concatChars <$> go
  where
    go =
      try ((:) <$> (edit <$> p) <*> go)
      <|>
      ((:) <$> (packChars . (: []) <$> anySingle) <*> go)
      <|>
      return []

-- --------------------

sedP :: (CPC s, Monoid s) => (a -> s) -> CP s a -> (s -> s)
sedP ef p = fromMaybe mempty . parseMaybe (sedParser ef p)
{-# INLINE sedP  #-}

matchP :: CPC s => CP s a -> s -> Bool
matchP p = toBool . parseMaybe p
{-# INLINE matchP  #-}

toBool :: Maybe a -> Bool
toBool = isJust
{-# INLINE toBool  #-}

matchPred :: (a -> Bool) -> a -> Maybe a
matchPred p x
  | p x       = Just x
  | otherwise = Nothing

eqNoCase :: String -> String -> Bool
eqNoCase = (==) `on` map toLower
{-# INLINE eqNoCase  #-}

-- ----------------------------------------

-- >>> sedP (\ c -> [c, c, c]) (single '1') ("1x1x1" :: String)
-- "111x111x111"

-- >>> sedP (\ c -> T.pack [c, c, c]) (single '1') ("1x1x1" :: Text)
-- "111x111x111"

-- ----------------------------------------
--
-- parser for glob style wildcards
--
-- parseGlob is a parser for glob style wildcards
-- input is a glob style pattern
-- output is a parser for pattern matching against the pattern

parseGlobString :: CP String (CP String String)
parseGlobString = parseGlob elemCase
{-# INLINE parseGlobString  #-}

parseGlobText :: CP Text (CP Text Text)
parseGlobText = parseGlob elemCase
{-# INLINE parseGlobText  #-}

parseGlobNoCaseString :: CP String (CP String String)
parseGlobNoCaseString = parseGlob elemNoCase
{-# INLINE parseGlobNoCaseString  #-}

parseGlobNoCaseText :: CP Text (CP Text Text)
parseGlobNoCaseText = parseGlob elemNoCase
{-# INLINE parseGlobNoCaseText  #-}

parseGlob :: (CPC s) => (Char -> Char -> (Char -> Bool)) -> CP s (CP s s)
parseGlob elemChar =
  ( do _ <- single '*'
       anyStringThen <$> go
  )
  <|>
  ( do _ <- single '?'
       p <- go
       return ( do x  <- anySingle
                   xs <- p
                   return (x `consChar` xs)
              )
  )
  <|>
  ( do inSet <- globSet
       p     <- go
       return ( do x  <- satisfy inSet
                   xs <- p
                   return (x `consChar` xs)
              )
  )
  <|>
  ( do c <- anySingle
       p <- go
       return ( do x  <- satisfy (elemChar c c)
                   xs <- p
                   return (x `consChar` xs)
              )
  )
  <|>
  ( eof *> return (eof *> return mempty) )
  where
    go = parseGlob elemChar

    globSet :: CPC s => CP s (Char -> Bool)
    globSet =
      do cs <- single '[' *> many globElem <* single ']'
         return $
           foldr uniSet empSet cs
      where
        uniSet s1 s2 = \ x -> s1 x || s2 x
        empSet       = const False

    globElem :: CPC s => CP s (Char -> Bool)
    globElem =
      do c1 <- noneOf' "]"
         c2 <- option c1 upperBound
         return (elemChar c1 c2)
      where
        upperBound = single '-' *> noneOf' "]"

-- --------------------

charCase :: Char -> (Char -> Bool)
charCase c = elemCase c c
{-# INLINE charCase  #-}

charNoCase :: Char -> (Char -> Bool)
charNoCase c = elemNoCase c c
{-# INLINE charNoCase  #-}

elemCase :: Char -> Char -> (Char -> Bool)
elemCase l u =
  case compare l u of
    EQ -> (== l)
    LT -> \x -> l <= x && x <= u
    GT -> const False

elemNoCase :: Char -> Char -> (Char -> Bool)
elemNoCase l u
  | l == u =                          -- single element interval
      if ll == ul                     -- no letter
      then (== l)
      else \x -> ll == x || x == ul   -- lower or upper case letter

  | lcase &&                          -- both bounds are lower case
    ucase &&                          -- or both are upper case
    ( bothLower ||
      bothUpper
    ) =
      \x -> (ll <= x && x <= lu) ||   -- both intervals (lower case and upper case)
            (ul <= x && x <= uu)      -- are checked

  | otherwise =                       -- interval is testet as given
      elemCase l u                    -- e.g. [A-z] contains '_'
  where
    ll = toLower l
    lu = toLower u
    ul = toUpper l
    uu = toUpper u

    lcase = ll /= ul
    ucase = lu /= uu

    bothLower = ll == l && lu == u && lcase && ucase
    bothUpper = ul == l && uu == u && lcase && ucase

-- ----------

-- case unsensitive checks for chars and intervals

-- >>> elemNoCase 'a' 'a' 'a'
-- True

-- >>> elemNoCase 'a' 'a' 'A'
-- True

-- >>> elemNoCase 'A' 'A' 'a'
-- True

-- >>> elemNoCase 'a' 'z' 'i'
-- True

-- >>> elemNoCase 'a' 'z' 'I'
-- True

-- >>> elemNoCase '1' '9' '5'
-- True

-- >>> elemNoCase '9' '1' '5'
-- False

-- >>> elemNoCase 'A' 'z' '_'
-- True

-- >>> elemNoCase 'z' 'A' 'A'
-- False

-- >>> elemNoCase 'z' 'A' 'z'
-- False

-- ----------------------------------------
--
-- glob style pattern matching: examples

-- >>> parseMaybe (fromMaybe mempty . parseMaybe parseGlobText $ "a") "a"
-- Just "a"

-- >>> parseMaybe (fromMaybe mempty . parseMaybe parseGlobText $ "a?") "ab"
-- Just "ab"

-- >>> parseMaybe (fromMaybe mempty . parseMaybe parseGlobText $ "a*") "a"
-- Just "a"

-- >>> parseMaybe (fromMaybe mempty . parseMaybe parseGlobText $ "a*") "abc"
-- Just "abc"

-- >>> parseMaybe (fromMaybe mempty . parseMaybe parseGlobText $ "a???") "abcd"
-- Just "abcd"

-- >>> parseMaybe (fromMaybe mempty . parseMaybe parseGlobText $ "a???") "abc"
-- Nothing

-- >>> parseMaybe (fromMaybe mempty . parseMaybe parseGlobText $ "?*.jpg") "x.jpg"
-- Just "x.jpg"

-- >>> parseMaybe (fromMaybe mempty . parseMaybe parseGlobText $ "[xyz].jpg") "y.jpg"
-- Just "y.jpg"

-- >>> parseMaybe (fromMaybe mempty . parseMaybe parseGlobText $ "[a-z].jpg") "y.jpg"
-- Just "y.jpg"

-- ----------------------------------------
