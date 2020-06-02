module Text.SimpleParser
  ( module Text.Megaparsec
  , module Text.Megaparsec.Char
  , module Text.SimpleParser
  )
where

import Data.Prim.Prelude
import Data.Void

import Text.Megaparsec                ( Parsec
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
-- Data.Prim.Prelude reexports lenses,
-- somewhere in lenses oneOf and noeOf are defined
-- hiding these in import for Prelude
-- gives a silly warning: Prelude does not export oneOf nor noneOf
--
-- so noneOf and oneOf parsers are imported qualified
-- and renamed to oneOf' and noeOf'
--
import qualified Text.Megaparsec as P (oneOf, noneOf)

-- not used locally but convenient for users
--
import Text.Megaparsec.Char

-- --------------------
--
-- simple String parser

type SP = Parsec Void String

-- --------------------

infixr 5 <++>

(<++>) :: (Applicative f, Semigroup b) => f b -> f b -> f b
p1 <++> p2 = (<>) <$> p1 <*> p2

{-# INLINE (<++>) #-}

anyStringThen' :: SP a -> SP (String, a)
anyStringThen' p =
  try ( do ps <- p
           return ("", ps)
      )
  <|>
  do c        <- anySingle
     (cs, ps) <- anyStringThen' p
     return (c : cs, ps)

-- substitute for regex ".*xxx"
--
-- example: split a file extension from a path
--
-- parseMaybe anyStringThen' (string ".jpg" <* eof) "abc.jpgf.jpg"
--    -> Just ("abc.jpgf", ".jpg")

anyStringThen :: SP String -> SP String
anyStringThen p = uncurry (++) <$> anyStringThen' p

splitSuffix :: SP a -> SP (String, a)
splitSuffix p = anyStringThen' (p <* eof)

withSuffix :: SP String -> SP String
withSuffix p = uncurry (++) <$> splitSuffix p

anyString :: SP String
anyString = many anySingle

-- rename noneOf, it's defined somewhere in lens
noneOf' :: String -> SP Char
noneOf' = P.noneOf

oneOf' :: String -> SP Char
oneOf' = P.oneOf

ssp :: SP String
ssp = some (single ' ')

msp :: SP String
msp = many (single ' ')

manyChars :: SP String
manyChars = many anySingle

someChars :: SP String
someChars = some anySingle

-- --------------------

sedParser :: (a -> String) -> SP a -> SP String
sedParser edit p = go
  where
    go = try ((edit <$> p) <++> go)
         <|>
         ((:) <$> anySingle <*> go)
         <|>
         return ""

-- --------------------

sedP :: (a -> String) -> SP a -> String -> String
sedP ef p = fromMaybe "" . parseMaybe (sedParser ef p)

matchP :: SP a -> String -> Bool
matchP p = toBool . parseMaybe p

toBool :: Maybe a -> Bool
toBool = maybe False (const True)

matchPred :: (a -> Bool) -> a -> Maybe a
matchPred p x
  | p x       = Just x
  | otherwise = Nothing

eqNoCase :: String -> String -> Bool
eqNoCase = (==) `on` map toLower

-- ----------------------------------------
--
-- parser for glob style wildcards
--
-- parseGlob is a parser for glob style wildcards
-- input is a glob style pattern
-- output is a parser for pattern matching against the pattern

type GlobParser = SP String

parseGlob :: SP GlobParser
parseGlob =
  do _ <- single '*'
     p <- parseGlob
     return (anyStringThen p)
  <|>
  do _ <- single '?'
     p <- parseGlob
     return ( do x  <- anySingle
                 xs <- p
                 return (x : xs)
            )
  <|>
  do inSet <- globSet
     p     <- parseGlob
     return ( do x  <- satisfy inSet
                 xs <- p
                 return (x : xs)
            )
  <|>
  do c <- anySingle
     p <- parseGlob
     return ( do x  <- single c
                 xs <- p
                 return (x : xs)
            )
  <|>
  eof *> return (eof *> return "")

globSet :: SP (Char -> Bool)
globSet =
  do cs <- single '[' *> many globElem <* single ']'
     return $
       foldr uniSet empSet cs
  where
    uniSet s1 s2 = \ x -> s1 x || s2 x
    empSet       = const False

globElem :: SP (Char -> Bool)
globElem =
  do c1 <- noneOf' "]"
     c2 <- option c1 upperBound
     return (\ x -> x >= c1 && x <= c2)
  where
    upperBound = single '-' *> noneOf' "]"

-- ----------------------------------------
