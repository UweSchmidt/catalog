-- | classify file names and compute a file type for a file name/path

module Data.FilePath
  ( splitPathNameExtMime
  , isImgCopiesDir
  , addJpg
  , ymdNameMb
  , baseNameMb
  )
where

import Data.Prim
       ( Alternative(many, (<|>), some)
       , MonadPlus(mzero)
       , optional
       , fromMaybe
       , isJust
       , isAlphaNum
       , imgMimeExt'
       , isJpgMT
       , MimeType(Unknown'mime_type)
       , ps'bycreatedate
       )

import Text.SimpleParser
       ( SP
       , (<++>)
       , anyStringThen'
       , atleast'ntimes
       , lowerOrUpperCaseWord
       , ntimes
       , someChars
       , parseMaybe
       , satisfy
       , single
       , char
       , digitChar
       , letterChar
       , lowerChar
       , upperChar
       , string
       , option
       , count
       , try
       , eof
       )

import qualified Text.SimpleParser          as SP

-- ----------------------------------------
--

p'geo :: SP String
p'geo = some digitChar <++> string "x" <++> some digitChar

-- "/archive/collections/photos" -> "photos"
baseNameParser :: SP String
baseNameParser =
  char '/' *> many (try $ anyStringThen' (char '/')) *> someChars

baseNameMb :: String -> Maybe String
baseNameMb = parseMaybe baseNameParser

-- "/archive/collections/byCreateDate/2000/12/24" -> "2000", "12", "24"
-- "/archive/collections/byCreateDate/2000/12"    -> "2000", "12"
-- "/archive/collections/byCreateDate/2000"       -> "2000"

ymdParser :: SP (String, Maybe (String, Maybe String))
ymdParser = do
  y  <- string ps'bycreatedate *>
        char '/' *>
        count 4 digitChar
  md <- optional $ do
        m <- char '/' *>
             count 2 digitChar
        d <- optional $ char '/' *>
                        count 2 digitChar
        return (m, d)
  return (y, md)

ymdNameMb :: String -> Maybe (String, Maybe (String, Maybe String))
ymdNameMb = parseMaybe ymdParser

-- --------------------

addJpg :: String -> String
addJpg fn
  | isJ fn  = fn
  | otherwise = fn <> ".jpg"
  where
    isJ = isJpgMT . snd . splitPathNameExtMime

-- ----------------------------------------

camName :: SP String
camName = (<>) <$> (try cn1 <|> cn2) <*> cno
  where
    cn = try (ntimes 3 ucn) <|> ntimes 3 lcn

    cn1 = (:)                  <$> single '_' <*> cn
    cn2 = (\xs x -> xs <> [x]) <$> cn         <*> single '_'

    cno = atleast'ntimes 4 digitChar
    ucn = upperChar <|> digitChar
    lcn = lowerChar <|> digitChar

camSuffix :: SP String
camSuffix =
  option mempty $
  (:) <$> c1 <*> some cc
  <|>
  (:) <$> single '.' <*> some cc
  where
    c' :: String
    c' = "_-."
    c1 = satisfy (`elem` c')
    cc = letterChar <|> digitChar <|> c1

imgName :: SP (String, String)
imgName =
  (,) <$> camName <*> camSuffix
  <|>
  (,mempty) <$> some (satisfy imgChar)
  where
    imgChar :: Char -> Bool
    imgChar c =
      isAlphaNum c
      ||
      c `elem` ichars
      where
        ichars :: String
        ichars = "_-."

-- special names for subdirs containing developped images
-- examples:
--   srgb
--   srgb123
--   srgb-bw
--   srgb-800x600
--   srgb-bw-800x600
--   800x600
--   bw, web, small, jpg, tiff, tif, dng, dxo
--   bw1000
--   bw1000x1000
--   bw1920x1200
--   bw-1920x1200

imgSubdir :: SP String
imgSubdir =
  try p'geo
  <|>
  try ( foldl1 (<|>) (
          map (try . string)   -- first "tiff", then "tif" !!!
          [ "srgb-bw", "srgb", "small"
          , "web"
          , "bw"
          , "jpg"
          , "tiff", "tif"
          , "dng"
          , "dxo"
          ])
        <++> og
      )
  where
    og :: SP String
    og = ( string "-" <|> return "" )
         <++>
         SP.option "" (try
           ( some digitChar
             <++>
             SP.option "" (try (string "x" <++> some digitChar))
           ))

type SplitName = (String, (String, String), String)

-- ----------------------------------------
-- test code

 {-

joinPathNameExt :: SplitName -> String
joinPathNameExt (d, (n, n'), e) = d <> n <> n' <> e

cmpSplitName :: SplitName -> SplitName -> Ordering
cmpSplitName =
  (compare `on` (^. _2 . _1))
  <>
  (compare `on` (^. _2 . _2))
  <>
  (compare `on` (^. _3))
  <>
  (compare `on` (^. _1))

tsplitJoin :: String -> Bool
tsplitJoin xs = xs == (joinPathNameExt . fst . splitPathNameExtTypeD $ xs)

tsplit :: String -> IO ()
tsplit xs = do
  sequence_ $ map putStrLn ns2
  putStrLn "-- boring names"
  sequence_ $ map putStrLn bs2
  where
    ns2 = map fmt $ sortBy (cmpSplitName `on` fst) ns
          where
            fmt ((d, (n, n'), e), ex) =
              unwords [n, d <> n <> n' <> e, show ex]
    bs2 = sort $ map fmt bs
          where
            fmt ((_d, (n, _n'), _e), _ex) = n

    (bs, ns) = partition ((== Unknown'mime_type) . snd) $
               map splitPathNameExtMime ls
    ls = lines xs

-- -}
-- ----------------------------------------

imgMimeLT :: [(String, MimeType)]
imgMimeLT = concatMap f imgMimeExt'
  where
    f (t, es) = map (,t) es

splitExtMimeP :: SP (String, (String, MimeType))
splitExtMimeP = anyStringThen' $ pext <* eof
  where
    pext      = foldr ((<|>) . pe) mzero imgMimeLT
    pe (e, t) = (,t) <$> try (lowerOrUpperCaseWord e)

splitPathNameExtMimeP :: SP (SplitName, MimeType)
splitPathNameExtMimeP = do
  p            <- path'
  (nm, (e, t)) <- splitExtMimeP
  let n        =  fromMaybe (nm, mempty) $ parseMaybe imgName nm
  return ((p, n, e), t)
  where
    p' :: SP String
    p' = try $ (<>) <$> many (satisfy (/= '/'))
                    <*> some (satisfy (== '/'))

    path' :: SP String
    path' = concat <$> many p'

isImgCopiesDir :: String -> Bool
isImgCopiesDir = isJust . parseMaybe imgSubdir

splitPathNameExtMime :: String -> (SplitName, MimeType)
splitPathNameExtMime n =
  fromMaybe ((mempty, (n, mempty),mempty), Unknown'mime_type) $
  parseMaybe splitPathNameExtMimeP n

-- ----------------------------------------
