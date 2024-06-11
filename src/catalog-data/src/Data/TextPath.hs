------------------------------------------------------------------------------
--
-- TextPath parsing
--
-- TextPath is an alias for Text,
-- it's used a a substitute for FilePath
-- to avoid Strings for representing file names and file paths
--
-- As long as there is a MTL version of the catalog modules
-- and a Polysemy version, this is a wrapper to module
-- Data.FilePath containing all parsing ops with FileName
-- as input

module Data.TextPath
  ( TextPath
  , ClassifiedName
  , ClassifiedNames
  , classifyPath
  , classifyPaths
  , path2MimeType
  , isImgCopiesDir
  , imgNames

  , addExt
  , addJpg

  -- Text versions of System.FilePath functions
  , splitExtension
  , takeDir
  , takeBaseName

  , ymdNameMb
  , baseNameMb

  , (<//>)
  )
where

import Data.Prim.Constants ( p'bycreatedate )

import Data.Prim.ImageType
    ( MimeType(Unknown'mime_type), isJpgMT, isBoringMT, imgMimeExt' )

import Data.Prim.Name ( Name )

import Data.Prim.Prelude
    ( Text,
      Alternative(many, some, (<|>)),
      fromMaybe,
      isJust,
      MonadPlus(mzero),
      optional,
      (&),
      (^.),
      (#),
      (%~),
      both,
      isAlphaNum,
      isDigit,
      IsoText(isoText),
      IsoString(isoString) )

import Text.SimpleParser
    ( CPC,
      CP,
      try,
      eof,
      parseMaybe,
      option,
      satisfy,
      single,
      anySingle,
      TP,
      (<++>),
      anyStringThen',
      someChars,
      digitChar,
      lowerOrUpperCaseWord,
      ntimes,
      nChars,
      atleast'ntimes,
      char,
      lowerChar,
      upperChar,
      string )

import qualified Data.Text        as T
import qualified System.FilePath  as FP

------------------------------------------------------------------------------

-- alias for filepaths as Text values
type TextPath        = Text

type ClassifiedName  = (Name, (Name, MimeType))
type ClassifiedNames = [ClassifiedName]


-- classify paths: compute base name and type
-- and remove boring names
classifyPaths :: [TextPath] -> [ClassifiedName]
classifyPaths = filter (not . isBoringMT . snd . snd) . map classifyPath

classifyPath :: TextPath -> ClassifiedName
classifyPath tp = (isoText # tp, (isoText # bn, mimeType))
  where
    ((_p, (bn, _bx), _ex), mimeType) = splitPathNameExtMime tp

path2MimeType :: TextPath -> MimeType
path2MimeType = snd . splitPathNameExtMime

imgNames :: [ClassifiedNames] -> [Name]
imgNames = map (fst . snd) . concatMap (take 1)

isImgCopiesDir :: Text -> Bool
isImgCopiesDir = isJust . parseMaybe imgSubdir

splitPathNameExtMime :: Text -> (SplitName, MimeType)
splitPathNameExtMime n =
  fromMaybe ((mempty, (n, mempty), mempty), Unknown'mime_type) $
    parseMaybe splitPathNameExtMimeP n

-- ----------------------------------------
--
-- path to/from file path operations

addExt :: Text -> TextPath -> TextPath
addExt ext fn
  | ext `T.isSuffixOf` fn = fn
  | otherwise = fn <> ext

-- >>> addExt ".xyz" "/abc/def"
-- "/abc/def.xyz"

-- >>> addExt ".xyz" "/abc/def.xyz"

-- ----------------------------------------
--
-- filr path parsers

type SplitName = (Text, (Text, Text), Text)

splitPathNameExtMimeP :: TP (SplitName, MimeType)
splitPathNameExtMimeP = do
  p <- path'
  (nm, (e, t)) <- splitExtMimeP
  let n = fromMaybe (nm, mempty) $ parseMaybe imgName nm
  return ((p, n, e), t)
  where
    p' :: TP Text
    p' =
      T.pack <$>
      try ( (<>)
            <$> many (satisfy (/= '/'))
            <*> some (satisfy (== '/'))
          )

    path' :: TP Text
    path' = T.concat <$> many p'

splitExtMimeP :: TP (Text, (Text, MimeType))
splitExtMimeP = anyStringThen' $ pext <* eof
  where
    pext = foldr ((<|>) . pe) mzero imgMimeLT
    pe (e, t) = (,t) <$> try (lowerOrUpperCaseWord e)

-- --------------------
--
-- names for image subdirectories
-- containing processed copies e.g. of a raw image

imgSubdir :: TP Text
imgSubdir =
  try p'geo
  <|>
  try ( foldl1 (<|>)
        ( map
          (try . string) -- first "tiff", then "tif" !!!
          [ "srgb-bw",
            "srgb",
            "small",
            "web",
            "bw",
            "jpg",
            "tiff",
            "tif",
            "dng",
            "dxo"
          ]
        )
        <++> og
      )
  where
    og :: TP Text
    og =
      option mempty (string "-")
      <++>
      option mempty ( try p'geo
                      <|>
                      try (someChars digitChar)
                    )

p'geo :: TP Text
p'geo = someChars digitChar <++> string "x" <++> someChars digitChar

-- --------------------
--
-- img names are partitions into
-- camera generated names with a fixed syntax
-- of 3 letters, '_' and 4 digits plus extension
-- and other image names with letters, digits or '_', '-', '.'
--
-- camera generated images, there are usually
-- various files with same name and different extensions or suffixes
-- theses files are grouped together to a single (logical) image entry

-- examples

-- >>> parseMaybe imgName ("dsc_1234.nef")
-- >>> parseMaybe imgName ("DSC_1234.NEF")
-- >>> parseMaybe imgName ("dsc_1234.jpg")
-- >>> parseMaybe imgName ("dsc_1234.xmp")
-- >>> parseMaybe imgName ("dsc_1234_M.tiff")
-- >>> parseMaybe imgName ("dsc_1234_1.tiff")
-- >>> parseMaybe imgName ("dsc_1234-8.jpg")

-- >>> parseMaybe imgName ("dog.jpg")
-- >>> parseMaybe imgName ("dog_and_cat.jpg")

imgName :: TP (Text, Text)
imgName =
  try ((,) <$> camName <*> camSuffix)
  <|>
  (,mempty) . T.pack <$> some imgChar

camName :: TP Text
camName =
  T.pack <$> ((<>) <$> (try cn1 <|> cn2) <*> cno)
  where
    cn = try (ntimes 3 ucn) <|> ntimes 3 lcn

    cn1 = (:) <$> single '_' <*> cn
    cn2 = (\xs x -> xs <> [x]) <$> cn <*> single '_'

    cno = atleast'ntimes 4 digitChar
    ucn = upperChar <|> digitChar
    lcn = lowerChar <|> digitChar

camSuffix :: TP Text
camSuffix =
  T.pack <$>
  option mempty ((:) <$> imgDel <*> some imgChar)

imgDel :: CPC s => CP s Char
imgDel = satisfy (`elem` ("_-." :: String))

imgChar :: (CPC s) => CP s Char
imgChar = satisfy isAlphaNum <|> imgDel

imgMimeLT :: [(Text, MimeType)]
imgMimeLT = concatMap f imgMimeExt'
  where
    f (t, es) = map (,t) (map T.pack es)

-- ----------------------------------------

infixr 5 <//>

(<//>) :: TextPath -> TextPath -> TextPath
p1 <//> p2 = p1 <> "/" <> p2

splitExtension :: TextPath -> (TextPath, Text)
splitExtension p =
  FP.splitExtension (p ^. isoString)
     & both %~ (isoString #)

takeDir :: TextPath -> TextPath
takeDir p =
   p & isoString %~ FP.takeDirectory

takeBaseName :: TextPath -> TextPath
takeBaseName p =
  p & isoString %~ FP.takeFileName

------------------------------------------------------------------------------

addJpg :: TextPath -> TextPath
addJpg fn
  | isJ fn = fn
  | otherwise = fn <> ".jpg"
  where
    isJ = isJpgMT . snd . splitPathNameExtMime

-- >>> addJpg "/abc/def"
-- "/abc/def.jpg"

-- >>> addJpg "/abc/def.jpg"
-- "/abc/def.jpg"

-- >>> addJpg "/abc/def.png"
-- "/abc/def.png.jpg"

-- --------------------

ymdNameMb :: TextPath -> Maybe (Text, Maybe (Text, Maybe Text))
ymdNameMb = parseMaybe ymdParser

ymdParser :: TP (Text, Maybe (Text, Maybe Text))
ymdParser = do
  y <- do _ <- string $ p'bycreatedate ^. isoText
          _ <- char '/'
          nChars 4 isDigit

  md <- optional $
        do _ <- char '/'
           m <- nChars 2 isDigit
           d <- optional $
                do _ <- char '/'
                   nChars 2 isDigit
           return (m, d)
  return (y, md)

-- >>> ymdNameMb (p'bycreatedate ^. isoText <> "/2024/06/11")
-- Just ("2024",Just ("06",Just "11"))

-- --------------------

baseNameMb :: Text -> Maybe Text
baseNameMb = parseMaybe baseNameParser

--  T.pack <$> F.baseNameMb (T.unpack p)

-- "/archive/collections/photos" -> "photos"
baseNameParser :: TP Text
baseNameParser =
  char '/' *> many (try $ anyStringThen' (char '/')) *> someChars anySingle


-- >>> baseNameMb "/archive/collections/photos"
-- >>> baseNameMb  "archive/collections/photos"
-- >>> baseNameMb "/archive/collections/photos/"

------------------------------------------------------------------------------
 {-

testC :: IO ()
testC = do
  c <- readFile "/Users/uwe/tmp/tnames"
  let c = unlines . map (^. isoString) $ files
  sequence_ $ map putStrLn (toC c)

toC :: String -> [String]
toC c = map fmt . sortBy cmp $ map classifyPath ts
  where
    ts :: [TextPath]
    ts = map (isoString #) $ lines c

    fmt (n, (bn, t)) = unwords [show bn, show n, show t]
    cmp = (compare `on` (^. _2 . _1))
          <>
          (compare `on` (^. _1))

files :: [TextPath]
files = [ ".DS_Store"
        , "index.md"
        , "tiff"
        , "sub-dir"
        , "srgb-bw5"
        , "tiff/.DS_Store"
        , "tiff/uz6_4004-07.afphoto"
        , "tiff/uz6_4063-64-Nik.tiff"
        , "tiff/uz6_4063-64.afphoto"
        , "tiff/uz6_4063-64.tiff"
        , "tiff/uz6_4063.tif"
        , "tiff/uz6_4064.tif"
        , "tiff/uz6_4065.tif"
        , "tiff/uz6_4066.tif"
        , "tiff/uz6_4067.tif"
        , "tiff/uz6_4068.tif"
        , "tiff/uz6_4069-72-Nik.tiff"
        , "tiff/uz6_4069-72.afphoto"
        , "tiff/uz6_4069-72.tiff"
        , "tiff/uz6_4069.tif"
        , "tiff/uz6_4070.tif"
        , "tiff/uz6_4071.tif"
        , "tiff/uz6_4072.tif"
        , "tiff/uz6_4073.tif"
        , "tiff/uz6_4074.tif"
        , "tiff/uz6_4075.tif"
        , "tiff/uz6_4076-80-Nik.afphoto"
        , "tiff/uz6_4076-80-Nik.tiff"
        , "tiff/uz6_4076-80.afphoto"
        , "tiff/uz6_4076-80.tiff"
        , "tiff/uz6_4076.tif"
        , "tiff/uz6_4077.tif"
        , "tiff/uz6_4078.tif"
        , "tiff/uz6_4079.tif"
        , "tiff/uz6_4080.tif"
        , "tiff/uz6_4081-84-Nik.afphoto"
        , "tiff/uz6_4081-84-Nik.tiff"
        , "tiff/uz6_4081-84.afphoto"
        , "tiff/uz6_4081-84.tiff"
        , "tiff/uz6_4081.tif"
        , "tiff/uz6_4082.tif"
        , "tiff/uz6_4083.tif"
        , "tiff/uz6_4084.tif"
        , "tiff/uz6_4085.tif"
        , "tiff/uz6_4086-88-0.tiff"
        , "tiff/uz6_4086-88-1.tiff"
        , "uz6_4063.nef"
        , "uz6_4063.nef.dop"
        , "uz6_4064.nef"
        , "uz6_4064.nef.dop"
        , "uz6_4065.nef"
        , "uz6_4065.nef.dop"
        , "uz6_4066.nef"
        , "uz6_4066.nef.dop"
        , "uz6_4067.nef"
        , "uz6_4067.nef.dop"
        , "uz6_4068.nef"
        , "uz6_4068.nef.dop"
        , "uz6_4069.nef"
        , "uz6_4069.nef.dop"
        , "uz6_4070.nef"
        , "uz6_4070.nef.dop"
        , "uz6_4071.nef"
        , "uz6_4071.nef.dop"
        , "uz6_4072.nef"
        , "uz6_4072.nef.dop"
        , "uz6_4073.nef"
        , "uz6_4073.nef.dop"
        , "uz6_4074.nef"
        , "uz6_4074.nef.dop"
        , "uz6_4075.nef"
        , "uz6_4075.nef.dop"
        , "uz6_4076.nef"
        , "uz6_4076.nef.dop"
        , "uz6_4077.nef"
        , "uz6_4077.nef.dop"
        , "uz6_4078.nef"
        , "uz6_4078.nef.dop"
        , "uz6_4079.nef"
        , "uz6_4079.nef.dop"
        , "uz6_4080.nef"
        , "uz6_4080.nef.dop"
        , "uz6_4081.nef"
        , "uz6_4081.nef.dop"
        , "uz6_4082.nef"
        , "uz6_4082.nef.dop"
        , "uz6_4083.nef"
        , "uz6_4083.nef.dop"
        , "uz6_4084.nef"
        , "uz6_4084.nef.dop"
        , "uz6_4085.nef"
        , "uz6_4085.nef.dop"
        , "uz6_4086.nef"
        , "uz6_4086.nef.dop"
        , "uz6_4087.nef"
        ]
--  -}
