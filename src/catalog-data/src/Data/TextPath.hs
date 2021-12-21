{-# LANGUAGE
    ConstraintKinds,
    DataKinds,
    FlexibleContexts,
    GADTs,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)

{-# LANGUAGE OverloadedStrings #-}

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
-- Catalog.FilePath containing all parsing ops with FileName
-- as input

module Data.TextPath
  ( TextPath
  , ClassifiedName
  , ClassifiedNames
  , classifyPath
  , classifyPaths
  , path2MimeType
  , isImgCopiesDir

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

import Data.Prim

import qualified Data.FilePath    as F ( splitPathNameExtMime
                                       , isImgCopiesDir
                                       , addJpg
                                       , ymdNameMb
                                       , baseNameMb
                                       )

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
classifyPath tp = (isoText # tp, (isoString # bn, mimeType))
  where
    ((_p, (bn, _bx), _ex), mimeType) = F.splitPathNameExtMime (tp ^. isoString)

path2MimeType :: TextPath -> MimeType
path2MimeType = snd . F.splitPathNameExtMime . (^. isoString)

isImgCopiesDir :: TextPath -> Bool
isImgCopiesDir p = F.isImgCopiesDir (p ^. isoString)

-- ----------------------------------------
--
-- path to/from file path operations

addExt :: Text -> TextPath -> TextPath
addExt ext fn
  | ext `T.isSuffixOf` fn = fn
  | otherwise             = fn <> ext

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
addJpg p =
  p & isoString %~ F.addJpg

ymdNameMb :: TextPath -> Maybe (Text, Maybe (Text, Maybe Text))
ymdNameMb p =
  fmap (T.pack *** fmap (T.pack *** fmap T.pack)) $ (F.ymdNameMb (T.unpack p))

baseNameMb :: Text -> Maybe Text
baseNameMb p =
  fmap T.pack $ F.baseNameMb (T.unpack p)

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
