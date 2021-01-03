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
  , classifyPath
  , classifyPaths
  , pathName2ImgType

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

import qualified Data.FilePath    as F ( splitPathNameExtTypeD
                                       , addJpg
                                       , ymdNameMb
                                       , baseNameMb
                                       )

import qualified Data.Text        as T
import qualified System.FilePath  as FP

------------------------------------------------------------------------------

type TextPath = Text

type ClassifiedName = (Name, (Name, ImgType))

-- classify paths: compute base name and type
-- and remove boring names
classifyPaths :: [TextPath] -> [ClassifiedName]
classifyPaths = filter (not . isBoring . snd . snd) . map classifyPath

classifyPath :: TextPath -> ClassifiedName
classifyPath tp = (isoText # tp, (isoString # bn, imgType))
  where
    ((_p, (bn, _bx), _ex), imgType) = F.splitPathNameExtTypeD (tp ^. isoString)

pathName2ImgType :: TextPath -> ImgType
pathName2ImgType = snd . snd . classifyPath

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
--  -}
