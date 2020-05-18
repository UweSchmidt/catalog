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

module Catalog.Data.TextPath
  ( textPathToImgType
  , textPathToExt
  , dropVirtualCopyNo
  , splitAbsPath
  , joinAbsPath
  , splitExt
  , joinExt
  , splitDirFileExt
  , splitDirFileExt2
  , matchExt
  , matchExts

  , imgTypeExt
  , extImg
  , extJpg
  , extTxt
  , extVideo
  , extRaw
  , extDng
  , extMeta
  , extDxO
  , extPto
  , extJson

  , imgFiles
  , jpgFiles
  , txtFiles
  , movieFiles
  , rawFiles
  , dngFiles
  , metaFiles
  , dxoFiles
  , huginFiles
  , jsonFiles
  , pathName2ImgType
  , addExt

  -- Text versions of System.FilePath functions
  , splitExtension
  , takeDir
  , takeBaseName

  , F.splitLast
  , F.joinLast

  , (<//>)
  )
where

import Data.Prim

import qualified Catalog.FilePath as F

import qualified Data.Text        as T
import qualified System.FilePath  as FP

------------------------------------------------------------------------------

type TextPath = Text

-- ----------------------------------------
--
-- the main entry points to file path classification
--
-- used in syncing catalog with file system

textPathToImgType :: TextPath -> NameImgType
textPathToImgType = F.filePathToImgType . T.unpack


textPathToExt :: ImgType -> TextPath -> Name
textPathToExt ty = F.filePathToExt ty . T.unpack

-- ----------------------------------------

dropVirtualCopyNo :: Text -> Text
dropVirtualCopyNo s = s & isoString %~ F.dropVirtualCopyNo

-- ----------------------------------------

splitAbsPath :: TextPath -> Maybe [Text]
splitAbsPath p =  fmap T.pack <$> F.splitAbsPath (T.unpack p)

joinAbsPath :: [Text] -> TextPath
joinAbsPath = mconcat . map ("/" <>)

-- --------------------

-- split a filename into basename and list of extensions
--
-- splitExt "abc.def"     -> Just ["abc", ".def"]
-- splitExt "abc.def.ghi" -> Just ["abc", ".def", ".ghi"]
-- splitExt "abc"         -> Nothing
-- splitExt "abc"         -> Nothing
-- splitExt ".iii"        -> Nothing
-- splitExt "abc..ii"     -> Nothing

splitExt :: TextPath -> Maybe [Text]
splitExt p =
  (fmap T.pack) <$> F.splitExt (T.unpack p)

joinExt :: [Text] -> Text
joinExt = mconcat

-- splitDirFileExt "/xxx/abc.jpg" -> Just ("/xxx","abc",".jpg")
-- splitDirFileExt "/abc.jpg"     -> Just ("","abc",".jpg")
-- splitDirFileExt "/abc.txt.jpg" -> Just ("","abc.txt",".jpg")

splitDirFileExt :: TextPath -> Maybe (TextPath, TextPath, Text)
splitDirFileExt p =
  (map3 T.pack) <$> F.splitDirFileExt (T.unpack p)
  where
    map3 f (x1, x2, x3) = (f x1, f x2, f x3)


-- splitDirFileExt2 "/abc.txt.jpg" -> Just ("","abc",".jpg",".txt")
-- splitDirFileExt2 "/abc.jpg"     -> Nothing

splitDirFileExt2 :: TextPath -> Maybe (TextPath, TextPath, Text, Text)
splitDirFileExt2 p =
  (map4 T.pack) <$> F.splitDirFileExt2 (T.unpack p)
  where
    map4 f (x1, x2, x3, x4) = (f x1, f x2, f x3, f x4)

matchExt :: ImgType -> Text -> TextPath -> Maybe ImgType
matchExt ty ex p = F.matchExt ty (T.unpack ex) (T.unpack p)

matchExts :: ImgType -> [Text] -> TextPath -> Maybe ImgType
matchExts ty exs p = F.matchExts ty (fmap T.unpack exs) (T.unpack p)


-- used in servant server

extImg
  , extJpg
  , extTxt
  , extVideo
  , extRaw
  , extDng
  , extMeta
  , extDxO
  , extPto
  , extJson :: Text -> Maybe ImgType

[ extImg
  , extJpg
  , extTxt
  , extVideo
  , extRaw
  , extDng
  , extMeta
  , extDxO
  , extPto
  , extJson
  ] = map (uncurry matchExts) imgTypeExt

-- sort extensions by length: ".tiff" before ".tif"
-- else backtracking with try does not work properly

imgTypeExt :: [(ImgType, [Text])]
imgTypeExt = fmap (second (fmap T.pack)) F.imgTypeExt


lookupExt :: ImgType -> [Text]
lookupExt = fmap T.pack . F.lookupExt

imgFiles
  , jpgFiles
  , txtFiles
  , movieFiles
  , rawFiles
  , dngFiles
  , metaFiles
  , dxoFiles
  , huginFiles
  , jsonFiles :: [Text]

imgFiles   = lookupExt IMGimg
jpgFiles   = lookupExt IMGjpg
txtFiles   = lookupExt IMGtxt
movieFiles = lookupExt IMGmovie
rawFiles   = lookupExt IMGraw
dngFiles   = lookupExt IMGdng
metaFiles  = lookupExt IMGmeta
dxoFiles   = lookupExt IMGdxo
huginFiles = lookupExt IMGhugin
jsonFiles  = lookupExt IMGjson


pathName2ImgType :: TextPath -> ImgType
pathName2ImgType = F.fileName2ImgType . T.unpack

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
