-- | classify file names and compute a file type for a file name/path

module Catalog.FilePath where

import Control.Applicative
import Data.ImageTree
import Data.Prim.Name
import Data.Prim.Prelude

-- ----------------------------------------

type FilePathConfig = [(Regex, ImgType)]

filePathToImgType :: FilePath -> NameImgType
filePathToImgType = fst . fpToImgType filePathConfig

filePathToExt :: ImgType -> FilePath -> Name
filePathToExt ty = snd . fpToImgType fpc
  where
    fpc = filter ((== ty) . snd) filePathConfig

fpToImgType :: FilePathConfig -> FilePath -> (NameImgType, Name)
fpToImgType conf path =
  fromMaybe ((mkName path, IMGother), emptyName) $
  foldr1 (<|>) $
  map parse conf
  where
    parse (re, ty) =
      partRes $  matchSubexRE re path
      where

        partRes [("1", base)] =
          Just ((mkName base, ty), emptyName)
        partRes [("1", base), ("2", ext)] =
          Just ((mkName base, ty), mkName ext)
        partRes _ =
          Nothing

filePathConfig :: FilePathConfig
filePathConfig = map (first parseRegexExt) $
  [ (mk1 boringName,               IMGboring)
  , (mk1 baseName  ++ mk2 rawExt,  IMGraw)
  , (mk1 baseName' ++ mk2 imgExt,  IMGimg)
  , (mk1 baseName  ++ mk2 metaExt, IMGmeta)
  , (mk1 baseName  ++ mk2 jsonExt, IMGjson)
  , (mk1 jpgdirName,               IMGjpgdir)
  , (mk1 imgdirName,               IMGimgdir)
  , (jpgdirPre
     ++ mk1 baseName
     ++ mk2 (geoExt ++ jpgExt),    IMGcopy)
  , (jpgdirPre
     ++ mk1 baseName
     ++ mk2 jpgExt,                IMGjpg)
  ]
  where
    mk1  e = "({1}(" ++ e ++ "))"
    mk2  e = "({2}(" ++ e ++ "))"

    baseName  = "[-._A-Za-z0-9]+"
    baseName' = "[-._A-Za-z0-9]+"
    rawExt    = "[.](nef|NEF||rw2|RW2)"
    imgExt    = "[.](jpp|JPG|gif|tiff|ppm|pgm|pbm)"
    metaExt   = "[.](xmp|((nef|NEF|rw2|RW2|jpg|JPG)[.]dop))"
    jsonExt   = "[.](json)"
    jpgExt    = "[.](jpg|JPG)"
    geoExt    = "[.]([0-9]+x[0-9]+)"

    imgdirName = "[-_A-Za-z0-9]+" -- no do
    jpgdirName =
      "("
      ++ ( intercalate "|"
           [ "srgb[0-9]*"
           , "srgb-bw"
           , "[0-9]+x[0-9]+"
           , "dxo"
           , "small"
           , "web"
           , "bw"
           ]
         )
      ++ ")"

    jpgdirPre =
      "(" ++ jpgdirName ++ "/)?"

    boringName = intercalate "|"
      [ "[.].*"
      , ".*~"
      , "tmp.*"
      , ".*[.](bak|old|tiff|dng)"
      ]

-- ----------------------------------------
