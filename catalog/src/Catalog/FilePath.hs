-- | classify file names and compute a file type for a file name/path

module Catalog.FilePath where

import Control.Applicative
import Data.Prim

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
  fromMaybe ((mkName path, IMGother), mempty) $
  foldr1 (<|>) $
  map parse conf
  where
    parse (e, ty) =
      partRes $  matchSubexRE e path
      where

        partRes [("1", base)] =
          Just ((mkName base, ty), mempty)
        partRes [("1", base), ("2", ext)] =
          Just ((mkName base, ty), mkName ext)
        partRes _ =
          Nothing

filePathConfig :: FilePathConfig
filePathConfig = map (first parseRegexExt) $
  [ (mk1 boringName,               IMGboring)
  , (mk1 baseName  ++ mk2 rawExt,  IMGraw)
  , (mk1 baseName  ++ mk2 jpgExt,  IMGjpg)  -- this jpg is the original image
  , (mk1 baseName  ++ mk2 imgExt,  IMGimg)
  , (mk1 baseName  ++ mk2 xmpExt,  IMGmeta)
  , (mk1 baseName  ++ mk2 jsonExt, IMGjson)
  , (mk1 baseName  ++ mk2 ptoExt,  IMGhugin)
  , (mk1 baseName  ++ mk2 dxoExt,  IMGdxo)
  , (mk1 baseName  ++ mk2 txtExt,  IMGtxt)
  , (mk1 baseName  ++ mk2 dngExt,  IMGdng)
  , (mk1 jpgdirName,               IMGjpgdir)
  , (mk1 imgdirName,               IMGimgdir)
  , (jpgdirPre
     ++ mk1 baseName
     ++ mk2 (geoExt ++ jpgExt),    IMGcopy)
  , (jpgdirPre
     ++ mk1 baseName
     ++ mk2 jpgExt,                IMGjpg)  -- these jpgs are developed from a raw image
  , (jpgdirPre
     ++ mk1 baseName
     ++ mk2 dngExt,                IMGdng)  -- these dngs are developed from a raw image
  ]
  where
    mk1  e = "({1}(" ++ e ++ "))"
    mk2  e = "({2}(" ++ e ++ "))"

    baseName  = "[-+._A-Za-z0-9]+"
    rawExt    = "[.](nef|NEF|rw2|RW2)"
    imgExt    = "[.](gif|tiff?|png|ppm|pgm|pbm)"
    xmpExt    = "[.](xmp|XMP)"
    dxoExt    = "[.]((nef|NEF|rw2|RW2|jpg|JPG)[.]dop)"
    ptoExt    = "[.]pto"
    jsonExt   = "[.]json"
    dngExt    = "[.](dng|DNG)"
    jpgExt    = "[.](jpg|JPG)"
    geoExt    = "[.]([0-9]+x[0-9]+)"
    txtExt    = "[.](md|txt)"

    imgdirName = "[-+._A-Za-z0-9]+" -- no do
    jpgdirName =
      "("
      ++ ( intercalate "|"
           [ "srgb[0-9]*"
           , "srgb" ++ "(-bw)?" ++ og
           , "[0-9]+x[0-9]+"
           , "dxo"
           , "small" ++ og
           , "web"   ++ og
           , "bw"    ++ og
           , "jpg"   ++ og
           , "tiff?" ++ og
           , "dng"   ++ og
           ]
         )
      ++ ")"
      where
        og = "(-?([0-9]+(x[0-9]+)?))?"

    jpgdirPre =
      "(" ++ jpgdirName ++ "/)?"

    boringName = intercalate "|"
      [ "[.].*"
      , ".*~"
      , "tmp.*"
      , ".*[.](bak|old|tiff|dng)"
      ]

-- ----------------------------------------
--
-- these regex must match the filePathConfig expressions
-- if new IMGimg or IMGtxt extensions are added, these regex
-- must be updated

txtSrcExpr :: Regex
txtSrcExpr =
  parseRegexExt txtpathRE

imgSrcExpr :: Regex
imgSrcExpr =
  parseRegexExt "({path}/.*[.](gif|png|tiff?)|ppm|pgm|pbm)[.]jpg"

txtPathExpr :: Regex
txtPathExpr =
  parseRegexExt $
  geoarRE ++ topdirRE ++ txtpathRE

imgPathExpr :: Regex
imgPathExpr =
  parseRegexExt $
  geoarRE ++ topdirRE ++ "({path}/.*[.]jpg)"

geoarRE :: String
geoarRE =
  "/({geoar}" ++ ar ++ "-" ++ ge ++ ")"
  where
    ar  = "(" ++ ar' ++ ")"
    ar' = intercalate "|" $ map (^. isoString) $ ars

    ge  = "([0-9]+x[0-9]+|org)"

    ars :: [AspectRatio]
    ars = [minBound..maxBound]

topdirRE :: String
topdirRE = "({topdir}/[^/]+)"

txtpathRE :: String
txtpathRE = "({path}/.*[.](txt|md))"

-- extract the path component from a file path

objSrc :: Regex -> FilePath -> FilePath
objSrc oex p =
  case matchSubexRE oex p of
    [("path", path)] ->
      path
    _ ->
      p

imgExtExpr :: Regex
imgExtExpr =
  parseRegex ".*[.](nef|NEF||rw2|RW2|dng|DNG|jpg|JPG|xmp|XMP)"

-- ----------------------------------------

pathToBreadCrump :: String -> String
pathToBreadCrump = sed (const " \8594 ") "/" . drop 1

-- ----------------------------------------
