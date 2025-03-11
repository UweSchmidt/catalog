{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- ----------------------------------------

module Catalog.GenImages
  ( Eff'Img
  , GeoOri

  , getThumbnailImage
  , createResizedImage
  , createVideoIcon
  , createResizedImage1
  , createVideoIcon1

  , genIcon
  , genBlogText
  , genBlogHtml
  , writeBlogText
  , selectFont
  , resizeGeo
  , resizeGeo'
  )
where

-- catalog-polysemy modules
import Catalog.Effects
       ( EffTime
       , EffLogging
       , EffError
       , EffIStore
       , EffCatEnv
       , EffFileSys
       , EffExecProg
       , TextPath
       , Sem
       , fileExist
       , throw
       , log'trc
       , log'verb
       , log'warn
       , writeFileT
       , readFileT
       , linkFile
       , fileNotEmpty
       , createDir
       , ask
       , catch
       )
import Catalog.CatEnv
       ( CatEnv
       , catFontName
       )
import Catalog.MetaData.ExifTool
       ( getExifMetaData )

import Catalog.TextPath
       ( toFileSysPath )

-- polysemy-tools
import Polysemy.ExecProg
       ( execScript )

-- catalog modules
import Data.MetaData
       ( lookupGeoOri )

import Data.TextPath
       ( path2MimeType )

import Data.Prim
       ( (.~)
       , (^.)
       , msgPath
       , (&)
       , isEmpty
       , IsoText(isoText)
       , Text
       , Path
       , unless
       , void
       , p'vico
       , flipGeo
       , geo'org
       , theAR
       , theGeo
       , theH
       , theW
       , isGifMT
       , isTiffMT
       , viewBase
       , (.||.)
       , toText
       , unlessM
       , AspectRatio(Pad, Crop, Flex, Fix)
       , Geo(..)
       , GeoAR
       , IsoString(isoString)
       , Field1(_1)
       )
import Data.CT       -- build shell commands
       ( addFlag
       , addOptVal
       , addOr
       , addPipe
       , addSeq
       , addVal
       , mkCexec
       , redirStdout
       , toBash
       , CTT
       )

-- libraries

import qualified Data.Digest.Murmur64     as MM
import qualified Data.Text                as T

-- ----------------------------------------

imageMagick :: String
imageMagick = "magick"

-- ----------------------------------------

type Eff'Img r = ( EffIStore   r   -- any effects missing?
                 , EffError    r   -- yes, EffJournal
                 , EffLogging  r
                 , EffCatEnv   r
                 , EffTime     r
                 , EffExecProg r
                 , EffFileSys  r
                 )

-- ----------------------------------------

genIcon :: ( EffIStore   r
           , EffError    r
           , EffLogging  r
           , EffCatEnv   r
           , EffExecProg r
           , EffFileSys  r
           )
        => Path -> Text -> Sem r ()
genIcon path t = do
  dst  <- toFileSysPath path
  dx   <- fileExist dst
  env  <- ask @CatEnv

  let fontOpt f
        | T.null f  = mempty
        | otherwise = "-font " <> f
  let fopt = fontOpt (env ^. catFontName)

  log'trc $ msgPath path ("genIcon " <> toText (t, dx))

  unless dx $ do
    dir <- toFileSysPath (path ^. viewBase . _1)
    createDir dir

    let script = buildIconScript dst fopt t
    log'verb script
    void $ execScript script

-- ----------------------------------------

getThumbnailImage :: Eff'Img r => Path -> Path -> Sem r ()
getThumbnailImage src dst = do
  sp <- toFileSysPath src
  dp <- toFileSysPath dst
  extractImage sp dp
    `catch`
    ( \ e ->
        log'warn $
    T.unwords [ "getThumbnailImage: no Thumbnail found in "
               , sp <> ","
               , "reason:"
               , e
               ]
    )
  where
    extractImage :: Eff'Img r => TextPath -> TextPath -> Sem r ()
    extractImage sp dp = do
      void $ execScript $ extractThumbnailScript sp dp

      unlessM (fileNotEmpty dp) $
        throw @Text $ "empty thumbnail file " <> dp
      return ()

-- ----------------------------------------

type GeoOri = (Geo, Int)

createResizedImage :: Eff'Img r => GeoAR -> Path -> Path -> Sem r ()
createResizedImage = createResized1 mempty

createResizedImage1 :: Eff'Img r => GeoOri -> GeoAR -> Path -> Path -> Sem r ()
createResizedImage1 = createResized2 mempty

createVideoIcon :: Eff'Img r => GeoAR -> Path -> Path -> Sem r ()
createVideoIcon = createResized1 p'vico

createVideoIcon1 :: Eff'Img r => GeoOri -> GeoAR -> Path -> Path -> Sem r ()
createVideoIcon1 = createResized2 p'vico

createResized1 :: Eff'Img r
               => Path
               -> GeoAR
               -> Path
               -> Path
               -> Sem r ()
createResized1 vico d'geo src dst = do
  s'geo <- lookupGeoOri <$> getExifMetaData src
  createResized2 vico s'geo d'geo src dst

createResized2 :: Eff'Img r
               => Path
               -> GeoOri
               -> GeoAR
               -> Path
               -> Path
               -> Sem r ()
createResized2 vico (s'geo, ori) d'geo src dst = do
  vc           <- if isEmpty vico
                  then return mempty
                  else toFileSysPath vico
  sp           <- toFileSysPath src
  dp           <- toFileSysPath dst

  let shellCmd    = buildResizeCmd vc ori d'geo s'geo dp sp
  let shellScript = toBash shellCmd

  if isEmpty shellCmd
    then
    do
      -- resize is a noop so a link is sufficient
      log'trc "createResizedImage: make link to src"
      linkFile sp dp
    else
    do
      -- resize done with external prog convert
      log'trc $ "createResizedImage: " <> shellScript
      void $ execScript shellScript

-- ----------------------------------------

resizeGeo       :: Geo -> Geo -> Geo
resizeGeo sGeo@(Geo sw sh) cGeo@(Geo dw dh)
    | cGeo == geo'org                   -- dest geo == org?
        = sGeo                          -- nothing to do
    | sw <= dw && sh <= dh              -- source fits into display
        = sGeo                          -- no downsizing, no magnification
    | otherwise
        = resizeGeo' sGeo cGeo

resizeGeo'       :: Geo -> Geo -> Geo
resizeGeo' (Geo sw sh) (Geo dw dh)
    | sw * dh >= dw * sh                -- source wider than display
        = Geo dw (dw * sh `div` sw)     -- maximum width, height scaled down

    | otherwise                         -- source taler than display
        = Geo (dh * sw `div` sh) dh     -- maximum height, width scaled down


cropGeo         :: Geo -> Geo -> (Geo, Geo)
cropGeo (Geo sw sh) (Geo dw dh)
    | sw * dh >= dw * sh                -- source wider than reqired
        = (Geo sw' sh, Geo xoff 0)
    | otherwise                         -- source higher than required
        = (Geo sw sh', Geo 0 yoff)
    where
    sw'  = dw * sh `div` dh
    xoff = (sw - sw') `div` 2           -- cut off left and right parts
    sh'  = dh * sw `div` dw
    yoff = (sh - sh') `div` 3           -- cut off 1/3 from top and 2/3 from bottom
                                        -- else important parts like heads
                                        -- are cut off (Ouch!!)

similarAspectRatio :: Geo -> Geo -> Bool
similarAspectRatio (Geo sw sh) (Geo dw dh) =
  dr >= sr / sf
  &&
  dr <= sr * sf
  where
    td :: Int -> Double
    td = fromInteger . toInteger

    sf :: Double  -- similarity factor
    sf = 1.2

    sr = td sw / td sh
    dr = td dw / td dh

-- ----------------------------------------

selectFont :: ( EffExecProg r
              , EffError r
              , EffLogging r
              )
           => Sem r Text
selectFont =
  catch @Text (sel <$> fontList) (const $ return mempty)
  where
    sel flist = mconcat . take 1 . filter (`elem` flist) $ fs
    fs        = ["ComicSans", "Helvetica"]

fontList :: ( EffExecProg r
            , EffError r
            , EffLogging r
            )
         => Sem r [Text]
fontList = T.lines <$> execScript fontListScript

-- ----------------------------------------

genBlogText :: ( EffIStore   r
               , EffError    r
               , EffLogging  r
               , EffCatEnv   r
               , EffFileSys  r
               ) => Path -> Sem r Text
genBlogText src = do
  sp  <- toFileSysPath src
  dx  <- fileExist sp
  log'trc $ T.unwords ["genBlogText", sp, toText dx]
  if dx
    then readFileT sp
    else return $ "no file found for blog text: " <> src ^. isoText

genBlogHtml :: ( EffIStore   r
               , EffError    r
               , EffLogging  r
               , EffCatEnv   r
               , EffExecProg r
               , EffFileSys  r
               )
            => Path -> Sem r Text
genBlogHtml src = do
  sp  <- toFileSysPath src
  dx  <- fileExist sp
  log'trc $ T.unwords ["genBlogText", sp, toText dx]
  if dx
    then formatBlogText sp
    else return $ "no file found for blog text: " <> src ^. isoText

formatBlogText :: ( EffError    r
                  , EffLogging  r
                  , EffExecProg r
                  )
               => TextPath -> Sem r Text
formatBlogText sp =
  execScript $ pandocScript sp

writeBlogText :: ( EffIStore   r
                 , EffError    r
                 , EffCatEnv   r
                 , EffFileSys  r
                 )
              => Text -> Path -> Sem r ()
writeBlogText t dst = do
  dp <- toFileSysPath dst
  writeFileT dp t

------------------------------------------------------------------------
--
-- build commands for resizing images

buildResizeCmd :: TextPath -> Int -> GeoAR -> Geo -> TextPath -> TextPath -> CTT
buildResizeCmd vico rot d'g s'geo d s =
  buildResize2 vico rot d'g' s'geo d s
  where
    -- normalize dest geometry
    d'g'
      | d'geo == geo'org = d'g & theGeo .~ s'geo  -- orgiginal size demaned
                               & theAR  .~ Pad
      | otherwise        = d'g

    d'geo = d'g ^. theGeo

buildResize2 :: TextPath -> Int -> GeoAR -> Geo -> TextPath -> TextPath -> CTT
buildResize2 vico rot d'g s'geo = buildResize3 vico rot' d'g s'geo'
  where
    -- rotate geometry
    s'geo'
      | odd rot   = flipGeo s'geo
      | otherwise =         s'geo

    rot' = rot `mod` 4

buildResize3 :: TextPath -> Int -> GeoAR -> Geo -> TextPath -> TextPath -> CTT
buildResize3 vico rot d'g s'geo d s'
  -- nothing to convert, just link or copy src to dst
  | d'geo == s'geo
    &&
    rot == 0
    &&
    isEmpty vico
    &&
    ".jpg" `T.isSuffixOf` s = mempty

  | otherwise =
      mempty
      & convert
      & addRotate
--    & addStrip           -- hack: throw away all exif data (even color space info)
      & addRest
      & addVideo
      & addVal d           -- final destination
      & delOrient          -- throw away exif data for orientation
                           -- rotations are all done in convert
  where
    -- combine options
    convert cmd   = cmd
                    & addPipe imageMagick
                    & addQuiet

    composite cmd = cmd
                    & addPipe "composite"
                    & addQuiet

    addRest cmd
      | isPad     = cmd & addPads
      | isCrop    = cmd & addCrops
      | otherwise = cmd & addOthers

    addPads cmd   = cmd
                    & addResize1
                    & addQuality
                    & addInterlace
                    & addVal s

    addCrops cmd  = cmd
                      -- reducing amount of intermediate data
                    & (if isThumbnail then addQuality else id)
                    & addCrop
                    & addVal s
                    & toStdout
                    & convert
                    & addResize
                    & addQuality
                    & addInterlace
                    & fromStdin

    addOthers cmd = cmd
                    & addResize
                    & addVal s

      & ( if isEmpty vico
          then id
          else addVideo
        )

    addVideo cmd
     | isEmpty vico = cmd
     | otherwise    = cmd
                    & toStdout
                    & composite
                    & addOptVal "-gravity" "center"
                    & addVal vico
                    & fromStdin

    -- add options
    -- ERROR: corvert 7.1 destroys some images, when piping miff format data
    -- from one convert cmf to anaother
    -- this is used for icons with a fixed aspect ratio
    -- first a crop is done, then a resize
    -- workaround: take jpg as intermediate format
    toStdout      = addVal "jpg:-"
    fromStdin     = addVal "jpg:-"

    addCrop       = addOptVal "-crop"
                    ( toText cw   <> "x" <> toText ch
                      <> "+" <>
                      toText xoff <> "+" <> toText yoff
                    )

    addInterlace  = addOptVal "-interlace" "Plane"

    addQuality    = addOptVal "-quality"
                    ( if isThumbnail
                      then "75"
                      else "90"
                    )

    addQuiet      = addFlag "-quiet"

    addResize     = addOptVal thumb (geo <> "!")

    addResize1    = addOptVal thumb (d'geo ^. isoText)

    addRotate
      | rot == 0  = id
      | otherwise = addOptVal "-rotate" (toText $ rot * 90)

--  addStrip      = addFlag "-strip"   -- remove exif data

    -- exif Orientation field of original is handled
    -- by convert, but convert does not update the
    -- Orientation value properly
    -- so an Orientation field is explicitly removed
    -- from the generated image by a call of  exiftool
    --
    -- exiftool somtimes emits warnings with older
    -- images, e.g. Nikon D100 .jpg files
    -- these must be ignored, else the bash command
    -- throws an error with the effect, that
    -- a "broken image" is delivered by the server

    delOrient     :: CTT -> CTT
    delOrient cmd = cmd
                    & addSeq "exiftool"
                    & addFlag "-quiet"
                    & addFlag "-Orientation="
                    & addFlag "-overwrite_original"
                    & addVal d
                    & addOr "true"   -- ignore all exif warnings and errors

    s             = tiffLayer s'
    d'geo         = d'g ^. theGeo
    aspect        = d'g ^. theAR
    (Geo cw ch, Geo xoff yoff)
                  = crGeo aspect
    r             = rGeo  aspect
    geo           = r ^. isoText

    rGeo Fix      = d'geo
    rGeo Flex
      | similAR   = d'geo
    rGeo _        = resizeGeo s'geo d'geo

    crGeo Fix     = cropGeo s'geo d'geo
    crGeo Pad     = (s'geo, Geo (-1) (-1))
    crGeo Crop    = (s'geo, Geo 0 0)
    crGeo Flex
      | similAR   = crGeo Fix
      | otherwise = crGeo Pad

    similAR       = similarAspectRatio s'geo d'geo

    thumb         = if isThumbnail
                    then "-thumbnail"
                    else "-resize"

    isPad         = xoff == (-1) && yoff == (-1)
    isCrop        = xoff > 0     || yoff > 0
    isThumbnail   = d'geo ^. theW <= 300 || d'geo ^. theH <= 300

    -- for .tiff convert needs the layer # to be taken
    -- if the image contains a thumbnail,
    -- there are 2 layers in the .tiff file
    --
    -- for wackelgifs a frame number is needed,
    -- else all frames are extracted

    tiffLayer fp
      | ( isTiffMT
          .||.
          isGifMT
        ) $ path2MimeType fp = fp <> "[0]"
      | otherwise            = fp

-- ----------------------------------------
{-

there is a bug in convert concerning the image orientation

# resize an image in portrait format to 2560x1440

uwe@scheibe:~/tmp> convert -verbose -resize 2560x1440 -quality 90 -interlace Plane ../Bilder/Diakaesten/themen/Reisen/Highway1-2015/Uwe/2015-09-08/srgb/_uwe9086.jpg _uwe9086-2560x1440.jpg
../Bilder/Diakaesten/themen/Reisen/Highway1-2015/Uwe/2015-09-08/srgb/_uwe9086.jpg JPEG 3264x4928 3264x4928+0+0 8-bit sRGB 17.7239MiB 0.550u 0:01.289
../Bilder/Diakaesten/themen/Reisen/Highway1-2015/Uwe/2015-09-08/srgb/_uwe9086.jpg=>_uwe9086-2560x1440.jpg JPEG 3264x4928=>954x1440 954x1440+0+0 8-bit sRGB 404643B 0.850u 0:00.850

# extract exif data for src and dst

uwe@scheibe:~/tmp> exiftool ../Bilder/Diakaesten/themen/Reisen/Highway1-2015/Uwe/2015-09-08/srgb/_uwe9086.jpg > org.out
uwe@scheibe:~/tmp> exiftool _uwe9086-2560x1440.jpg > cpy.out

# diff exif data
uwe@scheibe:~/tmp> diff org.out cpy.out
2,7c2,7
< File Name                       : _uwe9086.jpg
< Directory                       : ../Bilder/Diakaesten/themen/Reisen/Highway1-2015/Uwe/2015-09-08/srgb
---
> File Name                       : _uwe9086-2560x1440.jpg

16c16
< Orientation                     : Horizontal (normal)
---
> Orientation                     : Rotate 270 CW
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# convert thinks image should be rotated 90 degrees counter clock wise
# when displayed, this is wrong
# Orientation tag must be deleted afterwards

354,356c354,356
< Image Width                     : 3264
< Image Height                    : 4928
---
> Image Width                     : 954
> Image Height                    : 1440

366c366
< Image Size                      : 3264x4928
---
> Image Size                      : 954x1440

# workaround: remove all exif data with -strip flag
# generated copies don't need this exif data anyway

-}
-- ----------------------------------------

buildIconScript :: TextPath -> Text -> Text -> Text
buildIconScript dst fopt t =
  toBash ccmd
  where
    ccmd =
      mkCexec imageMagick
      & addImgSize
      & addBG
      & addText
      & addVignette
      & addVal dst

    (t', ps') = formatImgText t

    addImgSize cmd =
      cmd & addOptVal "-size" "600x400"

    addText cmd =
      cmd & addOptVal "-pointsize"   ps'
          & ( if isEmpty fopt
              then id
              else addOptVal "-font" fopt
            )
          & addOptVal "-gravity"     "center"
          & addOptVal "-fill"        "rgb(128,64,64)"
          & addOptVal "-annotate"    "0"
          & addFlag    t'

    addBG cmd =
      cmd & addOptVal "-seed" se
          & addFlag   ("plasma:" <> c1 <> "-" <> c2)
          & addOptVal "-blur" "10"
          & addOptVal "-swirl" sw
      where
        -- c1 = "rgb(255,192,192)"       -- preliminary
        -- c2 = "rgb(192,255,192)"       -- colors in range 192..255
        -- sw = "135"                    -- degree in range 90..270

        c1 = formatRGB $ map (scaleInt (192, 255)) rgb1
        c2 = formatRGB $ map (scaleInt (192, 255)) rgb2
        sw = scaleInt (-270, 270) sw1 ^. isoText
        se = scaleInt (0, two'31'1) see ^. isoText

        two'31'1 :: Int
        two'31'1 = 2^(31::Int) - 1

    _addBG cmd =                      -- old version
      cmd & addOptVal "-background"  "rgb(255,255,255)"

    addVignette cmd =
      cmd

    _addVignette cmd =                -- old version
      cmd & addOptVal "-background"  "rgb(128,128,128)"
          & addOptVal "-vignette"    "0x40"

    rs0         = randomHashes $ t ^. isoString
    (rgb1, rs1) = splitAt 3 rs0
    (rgb2, rs2) = splitAt 3 rs1
    (sw1 : rs3) = rs2               -- warning: -Wno-incomplete-uni-patterns
    (see :_rs4) = rs3

    formatRGB [r, g, b] = concat ["rgb(" ,show r, ",", show g, ",", show b, ")"] ^. isoText
    formatRGB _         = mempty

formatImgText :: Text -> (Text, Text)
formatImgText t = (t', ps' ^. isoText)
  where
    ps' :: Int
    (t', ps')
      | multiline = (t0, ps0)
      | len <= 10 = (t, 92)
      | len <= 20 = (t1 <> "\\n" <> t2, 80)
      | otherwise = (s1 <> "\\n" <> s2 <> "\\n" <> s3, 60)

    ls        = T.lines t
    lsn       = length ls
    multiline = lsn > 1
    t0        = T.intercalate "\\n" ls
    ps0
      | lsn == 2  = 80
      | lsn == 3  = 60
      | lsn == 4  = 50
      | otherwise = 40
    len       = T.length t
    len2      = len `div` 2
    len3      = len `div` 3
    (t1, t2)  = T.splitAt len2 t
    (s1, r2)  = T.splitAt len3 t
    (s2, s3)  = T.splitAt len3 r2

------------------------------------------------------------------------
--
-- | hack for a very simple random generator

randomHashes :: MM.Hashable64 a => a -> [Int]
randomHashes x =
  map hh [0..]
  where
    hh :: Int -> Int
    hh i = nn . fromIntegral . MM.asWord64 $ MM.hash64Add i hx

    nn i
      | i < 0     = i + minBound
      | otherwise = i

    hx = MM.hash64 x

scaleInt :: (Int, Int) -> Int -> Int
scaleInt (lb, ub) i = i `mod` (ub - lb + 1) + lb

------------------------------------------------------------------------

extractThumbnailScript :: TextPath -> TextPath -> Text
extractThumbnailScript sp dp =
  toBash cmd
  where
    cmd =
      mkCexec       "exiftool"
      & addFlag     "-b"
      & addFlag     "-ThumbnailImage"
      & addVal      sp
      & redirStdout dp

fontListScript :: Text
fontListScript =
  toBash cmd
  where
    cmd =
      mkCexec imageMagick
      & addOptVal "-list" "font"
      & addPipe "grep"
      & addOptVal "-e" "Font:"
      & addPipe "sed"
      & addOptVal "-e" "s|^.*Font: ||"

pandocScript :: TextPath -> Text
pandocScript sp =
  toBash cmd
  where
    cmd =
      mkCexec "pandoc"
      & addOptVal "-f" "markdown"
      & addOptVal "-t" "html"
      & addVal sp

------------------------------------------------------------------------
