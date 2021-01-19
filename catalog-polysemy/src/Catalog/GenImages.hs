{-# LANGUAGE
    ConstraintKinds,
    DataKinds,
    FlexibleContexts,
    GADTs,
    OverloadedStrings,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)
{-# LANGUAGE TupleSections #-}

------------------------------------------------------------------------------

module Catalog.GenImages
  ( Eff'Img

  , getThumbnailImage
  , createResizedImage
  , createVideoIcon
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
import Catalog.CatEnv            (CatEnv, catFontName)
import Catalog.MetaData.ExifTool (getExifMetaData)
import Catalog.TextPath          (toFileSysPath)

-- polysemy-tools
import Polysemy.ExecProg         (execScript)

-- catalog modules
import Data.MetaData             (lookupGeoOri)
import Data.Prim
import Data.CT

-- libraries

import qualified Data.Text                as T

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
    log'verb $ script
    void $ execScript script

-- ----------------------------------------

getThumbnailImage :: Eff'Img r => Path -> Path -> Sem r ()
getThumbnailImage src dst = do
  sp <- toFileSysPath src
  dp <- toFileSysPath dst
  extractImage sp dp
    `catch`
    ( \ e -> do
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

createResizedImage :: Eff'Img r => GeoAR -> Path -> Path -> Sem r ()
createResizedImage = createResizedImage' mempty

createVideoIcon :: Eff'Img r => GeoAR -> Path -> Path -> Sem r ()
createVideoIcon = createResizedImage' p'vico

createResizedImage' :: Eff'Img r => Path -> GeoAR -> Path -> Path -> Sem r ()
createResizedImage' vico d'geo src dst = do
  vc           <- if isempty vico
                  then return mempty
                  else toFileSysPath vico
  sp           <- toFileSysPath src
  dp           <- toFileSysPath dst
  (s'geo, ori) <- lookupGeoOri <$> getExifMetaData src
  createResized vc ori s'geo sp dp
  where

    createResized :: Eff'Img r
                  => TextPath -> Int -> Geo -> TextPath -> TextPath -> Sem r ()
    createResized vc rot s'geo sp dp

      -- resize is a noop so a link is sufficient
      | isempty shellCmd = do
          log'trc "createResizedImage: make link to src"
          linkFile sp dp

      -- resize done with external prog convert
      | otherwise = do
          log'trc $ "createResizedImage: " <> shellScript
          void $ execScript shellScript
      where
        shellCmd    = buildResizeCmd vc rot d'geo s'geo dp sp
        shellScript = toBash shellCmd

-- ----------------------------------------

resizeGeo       :: Geo -> Geo -> Geo
resizeGeo sGeo@(Geo sw sh) cGeo@(Geo dw dh)
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
    sel flist = head $ filter (`elem` flist) fs <> mempty
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
buildResize2 vico rot d'g s'geo d s =
  buildResize3 vico rot' d'g s'geo' d s
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
    isempty vico
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
                    & addPipe "convert"
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

      & ( if isempty vico
          then id
          else addVideo
        )

    addVideo cmd
     | isempty vico = cmd
     | otherwise    = cmd
                    & toStdout
                    & composite
                    & addOptVal "-gravity" "center"
                    & addVal vico
                    & fromStdin

    -- add options

    toStdout      = addVal "miff:-"
    fromStdin     = addVal "miff:-"

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

    delOrient     :: CTT -> CTT
    delOrient cmd = cmd
                    & addSeq "exiftool"
                    & addFlag "-quiet"
                    & addFlag "-Orientation="
                    & addFlag "-overwrite_original"
                    & addVal d

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

    isPad         = (xoff == (-1) && yoff == (-1))
    isCrop        = (xoff > 0     || yoff > 0)
    isThumbnail   = d'geo ^. theW <= 300 || d'geo ^. theH <= 300

    -- for .tiff convert needs the layer # to be taken
    -- if the image contains a thumbnail,
    -- there are 2 layers in the .tiff file
    --
    -- for wackelgifs a frame number is needed,
    -- else all frames are extracted
    --
    -- TODO: don't check the extension but use the mime type
    -- to determine the layer selection

    tiffLayer x
      | ".tif"  `T.isSuffixOf` x
        ||
        ".tiff" `T.isSuffixOf` x
        ||
        ".gif"  `T.isSuffixOf` x = x <> "[0]"

      | otherwise                = x

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
  toBash cmd
  where
    cmd =
      mkCexec "convert"
      & addOptVal "-background"  "rgb(255,255,255)"
      & addOptVal "-fill"        "rgb(192,64,64)"
      & ( if isempty fopt
          then id
          else addOptVal "-font" fopt
        )
      & addOptVal "-size"        "600x400"
      & addOptVal "-pointsize"   ps'
      & addOptVal "-gravity"     "center"
      & addFlag   ("label:" <> t')                 -- sequence of options matters
      & addOptVal "-background"  "rgb(128,128,128)"
      & addOptVal "-vignette"    "0x40"
      & addVal    dst

    (t', ps')
      | multiline = (t0, ps0)
      | len <= 10 = (t, "92")
      | len <= 20 = (t1 <> "\\n" <> t2, "80")
      | otherwise = (s1 <> "\\n" <> s2 <> "\\n" <> s3, "60")

    ls        = T.lines t
    lsn       = length ls
    multiline = lsn > 1
    t0        = T.intercalate "\\n" ls
    ps0
      | lsn == 2  = "80"
      | lsn == 3  = "60"
      | lsn == 4  = "50"
      | otherwise = "40"
    len       = T.length t
    len2      = len `div` 2
    len3      = len `div` 3
    (t1, t2)  = T.splitAt len2 t
    (s1, r2)  = T.splitAt len3 t
    (s2, s3)  = T.splitAt len3 r2

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
      mkCexec "convert"
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
