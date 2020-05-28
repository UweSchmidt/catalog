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

  , getImageSize
  , getThumbnailImage
  , createResizedImage
  , genIcon
  , genAssetIcon
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
import Catalog.TextPath          (toSysPath)

-- polysemy-tools
import System.ExecProg           (execProgText, execScript)

-- catalog modules
import Data.MetaData             (getOrientation)
import Data.Prim
import Data.TextPath             ((<//>), takeDir)

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

genAssetIcon :: ( EffIStore   r
                , EffError    r
                , EffLogging  r
                , EffCatEnv   r
                , EffExecProg r
                , EffFileSys  r
                )
             => Text -> Text -> Sem r (Maybe TextPath)
genAssetIcon px s = do
  log'trc $ "genAssetIcon: " <> f <> " " <> s
  genIcon f s   -- call convert with string s, please no "/"-es in s
  return $ Just f
  where
    f = ps'iconsgen ^. isoText <//> px <> ".jpg"

genIcon :: ( EffIStore   r
           , EffError    r
           , EffLogging  r
           , EffCatEnv   r
           , EffExecProg r
           , EffFileSys  r
           )
        => TextPath -> Text -> Sem r ()
genIcon path t = do
  dst  <- (^. isoTextPath) <$> toSysPath path
  dx   <- fileExist dst
  env  <- ask @CatEnv
  let fontOpt f
        | T.null f  = mempty
        | otherwise = "-font " <> f
  let fopt = fontOpt (env ^. catFontName)

  log'trc $ T.unwords ["genIcon", path, t, dst, toText dx]

  unless dx $ do
    createDir (takeDir dst)
    log'verb $ shellCmd dst fopt
    void $ execScript (shellCmd dst fopt)

  where
    shellCmd :: TextPath -> Text -> Text
    shellCmd dst fopt =
      T.unwords $
      [ "convert"
      , "-background 'rgb(255,255,255)'"
      , "-fill 'rgb(192,64,64)'"
      ]
      <>
      [ fopt ]
      <>
      [ "-size 600x400"
      , "-pointsize " <> ps'
      , "-gravity center"
      , "label:'" <> t' <> "'"
      , "-background 'rgb(128,128,128)'"
      , "-vignette 0x40"
      , dst
      ]

    (t', ps')
      | multiline = (t0, ps0)
      | len <= 10 = (t, "92")
      | len <= 20 = (t1 <> "\\n" <> t2, "80")
      | otherwise = (s1 <> "\\n" <> s2 <> "\\n" <> s3, "60")
      where
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

-- ----------------------------------------

getThumbnailImage :: Eff'Img r => TextPath -> TextPath -> Sem r ()
getThumbnailImage src dst = do
  sp <- (^. isoTextPath) <$> toSysPath src
  dp <- (^. isoTextPath) <$> toSysPath dst
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
      void $ execScript $
        T.unwords [ "exiftool"
                  , "'-b'"
                  , "'-ThumbnailImage'"
                  , "'" <> sp <> "'"
                  , ">"
                  , "'" <> dp <> "'"
                  ]
      unlessM (fileNotEmpty dp) $
        throw @Text $ "empty thumbnail file " <> dp
      return ()

-- ----------------------------------------

getImageSize    :: (Eff'ISEL r, EffCatEnv r, EffExecProg r) => TextPath -> Sem r Geo
getImageSize fp = do
  sp <- (^. isoTextPath) <$> toSysPath fp
  sz <- execImageSize sp
  return $ fromMaybe geo'org (readGeo'' $ sz ^. isoString)
    where

      execImageSize :: (Eff'ISEL r, EffExecProg r) => TextPath -> Sem r Text
      execImageSize sp =
        catch @Text
        ( execProgText "exiftool" ["-s", "-ImageSize", sp] mempty )
        ( const $ do
            log'warn $ "getImageSize: size couldn't be determined for image " <> sp
            return ""
        )

-- ----------------------------------------

createResizedImage :: Eff'Img r => GeoAR -> TextPath -> TextPath -> Sem r ()
createResizedImage d'geo src dst = do
  sp0   <-  toSysPath src
  let sp = sp0 ^. isoTextPath
  dp    <- (^. isoTextPath) <$> toSysPath dst
  ori   <- getOrientation   <$> getExifMetaData sp0
  s'geo <- getImageSize src
  createResized ori s'geo sp dp
  where
    createResized :: Eff'Img r => Int -> Geo -> TextPath -> TextPath -> Sem r ()
    createResized rot s'geo sp dp

      -- resize is a noop so a link is sufficient
      | "#" `T.isPrefixOf` shellCmd = do
          log'trc "createResizedImage: make link to src"
          linkFile sp dp

      -- resize done with external prog convert
      | otherwise = do
          log'trc $ "createResizedImage: " <> shellCmd
          void $ execScript shellCmd
      where
        shellCmd =
          buildCmd rot d'geo s'geo dp sp

buildCmd :: Int -> GeoAR -> Geo -> TextPath -> TextPath -> Text
buildCmd rot d'g s'geo d s =
  buildCmd2 rot d'g' s'geo d s
  where
    d'geo = d'g ^. theGeo
    d'g'
      | d'geo == geo'org = d'g & theGeo .~ s'geo  -- orgiginal size demaned
                               & theAR  .~ Pad
      | otherwise        = d'g

buildCmd2 :: Int -> GeoAR -> Geo -> TextPath -> TextPath -> Text
buildCmd2 rot d'g s'geo d s =
  buildCmd3 os d'g s'geo' d s
  where
    os
      | rot' /= 0 = ["-rotate", toText (rot' * 90)]
      | otherwise = []

    s'geo'
      | odd rot   = flipGeo s'geo
      | otherwise =         s'geo

    rot' = rot `mod` 4

buildCmd3 :: [Text] -> GeoAR -> Geo -> TextPath -> TextPath -> Text
buildCmd3 rotate d'g s'geo d' s'
  | d'geo == s'geo
    &&
    null rotate
    &&
    ".jpg" `T.isSuffixOf` s = "# nothing to do for " <> s'
  | otherwise = shellCmd
  where
    s           = tiffLayer s'
    d           = d'
    d'geo       = d'g ^. theGeo
    aspect      = d'g ^. theAR
    (Geo cw ch, Geo xoff yoff)
                = crGeo aspect
    r           = rGeo  aspect

    rGeo Fix    = d'geo
    rGeo Flex
      | similAR = d'geo
    rGeo _      = resizeGeo s'geo d'geo

    crGeo Fix   = cropGeo s'geo d'geo
    crGeo Pad   = (s'geo, Geo (-1) (-1))
    crGeo Crop  = (s'geo, Geo 0 0)
    crGeo Flex
      | similAR = crGeo Fix
      | otherwise
                = crGeo Pad

    similAR   = similarAspectRatio s'geo d'geo

    resize      = thumb <> [geo <> "!"]
    resize1     = thumb <> [d'geo ^. isoText]

    thumb       = [ if isThumbnail
                    then "-thumbnail"
                    else "-resize"
                  ]
    quality     = "-quality" :
                  [ if isThumbnail
                    then "75"
                    else "90"
                  ]
    interlace   = [ "-interlace", "Plane" ]

    isPad       = (xoff == (-1) && yoff == (-1))
    isCrop      = (xoff > 0     || yoff > 0)
    isThumbnail = d'geo ^. theW <= 300 || d'geo ^. theH <= 300
    geo         = r ^. isoText

    -- for .tiff convert needs the layer # to be taken
    -- if the image contains a thumbnail,
    -- there are 2 layers in the .tiff file
    tiffLayer x
      | ".tif"  `T.isSuffixOf` x
        ||
        ".tiff" `T.isSuffixOf` x = x <> "[0]"
      | otherwise                = x

    cmdName             = [ "convert", "-quiet" ]

    cmdArgs
        | isPad         = resize1
                          <> quality <> interlace
                          -- <> [ "-background", "'#333333'" ]
                          <> [ s, d ]

        | isCrop        = [ "-crop", toText cw <> "x" <> toText ch <> "+" <>
                            toText xoff <> "+" <> toText yoff
                          , s, "miff:-"
                          , "|"
                          , "convert"
                          ]
                          <> resize <> quality <> interlace
                          <> ["miff:-", d ]

        | otherwise     = resize <> [s, d]

    shellCmd    = T.unwords $
                  cmdName
                  ++ rotate
                  ++ cmdArgs

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
fontList = T.lines <$> execScript shellCmd
  where
    shellCmd =
      "convert -list font | grep Font: | sed 's|^.*Font: ||'"

-- ----------------------------------------

genBlogText :: ( EffIStore   r
               , EffError    r
               , EffLogging  r
               , EffCatEnv   r
               , EffFileSys  r
               ) => TextPath -> Sem r Text
genBlogText src = do
  sp  <- (^. isoTextPath) <$> toSysPath src
  dx  <- fileExist sp
  log'trc $ T.unwords ["genBlogText", src, toText dx]
  if dx
    then readFileT sp
    else return $ "no file found for blog text: " <> src

genBlogHtml :: ( EffIStore   r
               , EffError    r
               , EffLogging  r
               , EffCatEnv   r
               , EffExecProg r
               , EffFileSys  r
               )
            => TextPath -> Sem r Text
genBlogHtml src = do
  sp  <- (^. isoTextPath) <$> toSysPath src
  dx  <- fileExist sp
  log'trc $ T.unwords ["genBlogText", src, toText dx]
  if dx
    then formatBlogText sp
    else return $ "no file found for blog text: " <> src

formatBlogText :: ( EffError    r
                  , EffLogging  r
                  , EffExecProg r
                  )
               => TextPath -> Sem r Text
formatBlogText sp =
    execProgText "pandoc" ["-f", "markdown", "-t", "html", sp] ""

writeBlogText :: ( EffIStore   r
                 , EffError    r
                 , EffCatEnv   r
                 , EffFileSys  r
                 )
              => Text -> TextPath -> Sem r ()
writeBlogText t dst = do
  dp <- (^. isoTextPath) <$> toSysPath dst
  writeFileT dp t

------------------------------------------------------------------------
