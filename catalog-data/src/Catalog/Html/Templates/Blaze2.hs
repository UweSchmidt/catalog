{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Catalog.Html.Templates.Blaze2
  ( Html
  , IconDescr
  , picPage'
  , colPage'
  , txtPage'
  , movPage'
  , gifPage'
  , renderPage
  , renderPage'
  )
where

import Data.MetaData ( MetaData
                     , MetaKey

                     , metaDataAt
                     , metaTextAt
                     , lookupGPSposDeg
                     , metaTimeStamp

                     , compositeDOF
                     , compositeFOV
                     , compositeHyperfocalDistance
                     , compositeGPSPosition
                     , compositeImageSize
                     , compositeLensID
                     , compositeLensSpec
                     , compositeMegapixels

                     , descrAddress
                     , descrComment
                     , descrCommentImg
                     , descrKeywords
                     , descrLocation
                     , descrSubtitle
                     , descrTitle
                     , descrTitleEnglish
                     , descrTitleLatin
                     , descrGPSAltitude
                     , descrWeb
                     , descrWikipedia

                     , exifCreateDate
                     , exifExposureCompensation
                     , exifExposureMode
                     , exifExposureProgram
                     , exifExposureTime
                     , exifFNumber
                     , exifFocalLength
                     , exifFocalLengthIn35mmFormat
                     , exifISO
                     , exifModel
                     , exifWhiteBalance

                     , fileFileSize
                     , fileMimeType
                     -- , fileName
                     , fileRefImg
                     , fileRefJpg
                     , fileRefRaw
                     , fileTimeStamp

                     , gifAnimationIterations
                     , gifDuration
                     , gifFrameCount

                     , imgRating

                     , makerNotesFocusDistance
                     , makerNotesShootingMode
                     , makerNotesShutterCount

                     , quickTimeDuration
                     , quickTimeVideoFrameRate
                     )

import           Data.Prim
import           Catalog.Version (date, version)

import           Text.Printf                     ( printf )
import qualified Data.Text                       as T
import qualified Data.Map                        as M
import           Text.Blaze.Html5                hiding (map, head)
import qualified Text.Blaze.Html5                as H
import           Text.Blaze.Html5.Attributes     hiding (title, rows, accept, id)
import qualified Text.Blaze.Html5.Attributes     as A
import qualified Text.Blaze.Html.Renderer.Pretty as R
import qualified Text.Blaze.Html.Renderer.Text   as T

-- ----------------------------------------

renderPage' :: Html -> LazyText
renderPage' p = T.renderHtml p

-- indent HTML
renderPage :: Html -> LazyText
renderPage p = R.renderHtml p ^. isoText . lazy

-- ----------------------------------------

type IconDescr = (Text, Text, Text, Text)

colPage' :: Text -> Text -> Text
         -> Text
         -> Geo
         -> Text -> Text -> Text -> Text
         -> Text -> Text -> Text -> Text
         -> Text -> Text
         -> Text -> Text -> Text -> Text -> Text
         -> Text
         -> Text -> Text
         -> Text -> Text -> Text
         -> Int  -> [IconDescr]
         -> MetaData
         -> Html
colPage'
  theBaseRef theHeadTitle theDate
  theTitle
  theImgGeo
  theDuration thisHref thisPos
  theNextHref thePrevHref theParentHref theChild1Href theFwrdHref
  theImgGeoDir theIconGeoDir
  thisImgRef nextImgRef prevImgRef child1ImgRef fwrdImgRef
  cBlogContents
  theParentTitle parentImgRef
  theNextTitle thePrevTitle theChild1Title
  no'cols icons
  metaData

  = colPage
    theBaseRef
    theHeadTitle
    theDate
    theImgGeo
    ( jsCode
      theDuration
      thisHref
      thisPos
      theNextHref
      thePrevHref
      theParentHref
      theChild1Href
      theFwrdHref
      ( imgRef theImgGeoDir nextImgRef )
      ( imgRef theImgGeoDir prevImgRef )
      ( imgRef theImgGeoDir fwrdImgRef )
      mempty
      mempty
      mempty
    )
    ( colImg
      theImgGeoDir theIconGeoDir
      thisImgRef
      theHeadTitle
    )
    ( colTitle
      theTitle
      metaData
    )
    ( colNav
      ( parentNav theParentTitle parentImgRef  theImgGeoDir theIconGeoDir)
      ( prevNav   thePrevTitle   prevImgRef    theImgGeoDir theIconGeoDir)
      ( child1Nav theChild1Title child1ImgRef
                                 theChild1Href theImgGeoDir theIconGeoDir)
      ( nextNav   theNextTitle   nextImgRef    theImgGeoDir theIconGeoDir)
    )
    ( colBlog
      cBlogContents
    )
    ( colContents
      no'cols
      ( map (colIcon no'cols theImgGeoDir theIconGeoDir) icons )
    )

picPage' :: Text -> Text -> Text
         -> Text -> Text -> Text
         -> Geo  -> Maybe Text
         -> Text -> Text -> Text
         -> Text -> Text -> Text -> Text
         -> Text
         -> Text -> Text -> Text -> Text
         -> Text -> Text
         -> MetaData
         -> Html
picPage'
  theBaseRef theHeadTitle theDate
  theTitle theSubTitle theComment
  theImgGeo thePanoGeoDir
  theDuration thisHref thisPos
  theNextHref thePrevHref theParentHref theFwrdHref
  theImgGeoDir
  thisImgRef nextImgRef prevImgRef fwrdImgRef
  orgImgRef panoImgRef
  metaData

  = picPage
    theBaseRef
    theHeadTitle
    theDate
    theImgGeo
    ( jsCode
      theDuration
      thisHref
      thisPos
      theNextHref
      thePrevHref
      theParentHref
      mempty -- child1 href
      theFwrdHref
      ( imgRef theImgGeoDir nextImgRef )
      ( imgRef theImgGeoDir prevImgRef )
      ( imgRef theImgGeoDir fwrdImgRef )
      ( imgRef theImgGeoDir thisImgRef )
      -- hack to work with old and new url scheme
      ( if T.null orgImgRef
        then imgRef orgGeoDir thisImgRef
        else orgImgRef
      )
      -- hack to work with old and new url scheme
      ( if T.null panoImgRef
        then maybe mempty (flip imgRef thisImgRef) thePanoGeoDir
        else panoImgRef
      )
    )
    ( picImg
      theImgGeo
      theImgGeoDir
      thisImgRef
    )
    ( picTitle
      theImgGeo
      theTitle
      theSubTitle
      theComment
    )
    picNav
    ( picInfo
      theImgGeo
      metaData
    )

movPage' :: Text -> Text -> Text
         -> Text -> Text -> Text
         -> Geo  -> Geo
         -> Text -> Text -> Text
         -> Text -> Text -> Text -> Text
         -> Text -> Text -> Text -> Text
         -> MetaData
         -> Html
movPage'
  theBaseRef theHeadTitle theDate
  theTitle theSubTitle theComment
  theImgGeo theScreenGeo
  thisDuration thisHref thisPos
  theNextHref thePrevHref theParentHref theFwrdHref
  thisImgRef nextImgRef prevImgRef fwrdImgRef
  metaData

  = movPage
    theBaseRef
    theHeadTitle
    theDate
    theImgGeo
    ( jsCode
      thisDuration
      thisHref
      thisPos
      theNextHref
      thePrevHref
      theParentHref
      mempty                         -- no child href
      theFwrdHref
      ( imgRef mempty nextImgRef )
      ( imgRef mempty prevImgRef )
      ( imgRef mempty fwrdImgRef )
      ( imgRef mempty thisImgRef )
      mempty                         -- no original size image
      mempty                         -- no pano image
    )
    ( picMov
      theScreenGeo
      thisImgRef
    )
    ( picTitle
      theImgGeo
      theTitle
      theSubTitle
      theComment
    )
    picNav
    ( picInfo
      theImgGeo
      metaData
    )

gifPage' :: Text -> Text -> Text
         -> Text -> Text -> Text
         -> Geo  -> Geo
         -> Text -> Text -> Text
         -> Text -> Text -> Text -> Text
         -> Text -> Text -> Text -> Text
         -> MetaData
         -> Html
gifPage'
  theBaseRef theHeadTitle theDate
  theTitle theSubTitle theComment
  theImgGeo theScreenGeo
  thisDuration thisHref thisPos
  theNextHref thePrevHref theParentHref theFwrdHref
  thisImgRef nextImgRef prevImgRef fwrdImgRef
  metaData

  = gifPage
    theBaseRef
    theHeadTitle
    theDate
    theImgGeo
    ( jsCode
      thisDuration
      thisHref
      thisPos
      theNextHref
      thePrevHref
      theParentHref
      mempty                         -- no child href
      theFwrdHref
      ( imgRef mempty nextImgRef )
      ( imgRef mempty prevImgRef )
      ( imgRef mempty fwrdImgRef )
      ( imgRef mempty thisImgRef )
      mempty                         -- no original size image
      mempty                         -- no pano image
    )
    ( picGif
      theScreenGeo
      thisImgRef
    )
    ( picTitle
      theImgGeo
      theTitle
      theSubTitle
      theComment
    )
    picNav
    ( picInfo
      theImgGeo
      metaData
    )

txtPage' :: Text -> Text -> Text
         -> Text -> Text -> Text
         -> Text -> Text -> Text -> Text
         -> Text
         -> Text -> Text -> Text
         -> Text
         -> Html
txtPage'
  theBaseRef theHeadTitle theDate
  theDuration thisHref thisPos
  theNextHref thePrevHref theParentHref theFwrdHref
  theImgGeoDir
  nextImgRef prevImgRef fwrdImgRef
  blogContents
  = txtPage
    theBaseRef
    theHeadTitle
    theDate
    ( jsCode
      theDuration
      thisHref
      thisPos
      theNextHref
      thePrevHref
      theParentHref
      mempty
      theFwrdHref
      ( imgRef theImgGeoDir nextImgRef )
      ( imgRef theImgGeoDir prevImgRef )
      ( imgRef theImgGeoDir fwrdImgRef )
      mempty mempty mempty
    )
    blogContents

-- ----------------------------------------

picPage :: Text -> Text -> Text
        -> Geo
        -> Html -> Html -> Html -> Html -> Html
        -> Html
picPage theBaseRef theHeadTitle theDate
        theImgGeo
        jsCode picImg picTitle picNav picInfo
  = htmlPage
    ( headPage theBaseRef theHeadTitle theDate jsCode ) $ do
  body
    ! onload "initPicture();"
    ! class_ (toValue $ "picture picture-" <> (theImgGeo ^. isoText)) $ do
    picImg
    picTitle
    picInfo
    picHelp theImgGeo
    picNav

-- ----------------------------------------

movPage :: Text -> Text -> Text
        -> Geo
        -> Html -> Html -> Html -> Html -> Html
        -> Html
movPage theBaseRef theHeadTitle theDate
        theImgGeo
        jsCode picMov picTitle picNav picInfo
  = htmlPage
    ( headPage theBaseRef theHeadTitle theDate jsCode ) $ do
  body
    ! onload "initMovie();"
    ! class_ (toValue $ "picture picture-" <> (theImgGeo ^. isoText)) $ do
    picMov
    picTitle
    picInfo
    picHelp theImgGeo
    picNav

-- ----------------------------------------

gifPage :: Text -> Text -> Text
        -> Geo
        -> Html -> Html -> Html -> Html -> Html
        -> Html
gifPage theBaseRef theHeadTitle theDate
        theImgGeo
        jsCode picGif picTitle picNav picInfo
  = htmlPage
    ( headPage theBaseRef theHeadTitle theDate jsCode ) $ do
  body
    ! onload "initPicture();"
    ! class_ (toValue $ "picture picture-" <> (theImgGeo ^. isoText)) $ do
    picGif
    picTitle
    picInfo
    picHelp theImgGeo
    picNav

-- ----------------------------------------

txtPage :: Text -> Text -> Text
        -> Html -> Text
        -> Html
txtPage theBaseRef theHeadTitle theDate
        jsCode blogContents
  = htmlPage
    ( headPage theBaseRef theHeadTitle theDate jsCode ) $ do
  body
    ! onload "initPicture();"
    ! class_ "text" $ preEscapedText blogContents

-- ----------------------------------------

colPage :: Text
        -> Text -> Text -> Geo
        -> Html -> Html -> Html -> Html -> Html -> Html
        -> Html
colPage theBaseRef theHeadTitle theDate theImgGeo
        jsCode colImg colTitle colNav colBlog colContents
  = htmlPage
    ( headPage theBaseRef theHeadTitle theDate jsCode ) $ do
  body
    ! onload "initAlbum();"
    ! class_ (toValue $ "album album-" <> theImgGeo ^. isoText)
    ! A.id   "theAlbumBody"
    $ do
    table ! class_ "head" $ do
      tr $ do
        td ! class_ "head1" $ colImg
        td ! class_ "head2" $ colTitle
        td ! class_ "head3" $ colNav
    colBlog
    H.div ! class_ "ruler" $ mempty
    H.div ! class_ "album-content" $ colContents
    H.div ! class_ "ruler" $ mempty
    table ! class_ "head" $ do
      tr $ do
        td ! class_ "head1" $ mempty
        td ! class_ "head2" $ mempty
        td ! class_ "head3" $ colNav
    picHelp theImgGeo


-- ----------------------------------------

headPage :: Text -> Text -> Text -> Html -> Html
headPage theBaseRef theHeadTitle theDate theJS
  = H.head $ do
  title $ toHtml theHeadTitle
  meta ! name "description" ! content "Web Photo Album"
  meta ! name "author"      ! content "Uwe Schmidt"
  meta ! name "generator"   ! content ( toValue $
                                        "catalog-servant version " <>
                                        version <> " from " <> date
                                      )
  meta ! name "date"        ! content (toValue theDate)
  base ! href (toValue theBaseRef)
  link
    ! rel   "shortcut icon"
    ! href  "/favicon.ico"
    ! type_ "image/x-icon"
  link
    ! rel   "stylesheet"
    ! type_ "text/css"
    ! href  "/assets/css/html-album.css"
  theJS
  script
    ! type_   "text/javascript"
    ! src     "/bootstrap/4.5.0/js/jquery/jquery-3.5.1.min.js"
    $ mempty
  script
    ! type_   "text/javascript"
    ! src     "/assets/javascript/html-album.js"
    $ mempty

htmlPage :: Html -> Html -> Html
htmlPage theHead theBody = do
  docType
  html
    ! lang "de" $ do
    theHead
    theBody

-- ----------------------------------------

colBlog :: Text -> Html
colBlog cBlogContents
  | T.null cBlogContents = mempty
  | otherwise = do
      H.div ! class_ "ruler" $ mempty
      H.div ! class_ "blog-contents" $ preEscapedText cBlogContents

jsCode :: Text -> Text -> Text
       -> Text -> Text -> Text
       -> Text -> Text
       -> Text -> Text -> Text
       -> Text -> Text -> Text
       -> Html
jsCode theDuration thisHref thisPos
      theNextHref thePrevHref theParentHref
      theChild1Href theFwrdHref
      theNextImgRef thePrevImgRef theFwrdImgRef
      thisImgRef thisOrgRef thisPanoRef
  = do
  script ! type_ "text/javascript" $ preEscapedText $ T.unlines $
    [ ""
    , "<!--"
    , "var duration = 7000 * '" <> theDuration <> "';"
    , "var thisp    = '" <> thisHref <> "';"
    , "var thispos  = '#" <> thisPos <> "';"
    , "var nextp    = '" <> theNextHref   <> "';"
    , "var prevp    = '" <> thePrevHref   <> "';"
    , "var fwrdp    = '" <> theFwrdHref   <> "';"
    , "var parentp  = '" <> theParentHref <> "';"
    , "var childp   = '" <> theChild1Href <> "';"
    , "var nextimg  = '" <> theNextImgRef <> "';"
    , "var previmg  = '" <> thePrevImgRef <> "';"
    , "var fwrdimg  = '" <> theFwrdImgRef <> "';"
    , "var thisimg  = '" <> thisImgRef    <> "';"
    , "var orgimg   = '" <> thisOrgRef    <> "';"
    , "var panoimg  = '" <> thisPanoRef   <> "';"
    , "-->"
    ]

colTitle :: Text -> MetaData -> Html
colTitle theTitle md = do
  di "title"    theTitle
  di "subtitle" (md ^. metaTextAt descrSubtitle)
  di "comment"  (md ^. metaTextAt descrComment)
  di' gpsToHtml "comment"  (lookupGPSposDeg md)
  where
    di :: Text -> Text -> Html
    di = di' toHtml

    di' :: (Text -> Html) -> Text -> Text -> Html
    di' toH c t
      | T.null t  = mempty
      | otherwise = H.div ! class_ (toValue c) $ toH t

colImg :: Text -> Text -> Text -> Text -> Html
colImg theImgGeoDir theIconGeoDir thisImgRef theHeadTitle =
  img ! src     (toValue $ imgRef' theImgGeoDir theIconGeoDir thisImgRef)
      ! class_  (toValue $ "icon-" <> theIconGeoDir)
      ! A.title (toValue theHeadTitle)
      ! alt     (toValue theHeadTitle)

colNav :: Html -> Html -> Html -> Html -> Html
colNav parentNav prevNav child1Nav nextNav = do
  table ! class_ "nav" {- ! align "right" -} $ do
    tr $ do
      td ! class_ "icon2" $ preEscapedText "&nbsp;"
      td ! class_ "icon2" ! A.id "theParentNav" $ parentNav
      td ! class_ "icon2" $ preEscapedText "&nbsp;"
    tr $ do
      td ! class_ "icon2" ! A.id "thePrevNav"   $ prevNav
      td ! class_ "icon2" ! A.id "theChild1Nav" $ child1Nav
      td ! class_ "icon2" ! A.id "theNextNav"   $ nextNav

parentNav :: Text -> Text -> Text -> Text -> Html
parentNav theParentTitle parentImgRef theImgGeoDir theIconGeoDir =
  nav' "javascript:parentPage();"
       ("Album" <:> theParentTitle)
       parentImgRef
       theImgGeoDir theIconGeoDir

nextNav :: Text -> Text -> Text -> Text -> Html
nextNav theNextTitle nextImgRef theImgGeoDir theIconGeoDir =
  nav' "javascript:nextPage();"
       ("weiter" <:> theNextTitle)
       nextImgRef
       theImgGeoDir theIconGeoDir

prevNav :: Text -> Text -> Text -> Text -> Html
prevNav thePrevTitle prevImgRef theImgGeoDir theIconGeoDir =
  nav' "javascript:prevPage();"
       ("zur\252ck" <:> thePrevTitle)
       prevImgRef
       theImgGeoDir theIconGeoDir

child1Nav :: Text -> Text -> Text -> Text -> Text -> Html
child1Nav theChild1Title child1ImgRef
                         theChild1Href theImgGeoDir theIconGeoDir =
  nav' ("javascript:childPage('" <> theChild1Href <> "');")
       ("1. Bild" <:> theChild1Title)
       child1ImgRef
       theImgGeoDir theIconGeoDir


nav' :: Text -> Text -> Text -> Text -> Text -> Html
nav' theHref theTitle theImgRef theImgGeoDir theIconGeoDir
  | T.null theImgRef = mempty
  | otherwise        = do
      a ! href (toValue theHref)
        ! A.title (toValue theTitle) $ do
        img ! src    (toValue $ imgRef' theImgGeoDir theIconGeoDir theImgRef)
            ! class_ (toValue $ "icon2-" <> theIconGeoDir)
            ! alt    (toValue theTitle)

-- ----------------------------------------
--
-- format collection contents
-- 1. variant:
--    flow layout of icons,
--    all icons are of the same height
--    and aspect ratio of the original
-- 2. variant:
--    all icons have the same size and aspect ratio
--    parts of the original are cut of

colContents :: Int -> [Html] -> Html

-- floating layout of collection contents
colContents 0 colIcons = do
  H.p ! class_ "col-contents" $ mconcat colIcons

-- table layout of collection contents
colContents no'cols colIcons = do
  table ! class_ "col-contents" $ mconcat $ map toRow colRows
  where
    colRows = divideAt no'cols colIcons
    toRow :: [Html] -> Html
    toRow r = do
      tr ! class_ "col-row" $ mconcat r


colIcon :: Int -> Text -> Text -> IconDescr -> Html
colIcon 0 = colIcon' H.span
colIcon _ = colIcon' td

colIcon' :: (Html -> Html) -> Text -> Text -> IconDescr -> Html
colIcon' el theImgGeoDir theIconGeoDir
       (theChildHref, theChildImgRef, theChildTitle, theChildId) = do
  el ! class_ (toValue $ "icon-" <> theIconGeoDir)
     ! A.id   (toValue theChildId)
     ! A.name (toValue theChildId) $ do
    H.a ! href    (toValue $ "javascript:childPage('" <> theChildHref <> "');")
        ! A.title (toValue theChildTitle) $ do
      img ! src    (toValue $ imgRef' theImgGeoDir theIconGeoDir theChildImgRef)
          ! class_ (toValue $ "icon-" <> theIconGeoDir)
          ! alt    (toValue theChildTitle)

-- ----------------------------------------

picImg :: Geo -> Text -> Text -> Html
picImg theImgGeo theImgGeoDir thisImgRef = do
  -- the scaled picture fitting on the dispay
  H.div ! class_   "picture"
        ! A.id     "pic-scaled" $
    table ! class_ "picture" $
      tr $
        td ! class_ "picture" $
          img ! src (toValue $ imgRef theImgGeoDir thisImgRef)

  -- the panorama picture, fitting the screen height
  H.div ! scroll "-x"
        ! class_ "panorama"
        ! A.id   "pic-pano" $
    img ! src    ""
        ! class_ "panorama"

  -- the picture in original size
  H.div ! scroll ""
        ! A.id   "pic-org" $
    img ! src    ""
  where
    scroll x =
      A.style ( toValue $
                   "overflow: scroll" <> x <> ";"
                <> "display:  none;"
                <> "width: "  <> theImgGeo ^. theW . isoText <> "px;"
                <> "height: " <> theImgGeo ^. theH . isoText <> "px;"
              )

-- ----------------------------------------

picMov :: Geo  -> Text -> Html
picMov theScreenGeo thisImgRef = do
  -- the movie fitting into the dispay

  H.div ! class_   "picture"
        ! A.id     "pic-scaled" $

    H.video ! A.id "pic-movie"
            ! A.width  (toValue $ theScreenGeo ^. theW . isoText)
            ! A.height (toValue $ theScreenGeo ^. theH . isoText)
        --  ! A.controls mempty                  -- toggled with key c
            ! A.autoplay mempty                  -- autoplay works only
                                                 -- with muted set (in Chrome)
            ! H.customAttribute  "muted"  mempty -- toggled with key m
            $ do
      H.source ! A.src  (toValue thisImgRef)
               ! A.type_ "video/mp4"
      H.span "Your browser does not support HTML5 mp4 video"

-- ----------------------------------------

picGif :: Geo  -> Text -> Html
picGif _theScreenGeo thisImgRef = do
  -- the movie fitting into the dispay

  H.div ! class_   "picture"
        ! A.id     "pic-scaled" $
    img ! src (toValue thisImgRef)

-- ----------------------------------------

picTitle :: Geo -> Text -> Text -> Text -> Html
picTitle theImgGeo theTitle theSubTitle theComment =
  H.div ! class_ "title-area"
        ! onmouseover "showTitle();"
        ! onmouseout  "hideTitle();" $
    H.div ! class_ (toValue $ "title-area-line title-area-line-" <>
                              (theImgGeo ^. isoText)
                   )
          ! A.id   "title-area-line" $ do
      H.div ! class_ "title"    $ toHtml theTitle
      H.div ! class_ "subtitle" $ toHtml theSubTitle
      H.div ! class_ "comment"  $ toHtml theComment

picNav :: Html
picNav = do
    H.a ! href "javascript:parentPage()"
        ! A.title "Album"
        ! class_ "up"
        ! A.id "theUpButton" $
      espan "theContainingAlbum"

    H.a ! href "javascript:prevPage()"
        ! A.title "zur\252ck"
        ! class_ "back"
        ! A.id "theBackButton" $
      espan "thePreviousPic"

    H.a ! href "javascript:nextPage()"
        ! A.title "weiter"
        ! class_ "forward"
        ! A.id "theForwardButton" $
      espan "theNextPic"
  where
    espan :: Text -> Html
    espan sid = H.span ! A.id (toValue sid) $
      preEscapedText "&nbsp;"

picInfo :: Geo -> MetaData -> Html
picInfo theImgGeo md =
    H.div ! class_ "info-area" $
      H.div ! class_ (toValue $ "info-area-content info-area-content-" <>
                                theImgGeo ^. isoText
                     )
            ! A.id "info-area-content" $
        H.div ! class_ "info" $ do
          H.div ! class_ "subtitle" $ "Bild-Daten"
          table ! class_ "info" $ picMeta md

picHelp :: Geo -> Html
picHelp theImgGeo =
  H.div ! class_ "help-area" $
    H.div ! class_ (toValue $ "help-area-content help-area-content-" <>
                              theImgGeo ^. isoText
                   )
          ! A.id "help-area-content" $ do
      H.div ! class_ "help" $ do
        H.div ! class_ "subtitle" $ "Tastatur-Kommandos"
        table ! class_ "help" $ picKeys

picKeys :: Html
picKeys = do
  toRow "→, n, > \" \"" "nächstes Bild"
  toRow "←, p, <"       "vorheriges Bild"
  toRow "↑, u, ^"       "zum Album"
  toRow "↓, d, v"       "zum 1. Bild eines Albums"
  toRow "i"             "Anzeige der Metadaten eines Bildes"
  toRow "f"             "Anzeige eines Bildes in Originalgröße"
  toRow "a, q"          "Animation eines Panorama-Bildes"
  toRow "s"             "start/stop automatischer Bildwechsel im momentanen Album"
  toRow "S"             "start/stop automatischer Bildwechsel alle Alben"
  toRow "+"             "automatischer Bildwechsel schneller"
  toRow "-"             "automatischer Bildwechsel langsamer"
  toRow "0"             "automatischer Bildwechsel Dauer zurücksetzen"
  toRow "t"             "Anzeige des Titels eines Bildes"
  toRow "m"             "Ton bei Videos an/aus"
  toRow "c"             "Kontrollleiste bei Videos an/aus"
  toRow "?, h"          "Diese Hilfe-Info"

  where
    toRow :: Text -> Text -> Html
    toRow k v = do
      tr ! class_ "help" $ do
        th $ toHtml k
        td $ toHtml v

picMeta :: MetaData -> Html
picMeta md = mconcat mdTab
  where
    toEntry :: Text -> MetaKey -> Text -> Html -> Html
    toEntry descr key val val'
      | T.null val = mempty
      | otherwise  =
          tr ! class_ "info"
             ! A.id (toValue $ key ^. isoText) $ do
          th $ toHtml descr
          td $ val'

    mdval' :: (Text -> Text) -> Text -> MetaKey -> Html
    mdval' tr descr key =
      toEntry descr key val $
      toHtml val
      where
        val = md ^. metaTextAt key . to tr

    mdval :: Text -> MetaKey -> Html
    mdval = mdval' transMDVal

    mdval0 :: Text -> MetaKey -> Html
    mdval0 = mdval' id

    mdDeg :: Text -> MetaKey -> Html
    mdDeg = mdval' formatDegree

    mdLink :: Text -> MetaKey -> Html
    mdLink descr key =
      toEntry descr key val $
      H.a ! href (toValue val) $
      toHtml val
      where
        val = md ^. metaTextAt key

    mdRat :: Text -> Html
    mdRat descr =
      toEntry descr key val $
      H.span ! A.style "color: red" $
      toHtml val
      where
        key   = imgRating
        val   = md ^. metaTextAt key

    mdTs :: Text -> Html
    mdTs descr =
      toEntry descr key val $
      toHtml val
      where
        key = fileTimeStamp
        ts  = md ^. metaDataAt key . metaTimeStamp
        val
          | isempty ts = mempty
          | otherwise  = timeStampToText ts

    mdMPX :: Text -> Html
    mdMPX descr =
      toEntry descr key val $
      toHtml val
      where
        key = compositeMegapixels
        mpx = md ^. metaTextAt key
        val
          | isempty mpx = mempty
          | otherwise   = mpx & isoString %~ fmt
        fmt s = fromMaybe "" $ printf "%f" <$> px
          where
            px :: Maybe Double
            px = readMaybe s

    mdMap :: Text -> Html
    mdMap descr =
      toEntry descr compositeGPSPosition val $
      gpsToHtml val
      where
        val  = lookupGPSposDeg md

    mdKw :: Text -> Html
    mdKw descr =
      toEntry descr key val $
      toHtml val
      where
        key = descrKeywords
        val = md ^. metaTextAt key

    mdTab :: [Html]
    mdTab =
      [ mdval0 "Titel"                  descrTitle
      , mdval0 "Untertitel"             descrSubtitle
      , mdval0 "Titel engl."            descrTitleEnglish
      , mdval0 "Titel lat."             descrTitleLatin
      , mdval0 "Kommentar (Aufnahme)"   descrComment
      , mdval0 "Kommentar (Kopie)"      descrCommentImg
      , mdval0 "Aufnahmedatum"          exifCreateDate
      , mdval0 "Adresse"                descrAddress
      , mdval0 "Ort"                    descrLocation
      , mdMap  "Position"
      , mdval0 "Höhe"                   descrGPSAltitude
      , mdLink "Web"                    descrWeb
      , mdLink "Wikipedia"              descrWikipedia
      , mdKw   "Schlüsselwörter"
      , mdval0 "Kamera"                 exifModel
      , mdval  "Objektiv"               compositeLensSpec
      , mdval  "Objektiv Typ"           compositeLensID
      , mdval0 "Brennweite"             exifFocalLength
      , mdval0 "Brennweite in 35mm"     exifFocalLengthIn35mmFormat
      , mdval0 "Belichtungszeit"        exifExposureTime
      , mdval0 "Blende"                 exifFNumber
      , mdval0 "Belichtungskorrektur"   exifExposureCompensation
      , mdval0 "ISO"                    exifISO
      , mdval  "Belichtungsmessung"     exifExposureMode
      , mdval  "Aufnahmebetriebsart"    exifExposureProgram
      , mdval0 "Entfernung"             makerNotesFocusDistance
      , mdval  "Tiefenschärfe"          compositeDOF
      , mdDeg  "Sichtfeld"              compositeFOV
      , mdval  "Hyperfokale Distanz"    compositeHyperfocalDistance
      , mdval  "Aufnahmemodus"          makerNotesShootingMode
      , mdval  "Weißabgleich"           exifWhiteBalance
      , mdval0 "Aufnahmezähler"         makerNotesShutterCount
      , mdval0 "Geometrie"              compositeImageSize
      , mdval0 "Bilddatei"              fileRefImg
      , mdval0 "Dateityp"               fileMimeType
      , mdMPX  "Megapixel"
      , mdval0 "Dateigröße"             fileFileSize
      , mdval0 "Animation: Wiederh."    gifAnimationIterations
      , mdval0 "Animation: Dauer"       gifDuration
      , mdval0 "Animation: # Bilder"    gifFrameCount
      , mdval0 "Video-Dauer"            quickTimeDuration
      , mdval0 "Frame-Rate"             quickTimeVideoFrameRate
      , mdval0 "Bild-Kopie"             fileRefJpg
      , mdval0 "Raw-Datei"              fileRefRaw
      , mdTs   "Bearbeitet"
      , mdRat  "Bewertung"
      ]

gpsToHtml :: Text -> Html
gpsToHtml val =
  H.a ! href (toValue url) $
  toHtml $ formatDegree val
  where
    url :: Text
    url  = "https://maps.google.de/maps/@"
           <>
           (val & isoString %~ (isoGoogleMapsDegree #))
           <>
           ",17z"

    -- subst " deg" by degree char '\176'

formatDegree :: Text -> Text
formatDegree = T.replace " deg" "\176"
      -- t & isoString %~ SP.sedP (const "\176") (SP.string " deg")

-- ----------------------------------------

imgRef :: Text -> Text -> Text
imgRef theImgGeoDir theImgRef
  | isempty theImgRef = mempty
  | otherwise         = cond ("/" <>) theImgGeoDir <> theImgRef

imgRef' :: Text -> Text -> Text -> Text
imgRef' theImgGeoDir theIconGeoDir theIconRef
  | T.null theImgGeoDir = theIconRef
  | otherwise           = theIconGeoDir <> theIconRef

orgGeoDir :: Text
orgGeoDir = geoar'org ^. isoText

cond :: (Text -> Text) -> Text -> Text
cond f s
  | T.null s  = s
  | otherwise = f s

infixr 6 <:>

(<:>) :: Text -> Text -> Text
x <:> y
  | T.null y = x
  | otherwise = x <> ": " <> y

-- ----------------------------------------
--
-- translate/normalize metadata text

transMDVal :: Text -> Text
transMDVal t = fromMaybe t $ M.lookup t mdValMap

mdValMap :: Map Text Text
mdValMap = M.fromList
  [ ("Unknown (A3 38 5C 8E 34 40 CE 8E)", "AF-P DX Nikkor 70-300mm f/4.5-6.3G ED")
    -- shooting mode
  , ("Aperture Priority", "Zeitautomatik (A)")
  , ("Continuous", "Kontinuierlich")
  , ("Continuous, Auto ISO", "Kontinuierlich, ISO-Automatik")
  , ("Continuous, Exposure Bracketing", "Kontinuierlich, Belichtungsreihe")
  , ("Continuous, Exposure Bracketing, Auto ISO", "Kontinuierlich, Belichtungsreihe, ISO-Automatik")
  , ("Continuous, Self-timer", "Kontinuierlich, Selbstauslöser")
  , ("Delay", "Verzögert")
  , ("Delay, Exposure Bracketing", "Verzögert, Belichtungsreihe")
  , ("Delay, Self-timer", "Verzögert, Selbstauslöser")
  , ("Food", "Lebensmittel")
  , ("Intelligent Auto", "Intelligente Automatik")
  , ("Macro", "Makro")
  , ("Manual", "Manuell")
  , ("Movie Preview", "Video Vorschau")
  , ("Night Scenery", "Nachszene")
  , ("Panorama Assist", "Panorama Unterstützung")
  , ("Panorama", "")
  , ("Portrait", "")
  , ("Program", "Programm (P)")
  , ("Scenery", "Landschaft")
  , ("Single-Frame", "Einzelbild")
  , ("Single-Frame, Auto ISO", "Einzelbild, ISO-Automatik")
  , ("Single-Frame, Auto ISO, [9]", "Einzelbild, ISO-Automatik")
  , ("Single-Frame, Exposure Bracketing", "Einzelbild, Belichtungsreihe")
  , ("Single-Frame, Exposure Bracketing, Auto ISO", "Einzelbild, Belichtungsreihe, ISO-Automatik")
  , ("Sunset", "Sonnenuntergang")
  , ("Unknown (60)", "")
    -- exposure mode
  , ("Auto bracket", "Automatik, Belichtungsreihe")
  , ("Auto", "Automatik")
    -- exposure program
  , ("Aperture-priority AE", "Zeitautomatik (A)")
  , ("Creative (Slow speed)", "Kreativ (langsam)")
  , ("Landscape", "Landschaft")
  , ("Not Defined", "")
  , ("Program AE", "Program-Automatik (P)")
  , ("Shutter speed priority AE", "Blendenautomatik (S)")
  ]

-- ----------------------------------------
