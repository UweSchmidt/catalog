module Catalog.GenPages
  ( Req'
  , JPage(..)
  , emptyReq'
  , processReqMediaPath
  , processReqImg
  , processReqPage
  , rType
  , rGeo
  , rPathPos
  )
where

-- catalog
import Data.Prim
       ( Alternative(empty, (<|>))
       , Field1(_1)
       , Field2(_2)
       , Geo(..)
       , GeoAR
       , IsEmpty(isempty)
       , IsoString(isoString)
       , IsoText(isoText)
       , Ixed(ix)
       , LazyByteString
       , Lens
       , Lens'
       , MonadPlus(mzero)
       , ObjId
       , Path
       , PathPos
       , Pos
       , ReqType(..)
       , Text
       , Traversal'
       , (#)
       , (%~)
       , (&)
       , (.~)
       , (>=>)
       , (^.)
       , (^..)
       , (^?)
       , addNameSuffix
       , concPath
       , consPath
       , from
       , fromMaybe
       , geo'org
       , isAlphaNum
       , isImgMT
       , isJpgMT
       , isJust
       , isMovieMT
       , isTxtMT
       , isWackelGifMT
       , isoEpochTime
       , isoPicNo
       , isoSeqList
       , listToMaybe
       , mkGeoAR
       , mkPath
       , msgPath
       , p'archive
       , p'docroot
       , p'gen'icon
       , p'qmark
       , ps'archive
       , ps'docroot
       , readPath
       , reqType2AR
       , snocPath
       , substPathName
       , tailPath
       , take1st
       , to
       , toText
       , unless
       , viewBase
       )
import Data.ImgNode
       ( ImgRef'(ImgRef)
       , ImgRef
       , theColEntries
       , theParts
       , theColObjId
       , colEntry'
       , theColBlog
       , theColImg
       , theMetaData
       , thePartNames
       , thePartNames'
       )

import Data.ImgTree
       ( ImgNode )

import Data.MetaData
       ( MetaData
       , metaTextAt
       , metaDataAt
       , metaName
       , lookupGeo
       , lookupGeoOri
       , lookupMimeType
       , lookupRating

         -- MetaKey's
       , descrComment
       , descrDuration
       , descrSubtitle
       , descrTitle
       , fileRefImg
       , fileRefJpg
       , fileRefMedia
       , fileRefRaw
       , imgNameRaw
       , imgRating
       )
import Data.TextPath
       ( baseNameMb
       , ymdNameMb
       , takeDir
       )

import Catalog.Html.Templates.Blaze2
       ( colPage'
       , txtPage'
       , picPage'
       , movPage'
       , gifPage'
       )

-- catalog-polysemy
import Catalog.Effects
       ( Eff'ISEL
       , Eff'ISE
       , EffError
       , EffIStore
       , EffFileSys
       , EffCatEnv
       , EffNonDet
       , NonDet
       , Sem
       , TextPath
       , log'trc
       , log'dbg
       , setModiTime
       , readFileT
       , getModiTime
       , createDir
       , throw
       , catch
       , runMaybeEmpty
       , runMaybe
       , pureMaybe
       )
import Catalog.GenImages
       ( Eff'Img
       , GeoOri
       , createResizedImage
       , createVideoIcon
       , createResizedImage1
       , createVideoIcon1

       , genIcon
       , genBlogHtml
       , getThumbnailImage
       , resizeGeo'
       )
import Catalog.Html
       ( isPano )

import Catalog.ImgTree.Access
       ( objid2path
       , getImgVals
       , getImgVal
       , getImgMetaData
       , getImgParent
       , colEntryAt
       , getIdNode'
       )
import Catalog.TextPath
       ( toFileSysPath )

import Catalog.TimeStamp
       ( nowAsIso8601 )

-- libraries
import           Data.Aeson ( ToJSON )
import qualified Data.Aeson           as J
import qualified Data.Sequence        as Seq
import qualified Data.Text            as T
import qualified Text.Blaze.Html      as Blaze
import           Text.Blaze.Html.Renderer.Utf8
                                      ( renderHtml )

-- ----------------------------------------

type IdNode  = (ObjId, ImgNode)

data Req' a
  = Req' { _rType    :: ReqType      -- type
         , _rPathPos :: PathPos      -- collection path and maybe index
         , _rGeo     :: Geo          -- size of image or screen
         , _rVal     :: a            -- varying data when processing request
         }

type Req'IdNode                a = Req'              (IdNode,  a)
type Req'IdNode'ImgRef         a = Req'IdNode        (ImgRef,  a)

-- --------------------

deriving instance Show a => Show (Req' a)

emptyReq' :: Req' ()
emptyReq' =
  Req' { _rType    = RRef
       , _rPathPos = (mempty, Nothing)
       , _rGeo     = geo'org
       , _rVal     = ()
       }

-- --------------------

rType :: Lens' (Req' a) ReqType
rType k r = (\ new -> r {_rType = new}) <$> k (_rType r)

rPathPos :: Lens' (Req' a) PathPos
rPathPos k r = (\ new -> r {_rPathPos = new}) <$> k (_rPathPos r)

rPath :: Lens' (Req' a) Path
rPath = rPathPos . _1

rPos :: Lens' (Req' a) Pos
rPos = rPathPos . _2

rGeo :: Lens' (Req' a) Geo
rGeo k r = (\ new -> r {_rGeo = new}) <$> k (_rGeo r)

rVal :: Lens (Req' a) (Req' b) a b
rVal k r = (\ new -> r {_rVal = new}) <$> k (_rVal r)

rIdNode :: Lens' (Req'IdNode a) IdNode
rIdNode = rVal . _1

rColId :: Lens' (Req'IdNode a) ObjId
rColId = rIdNode . _1

rColNode :: Lens' (Req'IdNode a) ImgNode
rColNode = rIdNode . _2

rImgRef :: Lens' (Req'IdNode'ImgRef a) ImgRef
rImgRef = rVal . _2 . _1

{-
toReq'IdNode :: Req'IdNode a -> Req'IdNode ()
toReq'IdNode r = r & rVal . _2 .~ ()
-}
-- ----------------------------------------
--
-- commands working in MaybeT Cmd

-- invariant fpr Req'IdNode:
--
-- if pos isn't there, the IdNode is the collection itself
-- if pos is there, IdNode is the collection, where pos determines the img
-- if pos is there, it's an image page, else it's a collection page

normAndSetIdNode :: (Eff'ISE r, EffNonDet r) => Req' a -> Sem r (Req'IdNode a)
normAndSetIdNode = setIdNode >=> normPathPos


-- check the existence of a path
-- and add (objid, imgnode) to the request

setIdNode :: Eff'ISE r => Req' a -> Sem r (Req'IdNode a)
setIdNode r = do
  i'n <- getIdNode' (r ^. rPath)
  return (r & rVal %~ (i'n, ))


-- if a path ("/abx/def", Just i) points to a collection "ghi"
-- the path is normalized to ("/abc/def/ghi", Nothing)

normPathPos :: (Eff'ISE r, EffNonDet r) => Req'IdNode a -> Sem r (Req'IdNode a)
normPathPos r =
  ( do pos <- pureMaybe (r ^. rPos)
       ce  <- colEntryAt pos (r ^. rColNode)
       colEntry'
         (const $ return r)
         (normPathPosC r)
         ce
  )
  <|>
  return r

normPathPosC :: Eff'ISE r => Req'IdNode a -> ObjId -> Sem r (Req'IdNode a)
normPathPosC r c' =
  do p' <- objid2path c'
     setIdNode (r & rVal     %~ snd         -- forget IdNode
                  & rPathPos .~ (p', mzero) -- set path and no pos
               )                            -- set new id and node


-- compute a req with a path and pos
-- this is id when an image is requested
-- in case of a collection the parent col and the pos there are
-- computed
--
-- inverse of normPathPos

denormPathPos :: (Eff'ISE r, EffNonDet r)
              => Req'IdNode a -> Sem r (Req'IdNode a)
denormPathPos r =
  ( do _pos <- pureMaybe (r ^. rPos)
       return r
  )
  <|>
  ( do par'i <- getImgParent (r ^.rColId)
       par'n <- getImgVal    par'i
       par'p <- objid2path   par'i
       par'x <- pureMaybe $ lookupOId (r ^. rColId) par'n
       return
         (r & rPathPos .~ (par'p, Just par'x)
            & rIdNode  .~ (par'i, par'n)
         )
  )
  where
    lookupOId oid pnd =
      Seq.findIndexL
      ((== oid) . (^. theColObjId))
      (pnd ^. theColEntries)

-- --------------------
--
-- check whether an image ref is legal
-- and add the image ref to the result
-- in case of a ref to a collection, the empty ImgRef is set
-- the collection case does not occur with normalized paths

setImgRef :: (Eff'ISE r, EffNonDet r)
          => Req'IdNode a -> Sem r (Req'IdNode'ImgRef a)
setImgRef r = do
  pos <- pureMaybe (r ^. rPos)
  ce  <- colEntryAt pos (r ^. rColNode)
  colEntry'
      (\ ir -> return (r & rVal . _2 %~ (ir, )))
      (const mzero)
      ce

-- add the col image ref or col blog ref to the request

setColImgRef :: (Eff'ISE r, EffNonDet r)
             => Req'IdNode a -> Sem r (Req'IdNode'ImgRef a)
setColImgRef = setColRef' theColImg

setColBlogRef :: (Eff'ISE r, EffNonDet r)
              => Req'IdNode a -> Sem r (Req'IdNode'ImgRef a)
setColBlogRef = setColRef' theColBlog

setColRef' :: (Eff'ISE r, EffNonDet r)
           => Traversal' ImgNode (Maybe ImgRef)
           -> Req'IdNode a
           -> Sem r (Req'IdNode'ImgRef a)
setColRef' theC r = do
  ir <- pureMaybe $ r ^? rColNode . theC . traverse
  return (r & rVal . _2 %~ (ir, ))

-- --------------------
-- compute the .jpg url (if there) of a request
-- used in page generation for prefetching the next image
-- before moving to the next image page

toMediaUrl :: (Eff'Img r, EffNonDet r)
           => Req'IdNode a -> Sem r TextPath
toMediaUrl r = do
  r' <- setImgRef r >>= toMediaReq
  case r' ^. rType of
    RImg -> return $ toUrlPath r'  -- .jpg images
                                   -- toUrlPath is sufficient, no need for toUrlPath'
    _    -> mzero                  -- .txt, .md, .mp4

-- compute the metadata of an img reg

toImgMeta :: (Eff'ISE r, EffNonDet r)
          => Req'IdNode a -> Sem r MetaData
toImgMeta r =
  case r ^. rPos of
    Just _pos
      -> do ir <- (^. rImgRef) <$> setImgRef r
            getImgMetaData ir
    Nothing
      -> return $ r ^. rColNode . theMetaData

-- --------------------
--
-- compute path of a media file (img/movie/text/...)

processReqMediaPath :: (Eff'ISEL r)
                    => Req' a -> Sem r [Path]
processReqMediaPath r =
  fromMaybe [] <$> runMaybe (processReqMediaPath' r)

processReqMediaPath' :: (Eff'ISEL r, EffNonDet r)
                    => Req' a -> Sem r [Path]
processReqMediaPath' r0 = do
  r1 <- normAndSetIdNode r0

  ( -- collection entry with an image ref
    do
      r2 <- setImgRef r1
      p2 <- (p'archive <>) <$> toSourcePath r2

      log'trc $ msgPath p2 "processRegMediaPath: path="
      return [p2]
    )
    <|>
    ( -- image entry with paths for all parts
      do
        p <- objid2path (r1 ^. rIdNode . _1)

        let ns = r1 ^.. rIdNode . _2 . theParts . thePartNames
        let ps = map (`substPathName` p) ns

        log'trc $ "processRegMediaPath: paths="
                  <> (show ps ^. isoText)
        return ps
    )

-- --------------------
--
-- handle an img/icon request

processReqImg' :: (Eff'Img r, EffNonDet r)
               => Req' a -> Sem r Path
processReqImg' r0 = do
  r1 <- normAndSetIdNode r0

  let dstPath = toUrlPath'' r1

  log'trc $ msgPath dstPath "processReqImg: "

  case r1 ^. rPos of
    -- create an icon from a media file
    Just _pos -> do
      r2 <- setImgRef  r1
      genReqImg r2

    -- create an icon for a collection
    Nothing ->
      ( do r2 <- setColImgRef  r1
                 <|>
                 setColBlogRef r1
           genReqImg r2
      )
      <|>
      ( do _ <- createIconFromObj r1 dstPath
           return dstPath
      )

-- ----------------------------------------
--
-- handle a html page request

processReqPage' :: (Eff'Img r, EffNonDet r)
                => Req' a -> Sem r LazyByteString
processReqPage' r0 = do
  r1 <- normAndSetIdNode r0
  log'trc $ "processReqPage: " <> toUrlPath r1

  case r1 ^. rPos of
    -- create an image page
    Just _pos -> do
      r2 <- setImgRef      r1
      genReqImgPage r2

    -- create a collection page
    Nothing -> do
      genReqColPage r1

-- ----------------------------------------
--
-- main entry points

processReqImg  :: (Eff'Img r)
               => Req' a -> Sem r Path
processReqImg  = processReq processReqImg'

processReqPage :: (Eff'Img r)
               => Req' a -> Sem r LazyByteString
processReqPage = processReq processReqPage'

processReq :: (EffError r)
           => (Req' a -> Sem (NonDet ': r) b) -> Req' a -> Sem r b
processReq cmd r0 = do
  res <- runMaybe (cmd r0)
  case res of
    Nothing -> throw @Text "processReq failed"
    Just x  -> return x

-- ----------------------------------------
--
-- build file paths from a request

-- the "raw" path of a request
-- a static file path or a collection path,
-- maybe with index into the col
-- prefixed by a geometry spec
--
-- example: /160x120/archive/pictures/abc/pic-0001

toRawPath :: Req' a -> Path
toRawPath r =
  geo' `consPath` path' `concPath` pos'
  where
    path' = r ^. rPath
    geo'  = r ^. rGeo  . isoText . from isoText
    pos'  = maybe mempty mk1 $ r ^. rPos
      where
        mk1 :: Int -> Path
        mk1 x = x ^. isoPicNo . isoText . from isoText . to mkPath

-- --------------------
--
-- build the raw path and add appropriate extension
-- and prefix with request type
--
-- example: /icon/160x120/archive/pictures/abc/pic-0001.jpg
--          /page/1920x1200/archive/pictures/abc.html

toUrlPath :: Req' a -> TextPath
toUrlPath r0 = toUrlPath'' r0 ^. isoText

toUrlPath'' :: Req' a -> Path
toUrlPath'' r0 =
  p'docroot `concPath` px `consPath` fp
  where
    r   = unifyIconPath r0
    ty  = r ^. rType
    fp' = toRawPath r
    fp  = fp' & viewBase . _2 %~ addNameSuffix (toUrlExt ty)
    px  = ty ^. isoText . from isoText

    -- scaling of icons smaller than 160x120 with fixed aspect ratio
    -- is done in browser, so the # of generated icons
    -- is reduced

    unifyIconPath r'
      | RIcon   <- r' ^. rType
      , Geo w h <- r' ^. rGeo
      , w < 160 && h < 120    = r' & rGeo .~ Geo 160 120
      | otherwise             = r'

-- all pages and media files are accessed by collection path and img ix
-- except movies and wackelgifs, these are served as static files and are
-- referenced directly by the path to the movie

toUrlPath' :: Eff'Img r => Req'IdNode'ImgRef a -> Sem r TextPath
toUrlPath' r = case r ^. rType of
  RMovie -> toUrlImgPath r
  RGif   -> toUrlImgPath r
  _      -> return $ toUrlPath r

toUrlImgPath :: EffIStore r => Req'IdNode'ImgRef a -> Sem r TextPath
toUrlImgPath r = do
  ip <- toSourcePath r
  return $ (p'archive `concPath` ip) ^. isoText

toUrlExt :: ReqType -> Text
toUrlExt RPage  = ".html"
toUrlExt RPage1 = ".html"
toUrlExt RIcon  = ".jpg"
toUrlExt RIconp = ".jpg"
toUrlExt RImg   = ".jpg"
toUrlExt RBlog  = ".html"
toUrlExt RMovie = ".mp4"
toUrlExt RGif   = ".gif"
toUrlExt RJson  = ".json"
toUrlExt _      = ""

-- --------------------
--
-- build the source path for an image file
-- from a col entry and an ImgRef
-- used for converting the image file into
-- a file required by the request url
--
-- example usage: genImage    r (toSourcePath r) (toUrlPath r)
--                genBlogPage r (toSourcePath r) (toUrlPath r)

toSourcePath :: EffIStore r => Req'IdNode'ImgRef a -> Sem r Path
toSourcePath r = do
  p <- objid2path i
  return (tailPath . substPathName nm $ p)
  where
    ImgRef i nm = r ^. rImgRef

toCachedImgPath :: EffIStore r => Req'IdNode'ImgRef a -> Sem r Path
toCachedImgPath r = do
  p <- toSourcePath r
  return $ addP p
  where
    addP p =
      dr `concPath` ty `consPath` geo `consPath` ar `concPath` p'
      where
        dr  = readPath ps'docroot
        ty  = r ^. rType . isoText . from isoText
        geo = r ^. rGeo  . isoText . from isoText
        ar  = readPath ps'archive
        p'  = p & viewBase . _2 . isoText %~ jpg

        jpg n
          | ".jpg" `T.isSuffixOf` n = n
          | otherwise               = n <> ".jpg"

-- the url for a media file, *.jpg or *.mp4 or ...
-- is determined by the request type

toMediaReq :: Eff'Img r => Req'IdNode'ImgRef a -> Sem r (Req'IdNode'ImgRef a)
toMediaReq r = do
  md <- getImgMetaData (r ^. rImgRef)
  return (r & rType .~ ty (lookupMimeType md))
  where
    ty mt
      | isJpgMT   mt
        ||
        isImgMT       mt = RImg
      | isWackelGifMT mt = RGif
      | isMovieMT     mt = RMovie
      | otherwise        = RPage

-- ----------------------------------------
--
-- commands for icon and image generation

-- dispatch icon generation over media type (jpg, txt, md, mp4, wackelgif)

genReqImg :: Eff'Img r
          => Req'IdNode'ImgRef a -> Sem r Path
genReqImg r = do
  srcMd   <- getImgMetaData (r ^. rImgRef)
  srcPath <- toSourcePath    r
  imgPath <- toCachedImgPath r

  log'trc $ msgPath srcPath "genReqImg sp="

  case lookupMimeType srcMd of
    ity
      | isJpgMT ity
        ||
        isImgMT ity
        ||
        isWackelGifMT ity -> do   -- for Wackelgif icons a .jpg copy is generated
          createCopyFromImg
            (lookupGeoOri srcMd)
            geo
            srcPath
            imgPath

      | isMovieMT ity -> do
          mir <- toMovieIconReq r
          case mir of
            -- rule .1: use a .jpg copy of movie file
            Just r' -> do
              srcPath' <- toSourcePath r'
              srcMd'   <- getImgMetaData (r ^. rImgRef)

              log'trc $ msgPath srcPath' "genReqImg: icon for movie sp="
              createVideoIconFromImg1
                (lookupGeoOri srcMd')
                geo
                srcPath'
                imgPath

            Nothing -> do
              -- rule .2: try to extract a thumbnail out of the video file
              let thumbNail :: Eff'Img r => Sem r Path
                  thumbNail = do
                    tmpPath <- toCachedImgPath
                               ( r & rType .~ RMovie
                                   & rGeo  .~ geo'org
                               )
                    -- extract thumbnail from mp4
                    withCache  getThumbnailImage    srcPath tmpPath
                    withCache (createVideoIcon geo) tmpPath imgPath
                    return imgPath

              let fallBack :: Eff'Img r => Text -> Sem r Path
                  fallBack _e = do
                    createVideoIconFromImg geo p'qmark imgPath

              catch @Text thumbNail fallBack

      | isTxtMT ity -> do
          -- read text from source file
          -- if no text there,
          -- fall back to create icon from object path

          str <- getTxtFromFile srcPath
          if isempty str
            then createIconFromObj    r       imgPath
            else createIconFromString geo str imgPath

      | otherwise ->
          abortR "genReqIcon: no icon for image type" r
  where
    geo = mkGeoAR (r ^. rGeo) (reqType2AR $ r ^. rType)

----------------------------------------
--
-- How to get an icon out of a movie?
-- There are 3 steps
-- .1 look for an .jpg or other image in the parts of the image referenced by i
--    if an image exists, take that
-- .2 try to extract a thumnail out of the movie file with exiftool
--    success -> take the thumnail
-- .3 fallback: generate an icon from text "movie"
--    boring but works
--
-- to MovieIconPath checks the first posibility

toMovieIconReq :: Eff'ISE r
               => Req'IdNode'ImgRef a -> Sem r (Maybe (Req'IdNode'ImgRef a))
toMovieIconReq r = do
  n <- getImgVal i
  let nms = n ^.. theParts
               .  thePartNames' (\ ty -> isJpgMT ty || isImgMT ty)
  return ( (\ nm -> r & rImgRef .~ ImgRef i nm)
           <$>
           listToMaybe nms
         )
  where
    ImgRef i _nm = r ^. rImgRef

----------------------------------------
--
-- read text from a file (blog entry) to generate an icon

getTxtFromFile :: (EffError r, EffFileSys r, EffCatEnv r)
               => Path -> Sem r Text
getTxtFromFile srcPath = do
  cut 32
    . T.concat
    . take 1
    . filter (not . T.null)
    . map cleanup
    . T.lines
    <$>
    ( toFileSysPath srcPath >>= readFileT )
  where
    cleanup :: Text -> Text
    cleanup = T.dropWhile (not . isAlphaNum)

    cut :: Int -> Text -> Text
    cut l t
      | T.length t <= l = t
      | otherwise       = T.take (l - 3) t <> "..."


-- create an icon from the title of the path of a collection

createIconFromObj :: Eff'Img r
                  => Req'IdNode a -> Path -> Sem r Path
createIconFromObj r dstPath = do
  log'trc $ msgPath dstPath "createIconFromObj: "

  -- read the collection title
  txt1 <- getImgVals
          (r ^. rColId)
          (theMetaData . metaTextAt descrTitle)
  log'dbg $ "createIconFromObj: " <> txt1

  txt2 <- if isempty txt1
          -- if no title there, extract text from path
          then do
               p <- objid2path (r ^. rColId)
               return $ p ^. isoText . to path2txt
          else return txt1
  log'dbg $ "createIconFromObj: " <> txt2

  createIconFromString geo txt2 dstPath
    where
      geo = mkGeoAR (r ^. rGeo) (reqType2AR $ r ^. rType)

      path2txt :: Text -> Text
      path2txt f
        | Just (y, Nothing)           <- ymd = y
        | Just (y, Just (m, Nothing)) <- ymd = toN m <> "." <> y
        | Just (y, Just (m, Just d))  <- ymd = toN d <> "." <> toN m <> "." <> y
        | Just n <- nm                       = n
        | otherwise                          = "?"
        where
          ymd = ymdNameMb  f
          nm  = baseNameMb f

          toN :: Text -> Text
          toN = add0 . T.dropWhile (== '0')
            where
              add0 t
                | T.null t  = "0"
                | otherwise = t

-- --------------------
--
-- create an image copy with a given geometry
-- from a source image sp0
--
-- the copy is stored under the path of the image

createCopyFromImg :: Eff'Img r
                   => GeoOri -> GeoAR -> Path -> Path -> Sem r Path
createCopyFromImg s'geo = createCopyFromImg' (createResizedImage1 s'geo)

createVideoIconFromImg :: Eff'Img r
                   => GeoAR -> Path -> Path -> Sem r Path
createVideoIconFromImg = createCopyFromImg' createVideoIcon

createVideoIconFromImg1 :: Eff'Img r
                   => GeoOri -> GeoAR -> Path -> Path -> Sem r Path
createVideoIconFromImg1 s'geo = createCopyFromImg' (createVideoIcon1 s'geo)

createCopyFromImg' :: Eff'Img r
                   => (GeoAR -> Path -> Path -> Sem r ())
                   ->  GeoAR -> Path -> Path -> Sem r Path
createCopyFromImg' createResized geo sp ip =
  catch @Text
  ( do withCache (createResized geo) sp ip
       return ip
  )
  (\ _e -> createIconFromString geo "broken\nimage" ip)

-- --------------------

createIconFromString :: Eff'Img r
                     => GeoAR -> Text -> Path -> Sem r Path
createIconFromString = createIconFromString' createResizedImage

{- not in use
createVideoIconFromString :: Eff'Img r
                     => GeoAR -> Text -> Path -> Sem r Path
createVideoIconFromString = createIconFromString' createVideoIcon
-- -}

createIconFromString' :: Eff'Img r
                      => (GeoAR -> Path -> Path -> Sem r ())
                      -> GeoAR -> Text -> Path -> Sem r Path
createIconFromString' createResized geo t dp = do
  sp <- createRawIconFromString t
  withCache (createResized geo) sp dp
  return dp

createRawIconFromString :: Eff'Img r
                        => Text -> Sem r Path
createRawIconFromString t = do
  log'trc $ "createRawIconFromString: " <> toText (imgPath, t)
  genIcon imgPath t
  return imgPath
    where
      imgPath :: Path
      imgPath =
        p'gen'icon `snocPath` (isoText # t')

      t' :: Text
      t' = (t & isoString %~ str2fn) <> ".jpg"

      str2fn :: String -> String
      str2fn = concatMap urlEnc
        where
          urlEnc c
            | isAlphaNum c
              &&
              c <= toEnum 128 = [c]
            | otherwise       = "-" <> show (fromEnum c) <> "-"

-- ----------------------------------------
--
-- cache generated files, e.g. icons, scaled down images, ...
--
-- the cached entry gets the same time stamp as the source
-- cache hit only when both time stamps are the same

withCache :: (Eff'ISEL r, EffFileSys r, EffCatEnv r)
          => (Path -> Path -> Sem r ())
          ->  Path -> Path -> Sem r ()
withCache cmd sp dp = do
  sp' <- toFileSysPath sp
  dp' <- toFileSysPath dp
  sw  <- (isoEpochTime #) <$> getModiTime sp'
  dw  <- (isoEpochTime #) <$> getModiTime dp'

  unless (dw == sw && not (isempty dw)) $ do
    -- no cache hit
    -- execute command and
    -- set mtime of dest to mtime of source
    -- so cache hits are those with equal mtime timestamp (dw == ws)
    log'trc $ "withCache: cache miss: " <> toText ((sp, sw), (dp, dw))
    createDir (takeDir dp')
    cmd sp dp
    setModiTime (sw ^. isoEpochTime) dp'

-- ----------------------------------------
--
-- abort processing of a request

abortR :: EffError r => Text -> Req' a -> Sem r b
abortR msg r =
  throw @Text (msg <> ": req = " <> toUrlPath r)

-- --------------------
--
-- navigation ops

toParent :: (Eff'ISE r, EffNonDet r) => Req'IdNode a -> Sem r (Req'IdNode a)
toParent r = do
  r' <- denormPathPos r            -- r' has always a pos
  return ( r' & rPos .~ Nothing )  -- forget the pos
                                   -- the result is normalized

toPos' :: (Eff'ISE r, EffNonDet r)
       => (Int -> Int) -> Req'IdNode a -> Sem r (Req'IdNode a)
toPos' f r = do
  p <- denormPathPos r
  x <- pureMaybe (p ^. rPos)
  let x' = f x
  _ <- pureMaybe (p ^? rColNode . theColEntries . ix x')
  normPathPos (p & rPos .~ Just x')

toPrev :: (Eff'ISE r, EffNonDet r) => Req'IdNode a -> Sem r (Req'IdNode a)
toPrev = toPos' pred

toNext :: (Eff'ISE r, EffNonDet r) => Req'IdNode a -> Sem r (Req'IdNode a)
toNext = toPos' succ

toNextOrUp :: (Eff'ISE r, EffNonDet r) => Req'IdNode a -> Sem r (Req'IdNode a)
toNextOrUp r =
  toNext r
  <|>
  ( toParent r >>= toNextOrUp )

toChildOrNextOrUp :: (Eff'ISE r, EffNonDet r)
                  => Req'IdNode a -> Sem r (Req'IdNode a)
toChildOrNextOrUp r =
  toFirstChild r
  <|>
  toNextOrUp r

toFirstChild :: (Eff'ISE r, EffNonDet r) => Req'IdNode a -> Sem r (Req'IdNode a)
toFirstChild r
  | isJust (r ^. rPos) = empty                -- a picture doesn't have children
  | null   (r ^. rColNode . theColEntries)    -- empty collection
                       = empty
  | otherwise          = normPathPos (r & rPos .~ Just 0)

-- normalization must be done before
toChildren :: (Eff'ISE r, EffNonDet r) => Req'IdNode a -> Sem r [Req'IdNode a]
toChildren r =
  traverse normC $ zip [0..] (r ^. rColNode . theColEntries . isoSeqList)
  where
    normC (i, ce) =
      colEntry'
        (const $ return (r & rPos .~ Just i))
        (normPathPosC r)
        ce

toPrevNextPar :: (Eff'ISE r)
              => Req'IdNode a -> Sem r (PrevNextPar (Maybe (Req'IdNode a)))
toPrevNextPar r =
  PrevNextPar
  <$> runMaybe (toPrev            r)
  <*> runMaybe (toNext            r)
  <*> runMaybe (toParent          r)
  <*> runMaybe (toChildOrNextOrUp r)

-- ----------------------------------------

lookupPageCnfs :: ReqType -> Geo -> (Geo, Geo, Int)
lookupPageCnfs ty (Geo w _h)
  | ty == RPage1
    ||
    ty == RJson  = f2
  | otherwise    = f1
  where
    geo1600x160 = Geo 1600 160
    geo1200x120 = Geo 1200 120
    geo160x120  = Geo 160  120
    geo140x105  = Geo 140  105
    geo120x90   = Geo 120   90

    f1 | w <= 1280 = (geo120x90,  geo120x90,  w `div` 120 - 1)
       | w <= 1400 = (geo140x105, geo140x105, w `div` 140 - 1)
       | otherwise = (geo160x120, geo160x120, w `div` 160 - 1)

    f2 | w <= 1280 = (geo120x90,  geo1200x120, 0)
       | w <= 1400 = (geo140x105, geo1600x160, 0)
       | otherwise = (geo160x120, geo1600x160, 0)


-- ----------------------------------------

collectImgAttr :: Eff'Img r => Req'IdNode'ImgRef a -> Sem r MetaData
collectImgAttr r = do
  theMeta <- getImgMetaData ir
  theUrl  <- toMediaReq r >>= toUrlPath'  -- not toUrlPath due to RMovie, RGif
  theSrc  <- toSourcePath r
  let rnm  = theMeta ^. metaDataAt imgNameRaw . metaName
  let rp
        | isempty rnm = mempty
        | otherwise   = substPathName rnm theSrc ^. isoText
  return $
    theMeta
    & metaTextAt fileRefImg   .~ (theSrc ^. isoText)
    & metaTextAt fileRefMedia .~ (theUrl ^. isoText)
    & metaTextAt fileRefRaw   .~ rp
    & metaTextAt descrTitle   .~ take1st
                                 [ theMeta ^. metaTextAt descrTitle
                                 , nm ^. isoText
                                 ]
    & metaTextAt descrDuration .~ take1st
                                  [ theMeta ^. metaTextAt descrDuration
                                  , "1.0"
                                  ]
  where
    ir@(ImgRef _iOid nm) = r ^. rImgRef

-- --------------------

thePos :: Eff'ISE r => Req'IdNode a -> Sem r Text
thePos r =
  runMaybeEmpty $
  do r' <- denormPathPos r
     return (r' ^. rPos . traverse . isoPicNo . isoText)

-- ----------------------------------------
--
-- html/json page generation

genReqImgPage :: (Eff'Img r)
               => Req'IdNode'ImgRef a -> Sem r LazyByteString
genReqImgPage r =
  renderHtml . jPageToHtml <$> genReqImgPage' r

genReqImgPage' :: (Eff'Img r)
               => Req'IdNode'ImgRef a -> Sem r JPage
genReqImgPage' r = do
  now' <- nowAsIso8601

  this'meta             <- collectImgAttr r
  this'pos              <- thePos r

  let     this'mediaUrl  = this'meta ^. metaTextAt fileRefMedia
  let     this'url       = toUrlPath r  -- !!! no toUrlPath' due to RPage
  let     this'geo       = r ^. rGeo

  -- the urls of the siblings
  nav                   <- toPrevNextPar r
  let m2url              = maybe mempty ((^. isoText) . toUrlPath)
  let navRefs            = m2url <$> nav

  -- the image urls of the siblings
  let tomu mr            = (^. isoText)
                           <$> runMaybeEmpty (pureMaybe mr >>= toMediaUrl)
  navImgs               <- traverse tomu nav

  let star               = '\9733'
  let rating             = (\ x -> replicate x star ^. isoText) $
                           lookupRating this'meta
  let metaData           = this'meta -- add jpg filename and rating as stars
                           & metaTextAt fileRefJpg .~ (this'mediaUrl ^. isoText)
                           & metaTextAt imgRating  .~ rating
  let org'geo            = lookupGeo metaData
  let res'geo            = resizeGeo' org'geo this'geo

  mrq <- toMediaReq r

  let ipage = emptyJImgPage
              { _now        = now'
              , _imgMeta    = metaData
              , _imgNavRefs = navRefs
              , _imgNavImgs = navImgs
              , _imgPos     = this'pos
              , _imgUrl     = this'url
              , _imgGeo     = this'geo
              , _resGeo     = res'geo
              }

  case mrq ^. rType of

    --image page
    RImg -> do
      -- .jpg images may be shown in original size
      let org'mediaUrl   = toUrlPath
                           (r & rType .~ RImg
                              & rGeo  .~ geo'org
                           ) ^. isoText

      -- .jpg panoramas may be shown as moving fullscreen images
      let pano'mediaUrl  = maybe
                           mempty
                           (\ geo -> toUrlPath
                                     (r & rType .~ RImg
                                        & rGeo  .~ geo
                                     ) ^. isoText
                           )
                           (isPano this'geo org'geo)

      return ipage { _imgRType = RImg
                   , _orgUrl   = org'mediaUrl
                   , _panoUrl  = pano'mediaUrl
                   }

    -- wackelgif or mp4 video
    rty
      | rty == RMovie
        ||
        rty == RGif -> do

      return ipage {_imgRType = rty}


    -- blog page
    RPage -> do
      blogContents <- toSourcePath r >>= genBlogHtml

      return ipage { _imgRType = RPage
                   , _blogCont = blogContents
                   }

    _ ->
      return emptyJImgPage


-- ----------------------------------------
--
-- icons in a collection page
-- the icons are generated with a fixed aspect ratio (RIcon, not RIconp)

toIconDescr :: (Eff'ISE r) => Geo -> Req'IdNode a -> Sem r IconDescr
toIconDescr icon'geo r = do
  let r'url         = toUrlPath  r
  let r'iconurl     = toUrlPath (r & rType %~ iType
                                   & rGeo  .~ icon'geo
                                )
  r'meta           <- runMaybeEmpty (toImgMeta r)

  return $ IconDescr r'url r'iconurl r'meta
  where
    iType RPage1 = RImg   -- icons with variable width
    iType _      = RIcon  -- icons with fixed aspect ratio

-- --------------------

genReqColPage  :: (Eff'Img r)
               => Req'IdNode a -> Sem r LazyByteString
genReqColPage r =
  renderHtml . jPageToHtml <$> genReqColPage' r

genReqColPage' :: (Eff'Img r)
               => Req'IdNode a -> Sem r JPage
genReqColPage' r = do
  now'  <- nowAsIso8601

  let   this'ty          = r ^. rType
  let   this'geo         = r ^. rGeo
  let ( inav'geo,
        icon'geo,
        _icon'no )       = lookupPageCnfs this'ty this'geo

  let   this'meta        = r ^. rColNode . theMetaData
  this'icon             <- toIconDescr inav'geo r
  this'pos              <- thePos r

  -- the icons descr of the siblings
  nav   <- toPrevNextPar r
  nav'icons             <- traverse
                           (fmap (fromMaybe emptyIconDescr)
                            .
                            traverse (toIconDescr inav'geo)
                           )
                           nav
  -- the icon descr of the children
  cs                    <- runMaybeEmpty (toChildren r)
  cs'descr              <- traverse (toIconDescr icon'geo) cs
  c1'icon               <- fromMaybe emptyIconDescr
                           <$> traverse (toIconDescr inav'geo) (listToMaybe cs)

  this'blogContents     <- runMaybeEmpty
                           ( do r' <- setColBlogRef r
                                p' <- toSourcePath r'
                                genBlogHtml p'
                           )

  return JColPage { _colRType  = this'ty
                  , _now       = now'
                  , _colGeo    = this'geo
                  , _colPos    = this'pos
                  , _colMeta   = this'meta
                  , _colIcon   = this'icon
                  , _navIcons  = nav'icons
                  , _c1Icon    = c1'icon
                  , _contIcons = cs'descr
                  , _blogCont  = this'blogContents
                  }

-- ----------------------------------------
-- image attributes

data JPage
  = JImgPage { _imgRType   :: ReqType
             , _now        :: Text
             , _imgMeta    :: MetaData
             , _imgNavRefs :: PrevNextParPath
             , _imgNavImgs :: PrevNextParPath
             , _imgPos     :: Text
             , _imgUrl     :: TextPath
             , _imgGeo     :: Geo
             , _resGeo     :: Geo
             , _orgUrl     :: TextPath
             , _panoUrl    :: TextPath
             , _blogCont   :: Text
             }
  | JColPage { _colRType   :: ReqType
             , _now        :: Text
             , _colGeo     :: Geo
             , _colPos     :: Text
             , _colMeta    :: MetaData
             , _colIcon    :: IconDescr
             , _navIcons   :: PrevNextParIcons
             , _c1Icon     :: IconDescr
             , _contIcons  :: [IconDescr]
             , _blogCont   :: Text
             }

data PrevNextPar a =
  PrevNextPar { _prev :: a
              , _next :: a
              , _par  :: a
              , _fwrd :: a
              }
type PrevNextParPath  = PrevNextPar TextPath
type PrevNextParIcons = PrevNextPar IconDescr

data IconDescr =
  IconDescr { _targetUrl  :: Text
            , _iconUrl    :: Text
            , _targetMeta :: MetaData
            }

-- ----------------------------------------

deriving instance Functor     PrevNextPar
deriving instance Foldable    PrevNextPar
deriving instance Traversable PrevNextPar
deriving instance (Show a) => Show (PrevNextPar a)

instance ToJSON JPage where
  toJSON JImgPage
    { _imgRType   = i1
    , _now        = i2
    , _imgMeta    = i3
    , _imgNavRefs = i4
    , _imgNavImgs = i5
    , _imgPos     = i6
    , _imgUrl     = i7
    , _imgGeo     = i8
    , _resGeo     = i9
    , _orgUrl     = i10
    , _panoUrl    = i11
    , _blogCont   = i12
    } = J.object
        [ "imgRType"   J..= (i1 ^. isoText)
        , "now"        J..= i2
        , "imgMeta"    J..= i3
        , "imgNavRefs" J..= i4
        , "imgNavImgs" J..= i5
        , "imgPos"     J..= i6
        , "imgUrl"     J..= i7
        , "imgGeo"     J..= i8
        , "resGeo"     J..= i9
        , "orgUrl"     J..= i10
        , "panoUrl"    J..= i11
        , "blogCont"   J..= i12
        ]
  toJSON JColPage
    { _colRType   = c1
    , _now        = c2
    , _colGeo     = c3
    , _colPos     = c4
    , _colMeta    = c5
    , _colIcon    = c6
    , _navIcons   = c7
    , _c1Icon     = c8
    , _contIcons  = c9
    , _blogCont   = c10
    } = J.object
        [ "colRType"   J..= (c1 ^. isoText)
        , "now"        J..= c2
        , "colGeo"     J..= c3
        , "colPos"     J..= c4
        , "colMeta"    J..= c5
        , "colIcon"    J..= c6
        , "navIcons"   J..= c7
        , "c1Icon"     J..= c8
        , "contIcons"  J..= c9
        , "blogCont"   J..= c10
        ]


instance ToJSON a => ToJSON (PrevNextPar a) where
  toJSON PrevNextPar
    { _prev = x1
    , _next = x2
    , _par  = x3
    , _fwrd = x4
    } = J.object
        [ "prev" J..= x1
        , "next" J..= x2
        , "par"  J..= x3
        , "fwrd" J..= x4
        ]

instance ToJSON IconDescr where
  toJSON IconDescr
    { _targetUrl  = d1
    , _iconUrl    = d2
    , _targetMeta = d3
    } = J.object
        [ "targetUrl"  J..= d1
        , "iconUrl"    J..= d2
        , "targetMeta" J..= d3
        ]

-- ----------------------------------------

emptyJImgPage :: JPage
emptyJImgPage =
  JImgPage { _imgRType   = RRef
           , _now        = mempty
           , _imgMeta    = mempty
           , _imgNavRefs = emptyPrevNextPar
           , _imgNavImgs = emptyPrevNextPar
           , _imgPos     = mempty
           , _imgUrl     = mempty
           , _imgGeo     = mempty
           , _resGeo     = mempty
           , _orgUrl     = mempty
           , _panoUrl    = mempty
           , _blogCont   = mempty
           }

emptyPrevNextPar :: Monoid a => PrevNextPar a
emptyPrevNextPar =
  PrevNextPar { _prev = mempty
              , _next = mempty
              , _par  = mempty
              , _fwrd = mempty
              }

emptyIconDescr :: IconDescr
emptyIconDescr =
  IconDescr { _targetUrl  = mempty
            , _iconUrl    = mempty
            , _targetMeta = mempty
            }

-- ----------------------------------------
--
-- JPage conversion to Blaze.HTML
-- calling the ugly picPage', ... colPage' from Blaze module

jPageToHtml :: JPage -> Blaze.Html
jPageToHtml JImgPage
  { _imgRType   = rty
  , _now = now'
  , _imgMeta    = this'meta
  , _imgNavRefs = PrevNextPar
                  prev'url
                  next'url
                  par'url
                  fwrd'url
  , _imgNavImgs = PrevNextPar
                  prev'imgRef
                  next'imgRef
                  _par'imgRef
                  fwrd'imgRef
  , _imgPos     = this'pos
  , _imgUrl     = this'url
  , _imgGeo     = this'geo
  , _resGeo     = res'geo
  , _orgUrl     = org'mediaUrl
  , _panoUrl    = pano'mediaUrl
  , _blogCont   = blogContents
  } = let this'title    = this'meta ^. metaTextAt descrTitle
          this'subTitle = this'meta ^. metaTextAt descrSubtitle
          this'comment  = this'meta ^. metaTextAt descrComment
          this'duration = this'meta ^. metaTextAt descrDuration
          this'mediaUrl = this'meta ^. metaTextAt fileRefMedia
      in
      case rty of
        RImg ->
          picPage'
          "/"
          this'title
          now'
          this'title
          this'subTitle
          this'comment
          this'geo
          Nothing    -- old url scheme: thePanoGeoDir
          this'duration
          this'url
          this'pos
          next'url
          prev'url
          par'url
          fwrd'url
          ""    -- old url scheme: theImgGeoDir
          this'mediaUrl
          next'imgRef
          prev'imgRef
          fwrd'imgRef
          org'mediaUrl
          pano'mediaUrl
          this'meta
        _rty
          | rty == RMovie || rty == RGif ->
          (if rty == RMovie then movPage' else gifPage')
          "/"
          this'title
          now'
          this'title
          this'subTitle
          this'comment
          this'geo
          res'geo
          this'duration
          this'url
          this'pos
          next'url
          prev'url
          par'url
          fwrd'url
          this'mediaUrl
          next'imgRef
          prev'imgRef
          fwrd'imgRef
          this'meta
        RPage ->
          txtPage'
          "/"
          this'title
          now'
          this'duration
          this'url
          this'pos
          next'url
          prev'url
          par'url
          fwrd'url
          mempty
          next'imgRef
          prev'imgRef
          fwrd'imgRef
          blogContents
        _others ->
          mempty

jPageToHtml JColPage
  { _colRType  = this'ty
  , _now       = now'
  , _colGeo    = this'geo
  , _colPos    = this'pos
  , _colMeta   = this'meta
  , _colIcon   = this'icon
  , _navIcons  = nav'icons
  , _c1Icon    = c1'icon
  , _contIcons = cs'descr
  , _blogCont  = this'blogContents
  } = let IconDescr
            this'url
            this'iconurl
            _ = this'icon
          IconDescr
            c1'url
            c1'iconurl
            c1'meta = c1'icon

          this'title   = this'meta ^. metaTextAt descrTitle
          c1'title     = c1'meta   ^. metaTextAt descrTitle

          PrevNextPar
            prev'title
            next'title
            par'title
            _ = fmap ((^. metaTextAt descrTitle) . _targetMeta) nav'icons
          PrevNextPar
            prev'url
            next'url
            par'url
            fwrd'url = fmap _targetUrl nav'icons
          PrevNextPar
            prev'iconurl
            next'iconurl
            par'iconurl
            fwrd'iconurl = fmap _iconUrl nav'icons

          (_inav'geo, icon'geo, icon'no) = lookupPageCnfs this'ty this'geo

          cs'descr4 = zipWith
                      ( \ (IconDescr x1 x2 x3) i ->
                          ( x1
                          , x2
                          , let t3 = x3 ^. metaTextAt descrTitle in
                              if T.null t3
                              then (show i <> ". Bild") ^. isoText
                              else t3
                          , i ^. isoPicNo . isoText
                          )
                      )
                      cs'descr
                      [1..]

      in
        colPage'
        "/"
        this'title
        now'
        this'title
        this'geo
        "1.0"   -- theDuration
        this'url
        this'pos
        next'url
        prev'url
        par'url
        c1'url
        fwrd'url
        "" -- theImgGeoDir
        (icon'geo ^. isoText)
        this'iconurl
        next'iconurl
        prev'iconurl
        c1'iconurl
        fwrd'iconurl
        this'blogContents
        par'title
        par'iconurl
        next'title
        prev'title
        c1'title
        icon'no     -- == 0: floating icon layout
        cs'descr4
        this'meta

-- ----------------------------------------
