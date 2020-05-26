{-# LANGUAGE
    ConstraintKinds,
    DataKinds,
    DeriveFunctor,
    DeriveTraversable,
    FlexibleContexts,
    GADTs,
    OverloadedStrings,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    StandaloneDeriving,
    TupleSections,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)

module Catalog.GenPages
  ( Req'
  , emptyReq'
  , processReqImg
  , processReqPage
  , rType
  , rGeo
  , rPathPos
  )
where

-- catalog
import Data.Prim
import Data.ImgNode
import Data.ImgTree
import Data.MetaData                  ( MetaData
                                      , metaDataAt
                                      , descrComment
                                      , descrDuration
                                      , descrSubtitle
                                      , descrTitle
                                      , fileRefJpg
                                      , getRating
                                      , imgRating
                                      )
import Data.TextPath                  ( pathName2ImgType
                                      , baseNameMb
                                      , ymdNameMb
                                      , takeDir
                                      )

import Catalog.Html.Templates.Blaze2  ( colPage'
                                      , txtPage'
                                      , picPage'
                                      , movPage'
                                      )

-- catalog-polysemy
import Catalog.Effects
import Catalog.GenImages              ( Eff'Img
                                      , createResizedImage
                                      , genIcon
                                      , genBlogHtml
                                      , getImageSize
                                      , getThumbnailImage
                                      , resizeGeo'
                                      )
import Catalog.Html                   ( isPano )
import Catalog.ImgTree.Access
import Catalog.TextPath               ( toSysPath )
import Catalog.TimeStamp              ( nowAsIso8601 )

-- libraries
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

toMediaUrl :: (Eff'ISE r, EffNonDet r)
           => Req'IdNode a -> Sem r TextPath
toMediaUrl r = do
  r' <- toMediaReq <$> setImgRef r
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
      -> do ImgRef iOid _nm <- (^. rImgRef) <$> setImgRef r
            getMetaData iOid
    Nothing
      -> return $ r ^. rColNode . theMetaData

-- --------------------
--
-- handle an img/icon request

processReqImg' :: (Eff'Img r, EffNonDet r)
               => Req' a -> Sem r TextPath
processReqImg' r0 = do
  r1 <- normAndSetIdNode r0
  let dp = toUrlPath r1
  log'trc $ "processReqImg: " <> dp

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
      ( do _ <- createIconFromObj r1 dp
           return dp
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
               => Req' a -> Sem r TextPath
processReqImg  = processReq processReqImg'

processReqPage :: (Eff'Img r)
               => Req' a -> Sem r LazyByteString
processReqPage = processReq processReqPage'

processReq :: (EffError r)
           => (Req' a -> Sem (NonDet ': r) b) -> Req' a -> Sem r b
processReq cmd r0 = do
  res <- runMaybe (cmd r0)
  case res of
    Nothing -> throw @Text "emil"
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
  geo' `consPath` path' `snocPath` pos'
  where
    path' = r ^. rPath
    geo'  = r ^. rGeo  . isoText . from isoText
    pos'  = maybe mempty mk1 $ r ^. rPos
      where
        mk1 x = x ^. isoPicNo . isoText . from isoText

-- --------------------
--
-- build the raw path and add appropriate extension
-- and prefix with request type
--
-- example: /icon/160x120/archive/pictures/abc/pic-0001.jpg
--          /page/1920x1200/archive/pictures/abc.html

toUrlPath :: Req' a -> TextPath
toUrlPath r0 =
  (dr `concPath` px `consPath` fp) ^. isoText  --  ^. isoText <> toUrlExt ty
  where
    r   = unifyIconPath r0
    ty  = r ^. rType
    fp' = toRawPath r
    fp  = fp' & viewBase . _2 %~ addNameSuffix (toUrlExt ty)
    px  = ty ^. isoText . from isoText
    dr  = readPath ps'docroot
    -- scaling of icons smaller than 160x120 with fixed aspect ratio
    -- is done in browser, so the # of generated icons
    -- is reduced

    unifyIconPath r'
      | RIcon   <- r' ^. rType
      , Geo w h <- r' ^. rGeo
      , w < 160 && h < 120    = r' & rGeo .~ Geo 160 120
      | otherwise             = r'

-- all pages and media files are accesed by collection path and img ix
-- except movies, these are served as static files and are
-- referenced directly by the path to the movie

toUrlPath' :: EffIStore r => Req'IdNode'ImgRef a -> Sem r TextPath
toUrlPath' r
  | RMovie <- r ^. rType = toUrlImgPath r
  | otherwise            = return $ toUrlPath r

toUrlImgPath :: EffIStore r => Req'IdNode'ImgRef a -> Sem r TextPath
toUrlImgPath r = do
  ip <- toSourcePath r
  return $ (dr `concPath` px `consPath` ar) ^. isoText <> ip
  where
    ar  = readPath ps'archive
    dr  = readPath ps'docroot
    px  = r ^. rType . isoText . from isoText

toUrlExt :: ReqType -> Text
toUrlExt RPage  = ".html"
toUrlExt RPage1 = ".html"
toUrlExt RIcon  = ".jpg"
toUrlExt RIconp = ".jpg"
toUrlExt RImg   = ".jpg"
toUrlExt RBlog  = ".html"
toUrlExt RMovie = ".mp4"
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

toSourcePath' :: EffIStore r => Req'IdNode'ImgRef a -> Sem r Path
toSourcePath' r = do
  p <- objid2path i
  return $ (tailPath $ substPathName nm p)
  where
    ImgRef i nm = r ^. rImgRef

toSourcePath :: EffIStore r => Req'IdNode'ImgRef a -> Sem r TextPath
toSourcePath r = (^. isoText) <$> toSourcePath' r


toCachedImgPath :: EffIStore r => Req'IdNode'ImgRef a -> Sem r TextPath
toCachedImgPath r = do
  p <- toSourcePath' r
  return $ addP p ^. isoText
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

toMediaReq :: Req'IdNode'ImgRef a -> Req'IdNode'ImgRef a
toMediaReq r =
  r & rType .~ t
  where
    t =
      case pathName2ImgType (r ^. rImgRef . to _iname . isoText) of
        IMGjpg   -> RImg
        IMGimg   -> RImg
        IMGmovie -> RMovie
        IMGtxt   -> RPage
        _        -> RPage  -- unknown media type, should not occur

-- ----------------------------------------
--
-- commands for icon and image generation

-- dispatch icon generation over media type (jpg, txt, md, mp4)

genReqImg :: Eff'Img r
          => Req'IdNode'ImgRef a -> Sem r TextPath
genReqImg r = do
  sp <- toSourcePath r
  log'trc $ "genReqIcon sp=" <> sp

  ip <- toCachedImgPath r
  case ity of
    IMGjpg ->
      createCopyFromImg geo sp ip

    IMGimg ->
      createCopyFromImg geo sp ip

    IMGmovie -> do
      catch @Text
        ( do
            tp <- toCachedImgPath
                  ( r & rType .~ RMovie
                      & rGeo  .~ geo'org
                  )
            -- extract thumbnail from mp4
            withCache  getThumbnailImage       sp tp
            withCache (createResizedImage geo) tp ip
            return ip
        )
        (\ _e -> createIconFromString geo "movie" ip)

    IMGtxt -> do
      -- read text from source file
      -- if no text there,
      -- fall back to create icon from object path

      str <- getTxtFromFile sp
      if isempty str
        then createIconFromObj    r       ip
        else createIconFromString geo str ip

    _ ->
      abortR "genReqIcon: no icon for image type" r
  where
    geo = mkGeoAR (r ^. rGeo) (reqType2AR $ r ^. rType)
    ity = pathName2ImgType (r ^. rImgRef . to _iname . isoText)

-- read text from a file (blog entry) to generate an icon

getTxtFromFile :: (EffError r, EffFileSys r, EffCatEnv r)
               => TextPath -> Sem r Text
getTxtFromFile sp = do
  txt <- cut 32 . T.concat . take 1 .
         filter (not . T.null) . map cleanup . T.lines <$>
         ( do fp <- toSysPath sp
              readFileT (fp ^. isoTextPath)
         )
  return txt
  where
    cleanup :: Text -> Text
    cleanup = T.dropWhile (not . isAlphaNum)

    cut :: Int -> Text -> Text
    cut l t
      | T.length t <= l = t
      | otherwise       = T.take (l - 3) t <> "..."


-- create an icon from the title of the path of a collection

createIconFromObj :: Eff'Img r
                  => Req'IdNode a -> TextPath -> Sem r TextPath
createIconFromObj r dp = do
  log'trc $ "createIconFromObj: " <> dp

  -- read the collection title
  txt1 <- getImgVals
          (r ^. rColId)
          (theMetaData . metaDataAt descrTitle)
  log'trc $ "createIconFromObj: " <> txt1

  txt2 <- if isempty txt1
          -- if no title there, extract text from path
          then do
               p <- objid2path (r ^. rColId)
               return $ path2txt (p ^. isoText)
          else return txt1
  log'trc $ "createIconFromObj: " <> txt2

  createIconFromString geo txt2 dp
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
                  => GeoAR -> TextPath -> TextPath -> Sem r TextPath
createCopyFromImg geo sp ip =
  catch @Text
  ( do withCache (createResizedImage geo) sp ip
       return ip
  )
  (\ _e -> createIconFromString geo "broken\nimage" ip)

-- --------------------

createIconFromString :: Eff'Img r
                     => GeoAR -> Text -> TextPath -> Sem r TextPath
createIconFromString geo t dp = do
  sp <- createRawIconFromString t
  withCache (createResizedImage geo) sp dp
  return dp

createRawIconFromString :: Eff'Img r
                        => Text -> Sem r TextPath
createRawIconFromString t = do
  log'trc $ "createRawIconFromString: " <> toText (fp, t)
  genIcon fp t
  return fp
    where
      fp = ps'gen'icon ^. isoText <> "/" <> (t & isoString %~ str2fn) <> ".jpg"

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
          => (TextPath -> TextPath -> Sem r ())
          ->  TextPath -> TextPath -> Sem r ()
withCache cmd sp dp = do
  sp' <- (^. isoTextPath) <$> toSysPath sp
  dp' <- (^. isoTextPath) <$> toSysPath dp
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

abortR :: EffError r => Text -> (Req' a) -> Sem r b
abortR msg r =
  throw @Text (msg <> ": req = " <> toUrlPath r)

-- --------------------
--
-- navigation ops

toParent :: (Eff'ISE r, EffNonDet r) => Req'IdNode a -> Sem r (Req'IdNode a)
toParent r = do
  -- lift $ trc "toParent'"
  v <- toParent' r
  -- lift $ trc ("toParent: " <> show (toReq'IdNode v))
  return v

toParent' :: (Eff'ISE r, EffNonDet r) => Req'IdNode a -> Sem r (Req'IdNode a)
toParent' r = do
  r' <- denormPathPos r            -- r' has always a pos
  return ( r' & rPos .~ Nothing )  -- forget the pos
                                   -- the result is normalized

toPos' :: (Eff'ISE r, EffNonDet r)
       => (Int -> Int) -> Req'IdNode a -> Sem r (Req'IdNode a)
toPos' f r = do
  -- lift $ trc "toPos'"
  v <- toPos'' f r
  -- lift $ trc ("toPos': " <> show (toReq'IdNode v))
  return v

toPos'' :: (Eff'ISE r, EffNonDet r)
        => (Int -> Int) -> Req'IdNode a -> Sem r (Req'IdNode a)
toPos'' f r = do
  p <- denormPathPos r
  x <- pureMaybe (p ^. rPos)
  let x' = f x
  _ <- pureMaybe (p ^? rColNode . theColEntries . ix x')
  normPathPos (p & rPos .~ Just x')

toPrev :: (Eff'ISE r, EffNonDet r) => Req'IdNode a -> Sem r (Req'IdNode a)
toPrev = toPos' pred

toNext :: (Eff'ISE r, EffNonDet r) => Req'IdNode a -> Sem r (Req'IdNode a)
toNext = toPos' succ
{-
toFirst :: (Eff'ISE r, EffNonDet r) => Req'IdNode a -> Sem r (Req'IdNode a)
toFirst = toPos' (const 0)
-}
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
toFirstChild r = do
  -- lift $ trc "toFirstChild"
  v <- toFirstChild' r
  -- lift $ trc ("toFirstChild: " <> show (toReq'IdNode v))
  return v

toFirstChild' :: (Eff'ISE r, EffNonDet r) => Req'IdNode a -> Sem r (Req'IdNode a)
toFirstChild' r
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

data PrevNextPar a =
  PrevNextPar { _prev :: a
              , _next :: a
              , _par  :: a
              , _fwrd :: a
              }
  deriving (Functor, Foldable, Traversable, Show)

toPrevNextPar :: (Eff'ISE r)
              => Req'IdNode a -> Sem r (PrevNextPar (Maybe (Req'IdNode a)))
toPrevNextPar r =
  PrevNextPar
  <$> runMaybe (toPrev            r)
  <*> runMaybe (toNext            r)
  <*> runMaybe (toParent          r)
  <*> runMaybe (toChildOrNextOrUp r)

-- ----------------------------------------

thePageCnfs :: [(Geo, (Geo, Geo, Int))]
thePageCnfs =
  [ (Geo 2560 1440, (Geo  160  120, Geo  1600 160, 14))
  , (Geo 1920 1200, (Geo  160  120, Geo  1600 160, 11))
  , (Geo 1600 1200, (Geo  160  120, Geo  1500 160,  9))
  , (Geo 1400 1050, (Geo  140  105, Geo  1200 120,  9))
  , (Geo 1280  800, (Geo  120   90, Geo  1200 120,  9))
  ]

lookupPageCnfs :: ReqType -> Geo -> (Geo, Geo, Int)
lookupPageCnfs ty geo
  = fromMaybe (Geo  160  120, Geo 160 120,  9)
    (pty ty <$> lookup geo thePageCnfs)
  where
    pty RPage1 (geo1,  geo2, _ncol) = (geo1, geo2,    0)
    pty _RPage (geo1, _geo2,  ncol) = (geo1, geo1, ncol)

-- ----------------------------------------
-- image attributes

data ImgAttr =
  ImgAttr { _imgMediaUrl :: Text -- TextPath
          , _imgMeta     :: MetaData
          , _imgTitle    :: Text
          , _imgSubTitle :: Text
          , _imgComment  :: Text
          , _imgDuration :: Text
          }
  deriving Show

collectImgAttr :: Eff'ISE r => Req'IdNode'ImgRef a -> Sem r ImgAttr
collectImgAttr r = do
  theMeta <- getMetaData iOid
  theUrl  <- toUrlPath' (toMediaReq r)  -- !!! not toUrlPath due to RMovie
  return $
    ImgAttr
    { _imgMediaUrl = theUrl ^. isoText
    , _imgMeta     = theMeta
    , _imgTitle    = take1st
                     [ theMeta ^. metaDataAt descrTitle
                     , nm ^. isoText
                     ]
    , _imgSubTitle = theMeta ^. metaDataAt descrSubtitle
    , _imgComment  = theMeta ^. metaDataAt descrComment
    , _imgDuration = take1st
                     [ theMeta ^. metaDataAt descrDuration
                     , "1.0"
                     ]
    }
  where
    ImgRef iOid nm = r ^. rImgRef

-- --------------------

thePos :: Eff'ISE r => Req'IdNode a -> Sem r Text
thePos r =
  runMaybeEmpty $
  do r' <- denormPathPos r
     return (r' ^. rPos . traverse . isoPicNo . isoText)

-- ----------------------------------------
--
-- html page generation

genReqImgPage :: (Eff'Img r)
               => Req'IdNode'ImgRef a -> Sem r LazyByteString
genReqImgPage r =
  renderHtml <$> genReqImgPage' r

genReqImgPage' :: (Eff'Img r)
               => Req'IdNode'ImgRef a -> Sem r Blaze.Html
genReqImgPage' r = do
  now' <- nowAsIso8601

  ImgAttr this'mediaUrl
          this'meta
          this'title
          this'subTitle
          this'comment
          this'duration <- collectImgAttr r
  this'pos              <- thePos r

  let     this'url       = toUrlPath r ^. isoText  -- !!! no toUrlPath' due to RPage
  let     this'geo       = r ^. rGeo
  let     base'ref       = "/"  -- will be changed when working with relative urls

  nav  <- toPrevNextPar r

  let     m2url          = maybe mempty ((^. isoText) . toUrlPath)

  -- the urls of the siblings
  let PrevNextPar
          prev'url
          next'url
          par'url
          fwrd'url       = m2url <$> nav

  let tomu mr            = (^. isoText)
                           <$> runMaybeEmpty (pureMaybe mr >>= toMediaUrl)

  -- the image urls of the siblings
  PrevNextPar
          prev'imgRef
          next'imgRef
          _par'imgRef
          fwrd'imgRef   <- traverse tomu nav

  let star               = '\9733'
  let rating             = (\ x -> replicate x star ^. isoText) $
                           getRating this'meta
  let metaData           = this'meta -- add jpg filename and rating
                           & metaDataAt fileRefJpg .~ (this'mediaUrl ^. isoText)
                           & metaDataAt imgRating  .~ rating

  case toMediaReq r ^. rType of

    --image page
    RImg -> do
      org'imgpath       <- toSourcePath r
      org'geo           <- getImageSize org'imgpath

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

      return $
        picPage'
        base'ref
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
        metaData

    -- mp4 video
    RMovie -> do
      org'imgpath       <- toSourcePath r
      org'geo           <- getImageSize org'imgpath

      return $
        movPage'
        base'ref
        this'title
        now'
        this'title
        this'subTitle
        this'comment
        this'geo
        (resizeGeo' org'geo this'geo)
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
        metaData

    -- blog page
    RPage -> do
      blogContents <- toSourcePath r >>= genBlogHtml

      return $
        txtPage'
        base'ref
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

    _ -> return mempty


-- ----------------------------------------
--
-- icons in a collection page
-- the icons are generated with a fixed aspect ratio (RIcon, not RIconp)

type IconDescr3 = (Text, Text, Text)

toIconDescr :: (Eff'ISE r) => Geo -> Req'IdNode a -> Sem r IconDescr3
toIconDescr icon'geo r = do
  let r'url         = toUrlPath  r ^. isoText
  let r'iconurl     = toUrlPath (r & rType %~ iType
                                   & rGeo  .~ icon'geo
                                )  ^. isoText
  r'meta           <- runMaybeEmpty (toImgMeta r)
  let r'title       = r'meta ^. metaDataAt descrTitle

  return (r'url, r'iconurl, r'title)
  where
    iType RPage1 = RImg
    iType _RPage = RIcon

emptyIconDescr :: IconDescr3
emptyIconDescr = (mempty, mempty, mempty)

-- --------------------

genReqColPage  :: (Eff'Img r)
               => Req'IdNode a -> Sem r LazyByteString
genReqColPage r =
  renderHtml <$> genReqColPage' r

genReqColPage' :: (Eff'Img r)
               => Req'IdNode a -> Sem r Blaze.Html
genReqColPage' r = do
  now'  <- nowAsIso8601

  let   this'ty          = r ^. rType
  let   this'geo         = r ^. rGeo
  let ( inav'geo,
        icon'geo,
        icon'no )        = lookupPageCnfs this'ty this'geo

  let   this'meta        = r ^. rColNode . theMetaData
  (     this'url,
        this'iconurl,
        this'title )    <- toIconDescr inav'geo r

  let   base'ref         = "/"  -- will be changed when working with relative urls

  this'pos              <- thePos r

  -- the icons descr of the siblings
  nav   <- toPrevNextPar r
  PrevNextPar
        (prev'url, prev'iconurl, prev'title)
        (next'url, next'iconurl, next'title)
        ( par'url,  par'iconurl,  par'title)
        (fwrd'url, fwrd'iconurl, _wrd'title)
                        <- traverse
                           (\ r' -> fromMaybe emptyIconDescr <$>
                                    traverse (toIconDescr inav'geo ) r'
                           )
                           nav

  -- the icon descr of the children
  cs                    <- runMaybeEmpty (toChildren r)
  cs'descr              <- traverse (toIconDescr icon'geo) cs
  ( c1'url,
    c1'iconurl,
    c1'title )          <- fromMaybe emptyIconDescr
                           <$> traverse (toIconDescr inav'geo) (listToMaybe cs)

  let   cs'descr4        = zipWith
                           ( \ (x1, x2, x3) i ->
                               ( x1
                               , x2
                               , if T.null x3
                                 then (show i <> ". Bild") ^. isoText
                                 else x3
                               , i ^. isoPicNo . isoText
                               )
                           )
                           cs'descr
                           [1..]

  this'blogContents     <- runMaybeEmpty
                           ( do r' <- setColBlogRef r
                                p' <- toSourcePath r'
                                genBlogHtml p'
                           )

  return $
    colPage'
    base'ref
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
