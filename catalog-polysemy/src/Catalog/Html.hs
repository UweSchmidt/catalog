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

------------------------------------------------------------------------------

module Catalog.Html
  ( isPano
  , colImgRef
  , getColBlogSource
  , putColBlogSource
  , getColBlogCont
  )
where

-- catalog-polysemy
import Catalog.Effects
import Catalog.ImgTree.Access
import Catalog.Journal        (journal)
import Catalog.TextPath       (buildImgPath0)
import Catalog.GenImages      (genAssetIcon
                              ,genBlogText
                              ,genBlogHtml
                              ,writeBlogText
                              )
import Catalog.Data.TextPath  ((<//>), addJpg, ymdNameMb, baseNameMb)

-- catalog
import Data.ImgTree
import Data.Journal           (Journal'(SaveBlogText))
import Data.MetaData          (metaDataAt, descrTitle)
import Data.Prim

-- libraries
import qualified Data.Text       as T

-- ----------------------------------------

type Eff'Html r = ( EffIStore   r   -- any effects missing?
                  , EffError    r
                  , EffJournal  r
                  , EffLogging  r
                  , EffCatEnv   r
                  , EffTime     r
                  , EffExecProg r
                  , EffFileSys  r
                  )

-- ----------------------------------------
--
-- process a ColRef, an ObjId with optionally a position

colImgOp :: (Eff'ISE r, Monoid a)
         => (ObjId -> Name -> Sem r a)
         -> (ObjId ->         Sem r a)
         -> ColRef -> Sem r a
colImgOp iop cop = cColRef cop iref
  where
    iref i pos = do
      cs <- getImgVals i theColEntries
      case cs ^? ix pos of
        Just (ImgEnt (ImgRef j n)) -> iop j n
        _ -> return mempty

-- ----------------------------------------

colImgPath0 :: Eff'ISE r => ColRef -> Sem r (Maybe TextPath)
colImgPath0 = colImgOp iop cop
  where
    cop i = do -- col ref
      n <- getImgVal i
      case n ^? theColImg . traverse of
        -- collection has a front page image
        Just ir ->
          Just <$> buildImgPath0 ir
        _ ->
          return Nothing

    iop j n = Just <$> buildImgPath0 (ImgRef j n)

colImgPath :: Eff'Html r => ColRef -> Sem r (Maybe TextPath)
colImgPath cr = do
  f <- colImgPath0 cr
  return (addJpg <$> f)

-- compute the image ref of a collection
-- if collection has a front page image, take that
-- otherwise take the ref to a generated image

colImgRef :: Eff'Html r
          => ObjId -> Sem r TextPath
colImgRef i = do
  p <- colImgPath (i, Nothing)
  maybe (iconRef i) return p

-- ----------------------------------------

-- compute the icon ref of a collection

iconRef :: ( EffIStore   r
           , EffError    r
           , EffLogging  r
           , EffCatEnv   r
           , EffExecProg r
           , EffFileSys  r
           )
        => ObjId -> Sem r TextPath
iconRef i = do
  t  <- (^. metaDataAt descrTitle) <$> getMetaData i

  mbf <-
    if isempty t  -- no title there
    then do
      p <- (^. isoText) <$> objid2path i
      path2img p
    else
      genAssetIcon (t'hash t) t

  return $
    fromMaybe (ps'blank ^. isoText) mbf
  where

    t'hash :: Text -> Text
    t'hash t' =
      t' & isoString %~ (^. to mkCheckSum . isoString . to toP)
      where
        toP s = x </> y
          where
            (x, y) = splitAt 2 s

-- ----------------------------------------
-- test: is picture a panorama

isPanoramaH :: Geo -> Bool
isPanoramaH (Geo w h) =
  w >= 2 * h    -- w / h >= 2.0

isPanoramaV :: Geo -> Bool
isPanoramaV (Geo w h) =
  h >= 2 * w    -- h / w >= 2.0

isPano :: Geo        -- ^ screen  geo
       -> Geo        -- ^ img     geo
       -> Maybe Geo  -- ^ resized geo
isPano (Geo w' h') img@(Geo w h)
  -- horizontal panorama: landscape
  | h >= h'
    &&
    isPanoramaH img = Just gh

  -- vertical panorama: trees
  | w >= w'
    &&
    isPanoramaV img = Just gv

  | otherwise       = Nothing
  where
    gh = Geo w2 h'
    w2 = (w * h' + (h - 1)) `div` h

    gv = Geo w' h2
    h2 = (h * w' + (w - 1)) `div` w

--  w2 = w * (h' / h)  -- with real arithmetic
--  h2 = h * (w' / w)

-- ----------------------------------------

path2img :: ( EffIStore   r
            , EffError    r
            , EffLogging  r
            , EffCatEnv   r
            , EffExecProg r
            , EffFileSys  r
            )
         => TextPath -> Sem r (Maybe TextPath)
path2img f
  | Just (y, Nothing) <- ymd =
      genAssetIcon y y

  | Just (y, Just (m, Nothing)) <- ymd =
      let s = toN m <> "." <> y
      in
        genAssetIcon (y <//> s) s

  | Just (y, Just (m, Just d)) <- ymd =
      let s = toN d <> "." <> toN m <> "." <> y
      in
        genAssetIcon (y <//> s) s

  | Just n <- nm =
      genAssetIcon n n

  | otherwise =
      return Nothing
  where
    ymd = ymdNameMb  f
    nm  = baseNameMb f

    toN :: Text -> Text
    toN = T.dropWhile (== '0')   -- remove leading 0's

-- ----------------------------------------

getColBlogCont   :: ( EffIStore   r
                    , EffError    r
                    , EffJournal  r
                    , EffLogging  r
                    , EffCatEnv   r
                    , EffExecProg r
                    , EffFileSys  r
                    )
                 => ImgRef -> Sem r Text
getColBlogCont   = processBlog genBlogHtml

getColBlogSource :: Eff'Html r => ImgRef -> Sem r Text
getColBlogSource = processBlog genBlogText

putColBlogSource :: ( EffIStore   r
                    , EffError    r
                    , EffJournal  r
                    , EffLogging  r
                    , EffCatEnv   r
                    , EffFileSys  r
                    )
                 => Text -> ImgRef -> Sem r ()
putColBlogSource t ir@(ImgRef i n) =
  processBlog putBlog ir
  where
    putBlog f = do
      writeBlogText t f
      journal $ SaveBlogText i n t

processBlog :: EffIStore r
            => (TextPath -> Sem r a)
            -> ImgRef
            -> Sem r a
processBlog process (ImgRef i n) = do
  p <- objid2path i
  process $ (tailPath $ substPathName n p) ^. isoText

-- ----------------------------------------
