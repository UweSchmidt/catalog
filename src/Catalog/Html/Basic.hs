{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Catalog.Html.Basic
  ( isPano
  , colImgRef
  , getColBlogSource
  , putColBlogSource
  , getColBlogCont
  )
where

import Data.ImgTree
import Data.MetaData
import Data.Prim

import Catalog.Cmd
import Catalog.FilePath       ( addJpg, baseNameMb, ymdNameMb )
import Data.Journal           ( Journal'(SaveBlogText) )
import Catalog.System.Convert ( genAssetIcon
                              , genBlogText
                              , genBlogHtml
                              , writeBlogText
                              )

-- ----------------------------------------
--
-- process a ColRef, an ObjId with optionally a position

colImgOp :: Monoid a =>
            (ObjId -> Name -> Cmd a) ->
            (ObjId ->         Cmd a) ->
            ColRef -> Cmd a
colImgOp iop cop = cColRef cop iref
  where
    iref i pos = do
      cs <- getImgVals i theColEntries
      case cs ^? ix pos of
        Just (ImgEnt (ImgRef j n)) -> iop j n
        _ -> return mempty

-- ----------------------------------------

colImgPath0 :: ColRef -> Cmd (Maybe FilePath)
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

colImgPath :: ColRef -> Cmd (Maybe FilePath)
colImgPath cr = do
  f <- colImgPath0 cr
  return (addJpg <$> f)

-- compute the image ref of a collection
-- if collection has a front page image, take that
-- otherwise take the ref to a generated image

colImgRef :: ObjId -> Cmd FilePath
colImgRef i = do
  p <- colImgPath (i, Nothing)
  maybe (iconRef i) return p

-- ----------------------------------------

-- compute the icon ref of a collection

iconRef :: ObjId -> Cmd FilePath
iconRef i = do
  t  <- (^. metaDataAt descrTitle . isoString ) <$> getMetaData i

  mbf <-
    if isempty t  -- no title there
    then do
      p <- (^. isoString) <$> objid2path i
      path2img p
    else
      genAssetIcon (t'hash t) t

  return $
    fromMaybe (ps'blank ^. isoString) mbf
  where
    t'hash t' =
      mkCheckSum t' ^. isoString . to toP
      where
        toP s = x ++ "/" ++ y
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

path2img :: FilePath -> Cmd (Maybe FilePath)
path2img f
  | Just (y, Nothing) <- ymd =
      genAssetIcon y y

  | Just (y, Just (m, Nothing)) <- ymd =
      let s = toN m ++ "." ++ y
      in
        genAssetIcon (y </> s) s

  | Just (y, Just (m, Just d)) <- ymd =
      let s = toN d ++ "." ++ toN m ++ "." ++ y
      in
        genAssetIcon (y </> s) s

  | Just n <- nm =
      genAssetIcon n n

  | otherwise =
      return Nothing
  where
    ymd = ymdNameMb  f
    nm  = baseNameMb f

    toN :: String -> String
    toN s = show i   -- remove leading 0's
      where
        i :: Int
        i = read s

-- ----------------------------------------

getColBlogCont   :: ImgRef -> Cmd Text
getColBlogCont   = processBlog genBlogHtml

getColBlogSource :: ImgRef -> Cmd Text
getColBlogSource = processBlog genBlogText

putColBlogSource :: Text -> ImgRef -> Cmd ()
putColBlogSource t ir@(ImgRef i n) =
  processBlog putBlog ir
  where
    putBlog f = do
      writeBlogText t f
      journalChange $ SaveBlogText i n t

processBlog :: (FilePath -> Cmd a)
            -> ImgRef
            -> Cmd a
processBlog process (ImgRef i n) = do
  p <- objid2path i
  process $ (tailPath $ substPathName n p) ^. isoString

-- ----------------------------------------
