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
  ( Eff'Html
  , isPano
  , getColBlogSource
  , putColBlogSource
  , getColBlogCont
  )
where

-- catalog-polysemy
import Catalog.Effects
import Catalog.ImgTree.Access
import Catalog.Journal        (journal)
import Catalog.GenImages      (genBlogText
                              ,genBlogHtml
                              ,writeBlogText
                              )

-- catalog
import Data.ImgTree
import Data.Journal           (Journal'(SaveBlogText))
import Data.Prim

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
    putBlog path = do
      writeBlogText t path
      journal $ SaveBlogText i n t

processBlog :: EffIStore r
            => (Path -> Sem r a)
            -> ImgRef
            -> Sem r a
processBlog process (ImgRef i n) = do
  p <- objid2path i
  process $ (tailPath . substPathName n $ p)

-- ----------------------------------------
