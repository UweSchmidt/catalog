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

module Catalog.MetaData.Sync
  ( forceSyncAllMetaData
  , syncAllMetaData
  , syncMetaData
  )
where

import Catalog.CatEnv
import Catalog.Effects
import Catalog.ImgTree.Fold
import Catalog.ImgTree.Access
import Catalog.MetaData.Exif (setMD)

import Data.ImgNode
import Data.MetaData
import Data.Prim

-- ----------------------------------------

type Eff'Sync r = ( EffIStore   r   -- any effects missing?
                  , EffError    r
                  , EffJournal  r
                  , EffLogging  r
                  , EffTime     r
                  , EffCatEnv   r
                  , EffExecProg r
                  , EffFileSys  r
                  )

forceSyncAllMetaData :: Eff'Sync r => ObjId -> Sem r ()
forceSyncAllMetaData i =
  local @CatEnv (catForceMDU .~ True) (syncAllMetaData i)

syncAllMetaData :: Eff'Sync r => ObjId -> Sem r ()
syncAllMetaData i0 = do
  p <- objid2path i0
  log'trc $ "syncAllMetaData for: " <> toText (i0, p)

  foldMT imgA dirA rootA colA i0
  where
    imgA i ps _md = syncMetaData' i (ps ^. isoImgParts)

    -- traverse the DIR
    dirA go _i es _ts = traverse_ go (es ^. isoDirEntries)

    -- we only need to traverse the DIR hierachy
    rootA go _i dir _col = go dir

    -- for a COL the col img and the entries must be traversed
    colA go _i _md im _be es = do
      traverse_ (go . (\(ImgRef i' _name) -> i')) im
      traverse_ go' es
        where
          go' = colEntry' (go . _iref) go

-- i must be an objid pointing to am ImgNode
-- else this becomes a noop

syncMetaData :: Eff'Sync r => ObjId -> Sem r ()
syncMetaData i = do
  ps <- getImgVals i (theParts . isoImgParts)
  unless (null ps) $ do
    syncMetaData' i ps


syncMetaData' :: Eff'Sync r => ObjId -> [ImgPart] -> Sem r ()
syncMetaData' i ps = do
  ts <- getEXIFUpdateTime <$> getMetaData i
  fu <- (^. catForceMDU) <$> ask
  let update = fu || (ts < ps ^. traverse . theImgTimeStamp)

  -- trc $ "syncMetadata: " ++ show (ts, ps ^. traverse . theImgTimeStamp, update)

  -- collect meta data from raw and xmp parts
  when update $ do
    e0 <- setMD isRawMeta i ps  -- set metadata from raw files

    when e0 $ do                -- no raw file found
      e1 <- setMD isImg i ps    -- set meta from .tif, .png, ....

      when e1 $ do              -- no img file found (.tif, .png, ...)
        setMD isJpg i ps        -- set meta from .jpg
          >> return ()

------------------------------------------------------------------------
