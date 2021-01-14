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
  ( Eff'MDSync
  , forceSyncAllMetaData
  , syncAllMetaData
  , syncMetaData
  )
where

import Catalog.CatEnv
import Catalog.Effects
import Catalog.ImgTree.Fold
import Catalog.ImgTree.Access
import Catalog.MetaData.Exif   ( setMD )

import Data.ImgNode
import Catalog.Logging         ( trc'Obj )
import Data.MetaData           ( MetaData
                               , theImgEXIFUpdate )
import Data.Prim

-- ----------------------------------------

type Eff'MDSync r = ( EffIStore   r
                    , EffError    r
                    , EffJournal  r
                    , EffLogging  r
                    , EffTime     r
                    , EffCatEnv   r
                    , EffExecProg r
                    , EffFileSys  r
                    )

forceSyncAllMetaData :: Eff'MDSync r => ObjId -> Sem r ()
forceSyncAllMetaData i =
  local @CatEnv (catForceMDU .~ True) (syncAllMetaData i)

syncAllMetaData :: Eff'MDSync r => ObjId -> Sem r ()
syncAllMetaData i0 = do
  p <- objid2path i0
  log'trc $ "syncAllMetaData for: " <> toText (i0, p)

  foldMT imgA dirA rootA colA i0
  where
    imgA = syncMetaData'

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

syncMetaData :: Eff'MDSync r => ObjId -> Sem r ()
syncMetaData i = do
  ps <- getImgVals i theParts
  md <- getImgVals i theMetaData
  unless (isempty ps) $
    syncMetaData' i ps md


syncMetaData' :: Eff'MDSync r => ObjId -> ImgParts -> MetaData -> Sem r ()
syncMetaData' i ps md'old = do
  trc'Obj i $ "syncMetaData': " <> toText ps
  -- md   <-  getMetaData i
  trc'Obj i $ "syncMetaData': " <> toText md'old

  forceUpdate <- (^. catForceMDU) <$> ask
  let ts      =  md'old ^. theImgEXIFUpdate
  let ts'ps   =  ps     ^. traverseParts . theImgTimeStamp
  let update = forceUpdate || ts < ts'ps

  log'trc $ "syncMetadata: " <> toText (ts, ts'ps, update)

  -- collect meta data from raw and xmp parts
  when update $
    setMD i ps md'old

------------------------------------------------------------------------
