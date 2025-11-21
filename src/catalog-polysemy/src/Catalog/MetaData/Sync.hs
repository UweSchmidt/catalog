------------------------------------------------------------------------------

module Catalog.MetaData.Sync
  ( Eff'MDSync
  , syncTheMetaData
  , syncAllMetaData
  , syncMetaData
  )
where

import Catalog.CatEnv
       ( CatEnv
       , catForceMDU
       )
import Catalog.Effects
       ( Sem
       , EffExecProg
       , EffTime
       , EffLogging
       , EffJournal
       , EffIStore
       , EffFileSys
       , EffError
       , EffCatEnv
       , log'trc
       , local
       , ask
       )
import Catalog.ImgTree.Fold
       ( foldMT )

import Catalog.ImgTree.Access
       ( objid2path
       , getImgVals
       )
import Catalog.MetaData.Exif
       ( setMD )

import Data.ImgNode
       ( ImgParts
       , ImgRef'(ImgRef, _iref)
       , theImgTimeStamp
       , theParts
       , colEntryM'
       , theMetaData
       , traverseParts
       )
import Catalog.Logging
       ( trc'Obj )

import Data.MetaData
       ( MetaData
       , theImgEXIFUpdate
       )
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

syncTheMetaData :: Eff'MDSync r
                => Bool -> Bool -> ObjId -> Sem r ()
syncTheMetaData recursive force i
  | force     = local @CatEnv (catForceMDU .~ True) $
                syncAllMetaData recursive i
  | otherwise = syncAllMetaData recursive i

syncAllMetaData :: Eff'MDSync r
                => Bool -> ObjId -> Sem r ()
syncAllMetaData recursive i0 = do
  p <- objid2path i0
  log'trc $ "syncAllMetaData for: " <> toText (i0, p)

  foldMT imgA dirA rootA colA i0
  where
    imgA = syncMetaData'

    -- traverse the DIR
    dirA go _i es _ts
      | recursive = traverse_ go           es
      | otherwise = traverse_ syncMetaData es

    -- we only need to traverse the DIR hierachy
    rootA go _i dir _col = go dir

    -- for a COL the col img and the entries must be traversed
    colA go _i _md im _be es = do
      traverse_ (go . (\(ImgRef i' _name) -> i')) im
      traverse_ go' es
        where
          go' = colEntryM'
                (go . _iref)
                ( when recursive . go)

-- i must be an objid pointing to an ImgNode
-- else this becomes a noop

syncMetaData :: Eff'MDSync r => ObjId -> Sem r ()
syncMetaData i = do
  ps <- getImgVals i theParts
  md <- getImgVals i theMetaData
  unless (isEmpty ps) $
    syncMetaData' i ps md


syncMetaData' :: Eff'MDSync r => ObjId -> ImgParts -> MetaData -> Sem r ()
syncMetaData' i ps md'old = do
  trc'Obj i $ "syncMetaData:\n" <> prettyJSONText [] ps
  trc'Obj i $ "syncMetaData:\n" <> prettyJSONText [] md'old

  forceUpdate <- (^. catForceMDU) <$> ask
  let ts      =  md'old ^. theImgEXIFUpdate
  let ts'ps   =  ps     ^. traverseParts . theImgTimeStamp
  let update = forceUpdate || ts < ts'ps

  log'trc $ "syncMetadata: " <> prettyJSONText [] (ts, ts'ps, update)

  -- collect meta data from raw and img parts
  when update $
    setMD i ps md'old

------------------------------------------------------------------------
