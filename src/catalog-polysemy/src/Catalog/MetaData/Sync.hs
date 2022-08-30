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

import Catalog.ImgTree.Access
import Catalog.MetaData.Exif
       ( setMD )


import Catalog.Logging
       ( trc'Obj )

import Data.ImgTree
import Data.MetaData
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

syncTheMetaData :: Eff'MDSync r => Bool -> Bool -> ObjId -> Sem r ()
syncTheMetaData recursive force i
  | force     = local @CatEnv (catForceMDU .~ True) $
                syncAllMetaData recursive i
  | otherwise = syncAllMetaData recursive i


syncAllMetaData :: Eff'MDSync r => Bool -> ObjId -> Sem r ()
syncAllMetaData recursive = go
  where
    go i = withTF $ \ t -> do
      log'trc $ "syncAllMetaData for: " <> toText (i, refPath i t)

      case (t, i) ^. theNode of
        n | isIMG n ->
              syncMetaData' i (n ^. theParts) (n ^. theMetaData)

          | isDIR n ->
              traverseOf_ (theDirEntries . traverse)
                          ( if recursive
                            then go
                            else syncMetaData
                          ) n

          | isROOT n ->
              go (n ^. theRootImgDir)

          | isCOL n -> do
              traverseOf_ (theColImg     . traverse . imgref) go  n
              traverseOf_ (theColBlog    . traverse . imgref) go  n
              traverseOf_ (theColEntries . traverse         ) go' n

          | otherwise ->
              return ()

          where
            go' = colEntry'
                  (go . (^. imgref))
                  (when recursive . go)

-- i must be an objid pointing to am ImgNode
-- else this becomes a noop

syncMetaData :: Eff'MDSync r => ObjId -> Sem r ()
syncMetaData i = withTF go
  where
    go t = unless (isempty ps) $
           syncMetaData' i ps md
      where
        n  = (t, i) ^. theNode
        ps = n ^. theParts
        md = n ^. theMetaData

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
