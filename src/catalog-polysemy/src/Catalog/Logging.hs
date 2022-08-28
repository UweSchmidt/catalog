------------------------------------------------------------------------------

module Catalog.Logging
  ( trc'Obj
  , warn'Obj
  , verb'Obj
  )
where

import Catalog.Effects
       ( Sem
       , EffLogging
       , EffIStore
       , log'warn
       , log'verb
       , log'trc
       )
import Catalog.ImgTree.Access
       ( objid2path )

import Data.Prim
       ( Text
       , ObjId
       , IsoText(isoText)
       , (^.)
       )

import qualified Data.Text as T

------------------------------------------------------------------------------
--
-- logging and tracing convenient functions

log'Obj :: EffIStore r => (Text -> Sem r ()) -> ObjId -> Text -> Sem r ()
log'Obj logCmd i msg = do
  p <- objid2path i
  logCmd $ T.unwords
           [ msg
           , "(" <> show i ^. isoText <> ", " <> p ^. isoText <> ")"
           ]

trc'Obj
  , warn'Obj
  , verb'Obj :: (EffIStore r, EffLogging r) => ObjId -> Text -> Sem r ()
trc'Obj  = log'Obj log'trc
warn'Obj = log'Obj log'warn
verb'Obj = log'Obj log'verb

-- ----------------------------------------
