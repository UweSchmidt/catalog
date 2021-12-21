{-# LANGUAGE
    ConstraintKinds,
    DataKinds,
    FlexibleContexts,
    GADTs,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- {-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------

module Catalog.Logging
  ( trc'Obj
  , warn'Obj
  , verb'Obj
  )
where

-- import Control.Monad.Trans.Except (Except, runExcept)

import Catalog.Effects
import Catalog.ImgTree.Access (objid2path)

import Data.Prim

import qualified Data.Text as T

------------------------------------------------------------------------------
--
-- logging and tracing convenient functions

log'Obj :: EffIStore r => (Text -> Sem r ()) -> ObjId -> Text -> Sem r ()
log'Obj logCmd i msg = do
  p <- objid2path i
  logCmd $ T.unwords [msg, "(" <> i ^.isoText, ", " <> p ^. isoText <> ")"]

trc'Obj
  , warn'Obj
  , verb'Obj :: (EffIStore r, EffLogging r)
             => ObjId -> Text -> Sem r ()
trc'Obj  = log'Obj log'trc
warn'Obj = log'Obj log'warn
verb'Obj = log'Obj log'verb

-- ----------------------------------------
