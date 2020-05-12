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

module Catalog.Journal
where

-- import Control.Monad.Trans.Except (Except, runExcept)

import Catalog.Effects
import Catalog.ImgTreeAccess (objid2path)

import Data.Journal
import Data.Prim

import qualified Data.Text.IO  as T

------------------------------------------------------------------------------
--
-- journaling

journal :: Journal -> SemISJ r ()
journal jc = do
  jcp <- traverse objid2path jc
  consume @JournalP jcp

journalToStdout :: Member (Embed IO) r
                => InterpreterFor (Consume JournalP) r
journalToStdout = consumeIO $ T.putStrLn . ("journal: " <>) . toText

journalToDevNull :: InterpreterFor (Consume JournalP) r
journalToDevNull = consumeNull

-- ----------------------------------------
