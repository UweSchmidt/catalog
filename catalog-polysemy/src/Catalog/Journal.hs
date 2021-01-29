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
import Catalog.ImgTree.Access (objid2path)

import Data.Journal
import Data.Prim

import System.IO (Handle, hFlush, stdout, stderr)

import qualified Data.Aeson.Encode.Pretty as J
import qualified Data.ByteString.Lazy     as LB

------------------------------------------------------------------------------
--
-- journaling

journal :: Journal -> SemISJ r ()
journal jc = do
  jcp <- traverse objid2path jc
  consume @JournalP jcp

-- journal interpreter

-- journal to file handle
journalToHandle :: Member (Embed IO) r
                => Handle -> InterpreterFor (Consume JournalP) r
journalToHandle h = consumeIO $ \ j -> outJournal h j

-- journal to stdout
journalToStdout :: Member (Embed IO) r
                => InterpreterFor (Consume JournalP) r
journalToStdout = journalToHandle stdout

-- journal to stderr
journalToStderr :: Member (Embed IO) r
                => InterpreterFor (Consume JournalP) r
journalToStderr = journalToHandle stderr

-- throw away journal
journalToDevNull :: InterpreterFor (Consume JournalP) r
journalToDevNull = consumeNull

-- journal format and output

outJournal :: Handle -> JournalP -> IO ()
outJournal h j = do
  LB.hPutStr h (isoString # "\n")
  LB.hPutStr h (encodeJ j)
  LB.hPutStr h (isoString # "\n")
  hFlush     h

encodeJ :: JournalP -> LazyByteString
encodeJ = J.encodePretty' conf
  where
    conf = J.defConfig
      { J.confIndent  = J.Spaces 2
      , J.confCompare = J.keyOrder ["cmd", "path", "name"]
                        <>
                        compare
      }

-- ----------------------------------------
