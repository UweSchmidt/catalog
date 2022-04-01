------------------------------------------------------------------------------

module Catalog.Journal
where

import Catalog.Effects
       ( Consume
       , Embed
       , InterpreterFor
       , Member
       , Sem
       , Eff'ISJ
       , consumeNull
       , consumeIO
       , consume
       )
import Catalog.ImgTree.Access
       ( objid2path )

import Data.Journal
       ( JournalP
       , Journal
       )
import Data.Prim
       ( (#)
       , IsoString(isoString)
       , LazyByteString
       )

import Polysemy.Consume.BGQueue
       ( BGQueue
       , writeToBGQueue
       )

import System.IO
       ( Handle
       , hFlush
       , stdout
       , stderr
       )

import qualified Data.Aeson.Encode.Pretty as J
import qualified Data.ByteString.Lazy     as LB

------------------------------------------------------------------------------
--
-- journaling

journal :: Eff'ISJ r => Journal -> Sem r ()
journal jc = do
  jcp <- traverse objid2path jc
  consume @JournalP jcp

-- journal interpreters

-- --------------------
-- journal to background queue for output syncronised with
-- other streams, e.g. logging

journalToBGQueue :: Member (Embed IO) r
                 => BGQueue -> Handle -> InterpreterFor (Consume JournalP) r
journalToBGQueue q h = writeToBGQueue q (outJournal h)

-- --------------------
-- journal to file handle

journalToHandle :: Member (Embed IO) r
                => Handle -> InterpreterFor (Consume JournalP) r
journalToHandle h = consumeIO $ outJournal h

-- --------------------
-- journal to stdout

journalToStdout :: Member (Embed IO) r
                => InterpreterFor (Consume JournalP) r
journalToStdout = journalToHandle stdout

-- --------------------
-- journal to stderr

journalToStderr :: Member (Embed IO) r
                => InterpreterFor (Consume JournalP) r
journalToStderr = journalToHandle stderr

-- --------------------
-- throw away journal

journalToDevNull :: InterpreterFor (Consume JournalP) r
journalToDevNull = consumeNull

-- --------------------
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
