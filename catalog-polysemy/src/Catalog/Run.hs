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

module Catalog.Run
where

import Catalog.CatEnv
import Catalog.Effects
import Catalog.Journal (journalToStdout, journalToDevNull)

import Data.Prim
import Data.Journal    (JournalP)
import Data.ImageStore (ImgStore, emptyImgStore)

------------------------------------------------------------------------------

type CatApp a = Sem '[ FileSystem
                     , Time
                     , Reader CatEnv
                     , Consume JournalP
                     , Logging
                     , Consume LogMsg
                     , Error Text
                     , State ImgStore
                     , Embed IO
                     ] a

runApp :: AppEnv -> CatApp a -> IO a
runApp env cmd = do
  let runJournal
        | env ^. appEnvJournal = journalToStdout
        | otherwise            = journalToDevNull

  (_imgStore, Right res) <-
    runM
    . runState        @ImgStore emptyImgStore
    . runError        @Text
    . logToStdErr
    . logWithLevel    (env ^. appEnvLogLevel)
    . runJournal
    . runReader       @CatEnv (env ^. appEnvCat)
    . posixTime       ioExcToText
    . basicFileSystem ioExcToText
    $ cmd

  return res

------------------------------------------------------------------------------
