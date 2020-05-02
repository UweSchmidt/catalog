{-# LANGUAGE
    DataKinds,
    FlexibleContexts,
    GADTs,
    OverloadedStrings,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TemplateHaskell,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Polysemy.EmbedExc
  ( -- * Actions
    embedExc

  -- aux types andfunctions
  , IOException
  , ioExcToText
  )
where

import Polysemy
import Polysemy.Error

import Control.Exception
       ( IOException )

import Data.Text
       ( Text )

import qualified Control.Exception          as EX
import qualified Data.Text                  as T

------------------------------------------------------------------------------
--
-- | perform an IO cmd, catch all IOException
-- and map them to Error exc

embedExc :: forall exc r a
          . ( Member (Embed IO) r
            , Member (Error exc) r
            )
         => (IOException -> exc)
         -> IO a
         -> Sem r a
embedExc ef iocmd = do
  r <- embed $ EX.try iocmd
  case r of
    Left  e -> throw @exc (ef e)
    Right a -> pure a

{-# INLINE embedExc #-}

ioExcToText :: IOException -> Text
ioExcToText = T.pack . show

{-# INLINE ioExcToText  #-}

------------------------------------------------------------------------------
