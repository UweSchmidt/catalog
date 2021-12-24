------------------------------------------------------------------------------

module Polysemy.EmbedExc
  ( -- * Actions
    embedExc
  , embedExcText

  -- aux types andfunctions
  , IOException
  , ioExcToText
  )
where

import Polysemy
       ( Member
       , Sem
       , Embed
       , embed
       )
import Polysemy.Error
       ( Error
       , throw
       )

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

embedExcText :: forall r a
              . ( Member (Embed IO) r
                , Member (Error Text) r
                )
             => IO a
             -> Sem r a
embedExcText = embedExc ioExcToText

------------------------------------------------------------------------------
