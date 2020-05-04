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

module Client.Commands
  ( -- * Effects
    CCommand (..)

    -- * Actions
  , ccEntry
  , ccLs
  , ccLsmd
  , ccSetmd1
  , ccDelmd1
  , ccDownload
  , ccSnapshot
  , ccCheckSum
  , ccUpdCSum

    -- * reexported catalog types
  , Geo
  , Name
  , Path
  , PathPos
  , ReqType (..)
  , Text
  )
where

import Polysemy

import Data.Prim
       ( Geo
       , Name
       , Path
       , PathPos
       , ReqType (..)
       , Text
       )

------------------------------------------------------------------------------

data CCommand m a where
  CcEntry    :: Path                       -> CCommand m ()
  CcLs       :: Path                       -> CCommand m ()
  CcLsmd     :: PathPos -> [Name]          -> CCommand m ()
  CcSetmd1   :: PathPos ->  Name   -> Text -> CCommand m ()
  CcDelmd1   :: PathPos ->  Name           -> CCommand m ()
  CcDownload :: Path    -> ReqType -> Geo
             -> Text    -> Bool    -> Bool -> CCommand m ()
  CcSnapshot :: Text                       -> CCommand m ()
  CcCheckSum :: Path    -> Name
             -> Bool    -> Bool            -> CCommand m ()
  CcUpdCSum  :: Path    -> Name
             -> Bool    -> Bool            -> CCommand m ()

makeSem ''CCommand

------------------------------------------------------------------------------
