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

module Client.Effects.ClientCmd
  ( -- * Effects
    ClientCmd (..)

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

data ClientCmd m a where
  CcEntry    :: Path                       -> ClientCmd m ()
  CcLs       :: Path                       -> ClientCmd m ()
  CcLsmd     :: PathPos -> [Name]          -> ClientCmd m ()
  CcSetmd1   :: PathPos ->  Name   -> Text -> ClientCmd m ()
  CcDelmd1   :: PathPos ->  Name           -> ClientCmd m ()
  CcDownload :: Path    -> ReqType -> Geo
             -> Text    -> Bool    -> Bool -> ClientCmd m ()
  CcSnapshot :: Text                       -> ClientCmd m ()
  CcCheckSum :: Path    -> Name
             -> Bool    -> Bool            -> ClientCmd m ()
  CcUpdCSum  :: Path    -> Name
             -> Bool    -> Bool            -> ClientCmd m ()

makeSem ''ClientCmd

------------------------------------------------------------------------------