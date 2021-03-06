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
  , ccGlob
  , ccEntry
  , ccLsSub
  , ccLsmd
  , ccSetmd1
  , ccDelmd1
  , ccSetColImg
  , ccSetColBlog
  , ccDownload
  , ccSnapshot
  , ccCheckSum
  , ccUpdCSum
  , ccMediaPath
  , ccUndoList
  , ccExifUpdate
  , ccCheckMeta

    -- * reexported catalog types
  , Geo
  , HistoryID
  , Name
  , Path
  , PathPos
  , ReqType (..)
  , Text
  )
where

import Polysemy

import Data.History
       ( HistoryID )

import Data.MetaData
       ( MetaKey )

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
  CcGlob       :: Path                        -> ClientCmd m ()
  CcEntry      :: Path                        -> ClientCmd m ()
  CcLsSub      :: Path                        -> ClientCmd m ()
  CcLsmd       :: PathPos -> [MetaKey]        -> ClientCmd m ()
  CcSetmd1     :: PathPos ->  MetaKey -> Text -> ClientCmd m ()
  CcDelmd1     :: PathPos ->  MetaKey         -> ClientCmd m ()
  CcSetColImg  :: PathPos -> Path             -> ClientCmd m ()
  CcSetColBlog :: PathPos -> Path             -> ClientCmd m ()
  CcDownload   :: Path    -> ReqType  -> Geo
               -> Text    -> Bool     -> Bool -> ClientCmd m ()
  CcSnapshot   :: Text                        -> ClientCmd m ()
  CcCheckSum   :: Path    -> Name
               -> Bool    -> Bool             -> ClientCmd m ()
  CcUpdCSum    :: Path    -> Name
               -> Bool    -> Bool             -> ClientCmd m ()
  CcMediaPath  :: Path                        -> ClientCmd m ()
  CcUndoList   ::                                ClientCmd m ()
  CcExifUpdate :: Path    -> Bool     -> Bool -> ClientCmd m ()
  CcCheckMeta  :: Path                        -> ClientCmd m ()

makeSem ''ClientCmd

------------------------------------------------------------------------------
