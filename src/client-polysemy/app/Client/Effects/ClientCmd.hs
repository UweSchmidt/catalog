{-# LANGUAGE TemplateHaskell #-}

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
  , ccUndoList
  , ccKeywordCols
  , ccApplyUndo
  , ccDropUndo
  , ccExifUpdate
  , ccCheckMeta
  , ccGeoAddress
  , ccGetAddress
  , ccPage
  , ccSyncKeyword
  , ccNewKeywords
  , ccTest
  )
where

import Polysemy
       ( makeSem )

import Data.History
       ( HistoryID )

import Data.MetaData
       ( MetaKey )

import Data.Prim
       ( Geo
       , GPSposDec
       , Name
       , Path
       , PathPos
       , ReqType (..)
       , Text
       )

------------------------------------------------------------------------------

data ClientCmd m a where
  CcGlob        :: Path                        -> ClientCmd m ()
  CcEntry       :: Path                        -> ClientCmd m ()
  CcLsSub       :: Path                        -> ClientCmd m ()
  CcLsmd        :: PathPos -> [MetaKey]        -> ClientCmd m ()
  CcSetmd1      :: PathPos ->  MetaKey -> Text -> ClientCmd m ()
  CcDelmd1      :: PathPos ->  MetaKey         -> ClientCmd m ()
  CcSetColImg   :: PathPos -> Path             -> ClientCmd m ()
  CcSetColBlog  :: PathPos -> Path             -> ClientCmd m ()
  CcDownload    :: Path    -> ReqType  -> Geo
                -> Text    -> Bool     -> Bool -> ClientCmd m ()
  CcSnapshot    :: Text                        -> ClientCmd m ()
  CcCheckSum    :: Path    -> Name
                -> Bool    -> Bool             -> ClientCmd m ()
  CcUpdCSum     :: Path    -> Name
                -> Bool    -> Bool             -> ClientCmd m ()
  CcKeywordCols ::                      [Text] -> ClientCmd m ()
  CcUndoList    ::                                ClientCmd m ()
  CcApplyUndo   ::                   HistoryID -> ClientCmd m ()
  CcDropUndo    ::                   HistoryID -> ClientCmd m ()
  CcExifUpdate  :: Path    -> Bool     -> Bool -> ClientCmd m ()
  CcCheckMeta   :: Path                        -> ClientCmd m ()
  CcGeoAddress  :: Path    -> Bool             -> ClientCmd m ()
  CcGetAddress  :: GPSposDec                   -> ClientCmd m ()
  CcPage        :: Path                        -> ClientCmd m ()
  CcSyncKeyword :: Path                        -> ClientCmd m ()
  CcNewKeywords ::                                ClientCmd m ()
  CcTest        :: Path                        -> ClientCmd m ()

makeSem ''ClientCmd

------------------------------------------------------------------------------
