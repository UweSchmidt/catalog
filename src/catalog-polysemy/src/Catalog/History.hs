{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

------------------------------------------------------------------------------

module Catalog.History
  ( UndoListCmd(..)
  , addToUndoList
  , getFromUndoList
  , getWholeUndoList
  , dropFromUndoList

  , undoListWithState
  , undoListNoop

  , HistoryState
  , UndoHistory

  -- reexports
  , HistoryID
  , emptyHistory
  )
where

import Polysemy
       ( Sem
       , makeSem
       , reinterpret
       , interpret
       )
import Polysemy.State
       ( State
       , put
       , modify
       , get
       )

import Data.History
       ( History
       , HistoryID
       , addToHistory
       , resetHistory
       , dropHistory
       , emptyHistory
       , entriesInHistory
       )
import Data.ImageStore
       (ImgStore)

import Data.Prim.Prelude
       ( Text )

------------------------------------------------------------------------------

type UndoHistory  = History (Text, ImgStore)
type HistoryState = State UndoHistory

-- ----------------------------------------

data UndoListCmd m a where
  AddToUndoList    :: Text -> ImgStore -> UndoListCmd m HistoryID
  GetFromUndoList  :: HistoryID        -> UndoListCmd m (Maybe ImgStore)
  GetWholeUndoList ::                     UndoListCmd m [(HistoryID, Text)]
  DropFromUndoList :: HistoryID        -> UndoListCmd m ()

makeSem ''UndoListCmd

-- ----------------------------------------

undoListWithState :: Sem (UndoListCmd ': r) a
                  -> Sem (State UndoHistory ': r) a
undoListWithState =
  reinterpret $
  \ case
    AddToUndoList cmt s -> do
      h <- get @UndoHistory
      let (hid, h') = addToHistory (cmt, s) h
      put @UndoHistory h'
      return hid

    GetFromUndoList hid -> do
      h <- get @UndoHistory
      let (ms, h') = resetHistory hid h
      put @UndoHistory h'
      return (snd <$> ms)

    GetWholeUndoList -> do
      h <- get @UndoHistory
      let es = map (\ (i, (t, _s)) -> (i, t)) (entriesInHistory h)
      return es

    DropFromUndoList hid -> do
      modify @UndoHistory (dropHistory hid)

{-# INLINE undoListWithState #-}

undoListNoop :: Sem (UndoListCmd ': r) a
             -> Sem r a
undoListNoop =
  interpret $
  \ case
    AddToUndoList _cmt _s -> do
      return 0

    GetFromUndoList _hid -> do
      return Nothing

    GetWholeUndoList -> do
      return []

    DropFromUndoList _hid -> do
      return ()

{-# INLINE undoListNoop #-}

-- ----------------------------------------
