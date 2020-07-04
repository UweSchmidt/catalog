{-# LANGUAGE
    ConstraintKinds,
    DataKinds,
    FlexibleContexts,
    GADTs,
    OverloadedStrings,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)

{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------

module Catalog.History
  ( UndoListCmd(..)
  , addToUndoList
  , getFromUndoList
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
import Polysemy.State

import Data.History    ( History
                       , HistoryID
                       , addToHistory
                       , resetHistory
                       , dropHistory
                       , emptyHistory
                       )
import Data.ImageStore (ImgStore)

------------------------------------------------------------------------------

type UndoHistory  = History ImgStore
type HistoryState = State UndoHistory

-- ----------------------------------------

data UndoListCmd m a where
  AddToUndoList    :: ImgStore -> UndoListCmd m HistoryID
  GetFromUndoList  :: HistoryID -> UndoListCmd m (Maybe ImgStore)
  DropFromUndoList :: HistoryID -> UndoListCmd m ()

makeSem ''UndoListCmd

-- ----------------------------------------

undoListWithState :: Sem (UndoListCmd ': r) a
                  -> Sem (State UndoHistory ': r) a
undoListWithState =
  reinterpret $
  \ c -> case c of
    AddToUndoList s -> do
      h <- get @UndoHistory
      let (hid, h') = addToHistory s h
      put @UndoHistory h'
      return hid

    GetFromUndoList hid -> do
      h <- get @UndoHistory
      let (ms, h') = resetHistory hid h
      put @UndoHistory h'
      return ms

    DropFromUndoList hid -> do
      modify @UndoHistory (dropHistory hid)

{-# INLINE undoListWithState #-}

undoListNoop :: Sem (UndoListCmd ': r) a
             -> Sem r a
undoListNoop =
  interpret $
  \ c -> case c of
    AddToUndoList _s -> do
      return 0

    GetFromUndoList _hid -> do
      return Nothing

    DropFromUndoList _hid -> do
      return ()

{-# INLINE undoListNoop #-}

-- ----------------------------------------
