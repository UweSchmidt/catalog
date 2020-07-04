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

  , HistoryID
  , HistoryState
  , UndoHistory
  , emptyHistory
  )
where

import Polysemy
import Polysemy.State

import Data.Prim
import Data.ImageStore (ImgStore)

import qualified Data.IntMap as M

------------------------------------------------------------------------------
--

type HistoryID        = Int
type HistoryEntries a = M.IntMap a

data History a =
  HS { _histID      :: HistoryID
     , _histEntries :: HistoryEntries a
     }

emptyHistory :: History a
emptyHistory =
  HS { _histID      = 0
     , _histEntries = M.empty
     }

addToHistory :: a -> History a -> (HistoryID, History a)
addToHistory x (HS hid hs) = (hid', HS hid' hs')
  where
    hid' = hid + 1
    hs'  = M.insert hid' x hs

resetHistory :: HistoryID -> History a -> (Maybe a, History a)
resetHistory hid (HS hid' hs) = (x, HS hid' hs')
  where
    x   = M.lookup hid hs
    hs'
      | isJust x  = fst $ M.split hid hs
      | otherwise = hs

dropHistory :: HistoryID -> History a -> History a
dropHistory hid (HS hid' hs) = HS hid' hs'
  where
    hs' = snd $ M.split hid hs

-- --------------------

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
