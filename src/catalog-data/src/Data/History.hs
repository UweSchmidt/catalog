------------------------------------------------------------------------------

module Data.History
  ( HistoryID
  , History
  , emptyHistory
  , addToHistory
  , resetHistory
  , dropHistory
  , entriesInHistory
  )
where

import Data.Prim.Prelude
       ( isJust )

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

entriesInHistory :: History a -> [(HistoryID, a)]
entriesInHistory (HS _hid hs) = M.toDescList hs

-- ----------------------------------------
