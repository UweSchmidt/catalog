{-# LANGUAGE RankNTypes #-}

module Control.Lens.Util where

import           Control.Lens
import qualified Data.Map.Strict as M
import           Data.Prim.Prelude
import qualified Data.Set as S
import qualified Data.Text as T

-- ----------------------------------------

-- an iso for converting a list of elemets into a map,
-- the key function extracts the keys of the elements

isoMapElems :: Ord k => (e -> k) -> Iso' (Map k e) [e]
isoMapElems key = iso M.elems (M.fromList . map (\ e -> (key e, e)))

-- an iso for converting between maps and list of pairs

isoMapList :: Ord a => Iso' (Map a b) ([(a, b)])
isoMapList = iso M.toList M.fromList

isoSetList :: Ord a => Iso' (Set a) [a]
isoSetList = iso S.toList S.fromList

isoStringText :: Iso' String Text
isoStringText = iso T.pack T.unpack

-- a prism for filtering

is :: (a -> Bool) -> Prism' a a
is p = prism id (\ o -> (if p o then Right else Left) o)

-- ----------------------------------------
