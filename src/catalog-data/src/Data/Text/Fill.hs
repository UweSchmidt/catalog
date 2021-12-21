{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Fill where

import           Data.Text (Text)
import qualified Data.Text as T

-- ----------------------------------------
--
-- pretty printing stuff

fillLeft :: Char -> Int -> Text -> Text
fillLeft c i xs = T.pack (replicate (i - T.length xs) c) <> xs

fillRight :: Char -> Int -> Text -> Text
fillRight c i xs = xs <> T.pack (replicate (i - T.length xs) c)

fillLeftList :: Char -> [Text] -> [Text]
fillLeftList = fillList fillLeft

fillRightList :: Char -> [Text] -> [Text]
fillRightList = fillList fillRight

fillList :: (Char -> Int -> Text -> Text)
         -> Char -> [Text] -> [Text]
fillList ff c xs = map (ff c l) xs
  where
    l = maximum (0 : map T.length xs)

------------------------------------------------------------------------
