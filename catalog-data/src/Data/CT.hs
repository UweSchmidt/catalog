{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

------------------------------------------------------------------------
--
-- construct complex bash commands, e.g. for
-- ImageMagic command pipes
-- quoting is done automatically

module Data.CT
  ( CT    -- ^ Command Tree
  , CTT   -- ^ Command Tree with Text as basic pieces

    -- command constructors
  , mkCconc
  , mkCpipe
  , mkCseq
  , mkCexec

    -- adding options and arguments
  , addArg
  , addFlag
  , addOpt
  , addOptVal
  , addPipe
  , addSeq
  , addVal

    -- build commands
  , toBash
  , toWords

    -- predicates
  , isCmd
  , isCompCmd

    -- quoting
  , isUnquoted
  , quS
  , quT

    -- folds
  , foldCT
  )
where

import Data.Prim.Prelude           -- lens stuff

import qualified Data.Text as T

-- import qualified Data.Text.IO as T

------------------------------------------------------------------------

data CT val = Bin Cop (CT val) (CT val)
            | Val val
            | Nil

data Cop
  = Cconc     -- list conc for cmd options and args
  | Cexec     -- cmd to execute
  | Cargs     -- options and arguments
  | Cpipe     -- combine 2 cmds with a pipe (|)
  | Cseq      -- combine 2 cmds to a sequence (;)
  | Copt      -- options/flags

type CTT = CT Text

----------------------------------------
--
-- instances

deriving instance (Show val) => Show (CT val)
deriving instance Functor CT

instance Semigroup (CT val) where
  (<>) = mkCconc

instance Monoid (CT val) where
  mempty = Nil

instance IsEmpty (CT val) where
  isempty Nil = True
  isempty _   = False

deriving instance Eq   Cop
deriving instance Ord  Cop
deriving instance Enum Cop
deriving instance Show Cop

----------------------------------------
--
-- smart constructors

mkCconc :: CT val -> CT val -> CT val
mkCconc Nil   t = t
mkCconc t   Nil = t
mkCconc t1   t2 = Bin Cconc t1 t2

mkCpipe :: CT val -> CT val -> CT val
mkCpipe Nil t = t     -- empty commands are thrown away
mkCpipe t Nil = t
mkCpipe t1 t2
  | isCmd t1
    &&
    isCmd t2 = Bin Cpipe t1 t2
  | otherwise    = mempty

mkCseq :: CT val -> CT val -> CT val
mkCseq Nil t = t      -- empty commands are thrown away
mkCseq t Nil = t
mkCseq t1 t2
  | isCmd t1
    &&
    isCmd t2 = Bin Cseq t1 t2
  | otherwise    = mempty

mkVal :: val -> CT val
mkVal = Val

-- even smarter constructors

mkFlag :: val -> CT val
mkFlag val = Bin Copt (mkVal val) Nil

mkOptV :: val -> val -> CT val
mkOptV ov vv = Bin Copt (mkVal ov) (mkVal vv)

mkCexec :: val -> CT val
mkCexec x = Bin Cexec (Val x) (Bin Cargs Nil Nil)

--------------------
--
-- edit last exec cmd

-- add an argument to last exec

addArg :: CT val -> (CT val -> CT val)
addArg arg = editLastExec (_2tree . _2tree) (<> arg)

addVal :: val -> (CT val -> CT val)
addVal = addArg . mkVal

-- add an option to last exec
-- options are inserted in front of args

addOpt :: CT val -> (CT val -> CT val)
addOpt opt = editLastExec (_2tree . _1tree) (<> opt)

addFlag :: val -> (CT val -> CT val)
addFlag = addOpt . mkFlag

addOptVal :: val -> val -> (CT val -> CT val)
addOptVal n v = addOpt (mkOptV n v)

-- append a pipe command to last exec

addPipe :: val -> (CT val -> CT val)
addPipe cmd = editLastExec id (`mkCpipe` mkCexec cmd)

-- append a seq to last exec

addSeq :: val -> (CT val -> CT val)
addSeq cmd = editLastExec id (`mkCseq` mkCexec cmd)

--------------------

isCmd :: CT val -> Bool
isCmd (Bin o _ _) = o `elem` [Cexec, Cpipe, Cseq]
isCmd _           = False

isCompCmd :: CT val -> Bool
isCompCmd (Bin o _ _) = o `elem` [Cpipe, Cseq]
isCompCmd _           = False

----------------------------------------
--
-- internal edit and extend exec commands

editLastExec :: Traversal' (CT val) (CT val)
             -> (CT val -> CT val)
             -> (CT val -> CT val)
editLastExec trv = editLastExec' . editExec trv


editLastExec' :: (CT val -> CT val) -> (CT val -> CT val)
editLastExec' f t@(Bin Cexec _ _) = f t
editLastExec' f t@(Bin o l r)
  | isCompCmd t = Bin o l (editLastExec' f r)
editLastExec' _ t    = t


editExec :: Traversal' (CT val) (CT val)
         -> (CT val -> CT val)
         -> (CT val -> CT val)
editExec trv f t =
  t & trv %~ f


{- simple form of editExec

setExec :: Traversal' (CT val) (CT val)
         ->  CT val
         -> (CT val -> CT val)
setExec trv v t =
  t & trv %~ (<> v)

-- -}
----------------------------------------
--
-- folds

foldCT :: (Cop -> a -> a -> a) -> (val -> a) -> a -> CT val -> a
foldCT bop f c = go
  where
    go (Bin o l r) = bop o (go l) (go r)
    go (Val x)      = f x
    go Nil          = c


{- test

toCmdStr :: CT String -> String
toCmdStr = foldCT b2s id ("''")
  where
   b2s o ls rs = "(" <> ls <> o2s o <> rs <> ")"
   o2s o =
     " `" <> drop 1 (show o)  <> "` "
-- -}
----------------------------------------

toBash :: CT Text -> Text
toBash = T.unwords . toWords

toWords :: CT Text -> [Text]
toWords t0 = toCmd t0 []
  where
    toCmd Nil            = id
    toCmd (Val v)        = arg v
    toCmd (Bin o t1 t2) = case o of
      Cpipe -> toCompound t1 . sep . lit "|" . sep  . toCompound t2
      Cseq  -> toCmd t1 . lit ";" . sep . toCmd t2
      Cexec -> toCmd t1 . toArgs t2
      Cconc -> toCmd t1 . toCmd t2
      _     -> id                   -- illegal

    toCompound t@(Bin Cseq _ _) = lit "{" . toCmd t . lit ";}"
    toCompound t                = toCmd t

    toArgs (Bin Cargs t1 t2) = toArgs (t1 <> t2)
    toArgs (Bin Cconc t1 t2) = toArgs t1 . toArgs t2
    toArgs (Bin Copt  t1 t2) = toArgs t1 . toArgs t2
    toArgs (Val v)           = sep . arg v
    toArgs  Nil              = id
    toArgs  _                = id   -- illegal

    arg xs = (quT xs :)
    lit xs = (xs     :)
    sep    = id

----------------------------------------
--
-- optics

_1tree :: Traversal' (CT val) (CT val)
_1tree inj (Bin o t1 t2) = Bin o <$> inj t1 <*> pure t2
_1tree _   t             = pure t

_2tree :: Traversal' (CT val) (CT val)
_2tree inj (Bin o t1 t2) = Bin o t1 <$> inj t2
_2tree _   t             = pure t

------------------------------------------------------------------------
--
-- quoting

isUnquoted :: Char -> Bool
isUnquoted c =
  isAlphaNum c || c `elem` ("%+,-./:@^_" :: [Char])

quS :: String -> String
quS = concatMap qu
  where
    qu c
      | isUnquoted c = [c]
      | otherwise    = '\\' : [c]

quT :: Text -> Text
quT t
  | isJust $ T.find isUnquoted t = t & isoString %~ quS
  | otherwise                    = t

------------------------------------------------------------------------
{-

example :: CTT
example = mkCexec "echo"
          & addFlag "-n"
          & addVal  "Hello World!\nGood Bye?"
          & addPipe "cat"
          & addPipe "wc"
          & addFlag "-w"
          & addFlag "-l"

exrun :: IO ()
exrun = putStrLn . T.unpack . toBash $ example

-- -}
------------------------------------------------------------------------
