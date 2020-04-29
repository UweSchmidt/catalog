{-# LANGUAGE ConstraintKinds #-}
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

module Client.Commands.Interpreter
  ( -- * Interpreter
    evalCCommands
  )
where

import Polysemy
import Polysemy.Error
import Polysemy.HttpRequest
import Polysemy.Logging
import Polysemy.Reader

import System.HttpRequest

import Client.Commands

import Server.Commands
import Server.Commands.ClientInterpreter

import Data.Prim
import Data.ImgNode
import Data.MetaData

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T

------------------------------------------------------------------------------

type CCmdEffects r = (Members '[Reader Host, SCommand, Logging] r)

type Host = (Text, Int)

evalCCommands :: CCmdEffects r => InterpreterFor CCommand r
evalCCommands =
  interpret $
  \ c -> case c of
    CcLs path -> evalLs path

------------------------------------------------------------------------------

evalLs :: CCmdEffects r => Path -> Sem r [Path]
evalLs path = do
  subcols <$> theEntry path
  where
    subcols :: ImgNodeP -> [Path]
    subcols n
      | isCOL  n  = n ^.. theColEntries . traverse . theColColRef
      | isROOT n  = n ^.. theRootImgCol
      | otherwise = []

------------------------------------------------------------------------------


------------------------------------------------------------------------------
