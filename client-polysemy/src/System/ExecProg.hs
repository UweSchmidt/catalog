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


module System.ExecProg
  ( -- Effect
    ExecProg (..)

    -- * Actions
  , execProg

    -- * Interpreter
  , systemProcess
  )
where

import Polysemy
import Polysemy.EmbedExc
import Polysemy.Error
import Polysemy.Logging

import Control.Monad ( unless )
import Data.Text     ( Text )
import System.Exit

import qualified Data.Text            as T
import qualified System.IO.Error      as EX
import qualified System.Process       as X

------------------------------------------------------------------------------

type ProgPath = Text

data ExecProg m a where
  ExecProg :: ProgPath -> [Text] -> Text -> ExecProg m Text

makeSem ''ExecProg

------------------------------------------------------------------------------
--
-- | call external program
--


systemProcess :: forall exc r
               . ( Member (Embed IO) r
                 , Member (Error exc) r
                 , Member Logging r
                 )
              => (IOException -> exc)
              -> InterpreterFor ExecProg r
systemProcess mkExc =
  interpret $
  \ c -> case c of
    ExecProg prg args inp -> do
      let prg'  =     T.unpack prg
          args' = map T.unpack args
          inp'  =     T.unpack inp

      log'trc $
         T.unwords $
         ["exec:", prg] <> args <> [", stdin:" <> inp]

      (rc, out', err') <-
         embedExc mkExc $
         X.readProcessWithExitCode prg' args' inp'

      let out = T.pack out'
          err = T.pack err'

      case rc of
        ExitSuccess -> do
          unless (T.null err) $
            log'warn err
          return out

        ExitFailure rcn -> do
          log'err err
          throw @exc $
            mkExc (EX.userError $ "exec: rc = " ++ show rcn)

------------------------------------------------------------------------------
