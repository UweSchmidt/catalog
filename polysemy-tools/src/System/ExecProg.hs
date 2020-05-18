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
  , execProg        -- stdin and stdout as ByteString
  , execProgText    -- stdin and stdout as Text
  , execScript

    -- * Derived Actions
  , execProg0

    -- * Interpreter
  , systemProcess
  )
where

import Polysemy
import Polysemy.EmbedExc
import Polysemy.Error
import Polysemy.Logging

import Control.Monad      (unless)
import Data.ByteString    (ByteString)
import Data.Text          (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import System.Exit


import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T
import qualified System.IO.Error       as EX
import qualified System.Process        as X

------------------------------------------------------------------------------
--
-- | call external program

type ProgPath = Text

data ExecProg m a where
  ExecProg     :: ProgPath -> [Text] -> ByteString -> ExecProg m ByteString
  ExecProgText :: ProgPath -> [Text] -> Text       -> ExecProg m Text
  ExecScript   ::                       Text       -> ExecProg m Text

makeSem ''ExecProg

execProg0 :: Member ExecProg r
          => ProgPath -> [Text] -> Sem r ByteString
execProg0 prg args = execProg prg args mempty

------------------------------------------------------------------------------
--
-- | call external program via System.Process.readProcessWithExitCode
--
-- prog name and args are given as Text
-- input and output are represented as a strict ByteString
--
-- System.Process works with String as argument and result types
-- this is inefficient, but it's like it is
--
-- Encoding of input and decoding of output is part of
-- the calling commands, here stdin and stdout get sequences of bytes
-- char encoding is not handled

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
      exec mkExc prg args inp

    ExecProgText prg args inp -> do
      execText mkExc prg args inp

    -- exec bash script with Text input and Text as output
    -- UTF8 decoding errors raise an exception
    ExecScript script -> do
      execText mkExc "bash" [] script

{-# INLINE systemProcess #-}


exec :: forall exc r.
        ( Member (Embed IO) r
        , Member (Error exc) r
        , Member Logging r
        )
     => (IOException -> exc)
     -> ProgPath -> [Text] -> ByteString -> Sem r ByteString
exec mkExc prg args inp = do
  let prg'  =     T.unpack prg
      args' = map T.unpack args
      inp'  =     BS.unpack inp

  log'trc $
    untext $ ["exec:", prg]
             <> args
             <> if null inp'
                then mempty
                else [", stdin: " <> T.pack inp']

  (rc, out', err') <-
    embedExc mkExc $
    X.readProcessWithExitCode prg' args' inp'

  let out = BS.pack out'
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


execText :: forall exc r.
            ( Member (Embed IO) r
            , Member (Error exc) r
            , Member Logging r
            )
         => (IOException -> exc)
         -> ProgPath -> [Text] -> Text -> Sem r Text
execText mkExc prog args inp = do
  res <- exec mkExc prog args (encodeUtf8 inp)
  case decodeUtf8' res of
    Left  e -> do
      throw @exc $
        mkExc (EX.userError $ "exec: unicode decode error " ++ show e)

    Right t -> return t

------------------------------------------------------------------------------
