{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------

module Polysemy.ExecProg
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
       ( InterpreterFor
       , Member
       , Sem
       , Embed
       , makeSem
       , interpret
       )
import Polysemy.EmbedExc
       ( IOException
       , embedExc
       )
import Polysemy.Error
       ( Error
       , throw
       )
import Polysemy.Logging
       ( Logging
       , untext
       , log'warn
       , log'err
       , log'dbg
       )

import Control.Monad
       ( unless )

import Data.ByteString
       ( ByteString )

import Data.Text
       ( Text )

import System.Exit
       ( ExitCode(ExitFailure, ExitSuccess) )

import qualified Data.ByteString.Char8     as BS
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified System.IO.Error           as EX
import qualified System.Process.ByteString as XBS
import qualified System.Process.Text       as XT

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
  \ case
    ExecProg prg args inp -> do
      execByteString mkExc prg args inp

    ExecProgText prg args inp -> do
      execText mkExc prg args inp

    -- exec bash script with Text input and Text as output
    -- UTF8 decoding errors raise an exception
    ExecScript script -> do
      execText mkExc "bash" [] script

{-# INLINE systemProcess #-}

-- run process with binary IO, stdin and stdout as ByteString

execByteString :: forall exc r.
                  ( Member (Embed IO) r
                  , Member (Error exc) r
                  , Member Logging r
                  )
               => (IOException -> exc)
               -> ProgPath
               -> [Text]
               -> ByteString
               -> Sem r ByteString
execByteString mkExc prg args inp = do
  let prg'  =     T.unpack prg
      args' = map T.unpack args

  log'dbg $
    untext $ ["exec:", prg]
             <> args
             <> if BS.null inp
                then mempty
                else [", stdin: " <> bs2text inp]

  (rc, out, err) <-
    embedExc mkExc $
    XBS.readProcessWithExitCode prg' args' inp

  log'dbg $ bs2text out

  let err' = bs2text err
  case rc of
    ExitSuccess -> do
      unless (BS.null err) $
        log'warn $ "exec: stderr=" <> err'
      return out

    ExitFailure rcn -> do
      log'err $ "exec: stderr=" <> err'
      throw @exc $
        mkExc (EX.userError $ "exec: rc = " ++ show rcn)

----------------------------------------

execText :: forall exc r.
            ( Member (Embed IO) r
            , Member (Error exc) r
            , Member Logging r
            )
         => (IOException -> exc)
         -> ProgPath
         -> [Text]
         -> Text
         -> Sem r Text
execText mkExc prg args inp = do
  let prg'  =     T.unpack prg
      args' = map T.unpack args

  log'dbg $
    untext $ ["exec:", prg]
             <> args
             <> if T.null inp
                then mempty
                else [", stdin: " <> inp]

  (rc, out, err) <-
    embedExc mkExc $
    XT.readProcessWithExitCode prg' args' inp

  log'dbg out

  case rc of
    ExitSuccess -> do
      unless (T.null err) $
        log'warn $ "exec: stderr=" <> err
      return out

    ExitFailure rcn -> do
      log'err $ "exec: stderr=" <> err
      throw @exc $
        mkExc (EX.userError $ "exec: rc = " ++ show rcn)

----------------------------------------

bs2text :: ByteString -> Text
bs2text = T.decodeUtf8Lenient -- T.pack . BS.unpack

------------------------------------------------------------------------------
