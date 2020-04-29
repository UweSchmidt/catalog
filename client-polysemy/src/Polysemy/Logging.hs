{-# LANGUAGE
    DataKinds,
    FlexibleContexts,
    GADTs,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------

module Polysemy.Logging
  (  -- * Effect
    Logging (..)

    -- * Actions
  , log'       -- log message with level
  , log'err    -- 0: errors
  , log'warn   -- 1: warnings
  , log'verb   -- 2: verbose messages
  , log'trc    -- 3: trace messages
  , log'dbg    -- 4: debug messages
  , abort      -- abort with logging
  , abortWith  -- abort with text msg

    -- * Interpretations
  , logWithLevel
  , logWithoutLevel
  , logToStdErr
  , logToBGQueue

    -- * Data types
  , LogMsg
  , LogLevel (..)
  , logMsgToText

    -- * aux functions
  , untext
  )
where

import Polysemy
import Polysemy.Consume
import Polysemy.Consume.BGQueue
import Polysemy.Error

import           Data.Text    ( Text )
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           System.IO    ( Handle
                              , stderr
                              )

------------------------------------------------------------------------------

newtype LogMsg = LogMsg {logMsgToText :: Text}

data LogLevel = LogNull | LogErr | LogWarn | LogVerb | LogTrc | LogDbg
  deriving (Eq, Ord)

prettyLogLevel :: LogLevel -> Text
prettyLogLevel LogErr  = "error:   "
prettyLogLevel LogWarn = "warning: "
prettyLogLevel LogVerb = "verbose: "
prettyLogLevel LogTrc  = "trace:   "
prettyLogLevel LogDbg  = "debug:   "
prettyLogLevel LogNull = "quiet:   "

-- --------------------

data Logging m a where
  Log' :: LogLevel -> LogMsg -> Logging m ()

makeSem ''Logging

------------------------------------------------------------------------------

logWithoutLevel :: Member (Consume LogMsg) r
                => InterpreterFor Logging r
logWithoutLevel =
  interpret $
  \ c -> case c of
    Log' _l msg -> consume msg

logWithLevel :: Member (Consume LogMsg) r
             => LogLevel
             -> InterpreterFor Logging r
logWithLevel LogNull =    -- disable all logging
  interpret $
  \ c -> case c of
    Log' _l _msg -> pure ()

logWithLevel logLevel =
  interpret $
  \ c -> case c of
    Log' l (LogMsg msg) -> do
      if LogNull < l && l <= logLevel
        then consume $ LogMsg (prettyLogLevel l <> msg)
        else pure ()

{-# INLINE logWithoutLevel #-}
{-# INLINE logWithLevel #-}


log'err
  , log'warn
  , log'verb
  , log'trc
  , log'dbg :: Member Logging r => Text -> Sem r ()
log'err  = log' LogErr  . LogMsg
log'warn = log' LogWarn . LogMsg
log'verb = log' LogVerb . LogMsg
log'trc  = log' LogTrc  . LogMsg
log'dbg  = log' LogDbg  . LogMsg

{-# INLINE log'err #-}
{-# INLINE log'warn #-}
{-# INLINE log'verb #-}
{-# INLINE log'trc #-}
{-# INLINE log'dbg #-}

untext :: [Text] -> Text
untext = T.unwords

abortWith :: ( Member (Error Text) r
             , Member Logging r
             )
          => Text
          -> Sem r a
abortWith = abort id

abort :: forall e r a
       . ( Member (Error e) r
         , Member Logging r
         )
      => (Text -> e) -> Text
      -> Sem r a
abort ef msg = do
  log'err msg
  throw @e (ef msg)


------------------------------------------------------------------------------
--
-- | Logging to file

logToStdErr :: Member (Embed IO) r
            => InterpreterFor (Consume LogMsg) r
logToStdErr = logToHandle stderr

{-# INLINE logToStdErr #-}

logToHandle :: Member (Embed IO) r
            => Handle
            -> InterpreterFor (Consume LogMsg) r
logToHandle h = consumeIO (T.hPutStrLn h . logMsgToText)

{-# INLINE logToHandle #-}

------------------------------------------------------------------------------
--
-- | Logging to Background Queue

logToBGQueue :: Member (Embed IO) r
             => BGQueue
             -> InterpreterFor (Consume LogMsg) r
logToBGQueue qu =
  writeToBGQueue qu logMsgToIO
  where
    logMsgToIO :: LogMsg -> IO ()
    logMsgToIO = runM . logToStdErr . consume

------------------------------------------------------------------------------
