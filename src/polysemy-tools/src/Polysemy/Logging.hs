{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------

module Polysemy.Logging
  (  -- * Effect
    Logging (..)

    -- * Actions
  , log'       -- log message with level
  , log'err    -- 0: errors
  , log'warn   -- 1: warnings
  , log'info   -- 2: info messages
  , log'verb   -- 3: verbose messages
  , log'trc    -- 4: trace messages
  , log'dbg    -- 5: debug messages
  , abort      -- abort with logging
  , abortWith  -- abort with text msg

    -- * Interpretations
  , logWithLevel       -- regular logging, requires Consume effect
  , logWithoutLevel    -- issue messages, ignore levels, dto
  , noLoggingAtAll     -- ignore all logging actions

    -- * Interpretations for LogMsg
  , logToStdErr
  , logToBGQueue

    -- * Data types
  , LogMsg (..)
  , LogLevel (..)

    -- * aux functions
  , untext
  )
where

import Polysemy
       ( InterpreterFor
       , Member
       , Sem
       , Embed
       , runM
       , makeSem
       , reinterpret
       , interpret
       )
import Polysemy.Consume
       ( Consume
       , consumeIO
       , consume
       )
import Polysemy.Consume.BGQueue
       ( BGQueue
       , writeToBGQueue
       )
import Polysemy.Error
       ( Error
       , throw
       )

import Data.Text
       ( Text )

import System.IO
       ( Handle
       , stderr
       )

import Data.Maybe (fromMaybe)

import qualified Data.List    as L
import qualified Data.Text    as T
import qualified Data.Text.IO as T

------------------------------------------------------------------------------

newtype LogMsg = LogMsg {logMsgToText :: Text}

data LogLevel = LogNull | LogErr | LogWarn | LogInfo | LogVerb | LogTrc | LogDbg
  deriving (Eq, Ord)

prettyLogLevel :: LogLevel -> Text
prettyLogLevel LogErr  = "error:   "
prettyLogLevel LogWarn = "warning: "
prettyLogLevel LogInfo = "info:    "
prettyLogLevel LogVerb = "verbose: "
prettyLogLevel LogTrc  = "trace:   "
prettyLogLevel LogDbg  = "debug:   "
prettyLogLevel LogNull = "quiet:   "

prettyIndent :: Text
prettyIndent =  "\n         "

prettyLog :: LogLevel -> Text -> Text
prettyLog ll t =
  T.intercalate prettyIndent ((prettyLogLevel ll <> l1) : ls)
  where
    (l1, ls) = fromMaybe ("", []) . L.uncons  . T.lines $ t

-- --------------------

data Logging m a where
  Log' :: LogLevel -> LogMsg -> Logging m ()

makeSem ''Logging

------------------------------------------------------------------------------

-- | log messages with priority >= given logLevel
--
-- Logging effect reinterpreted with Consume LogMsg effect

logWithLevel :: LogLevel
             -> Sem (Logging ': r) a
             -> Sem (Consume LogMsg ': r) a
logWithLevel LogNull =    -- disable all logging
  reinterpret $
  \ case
    Log' _l _msg -> pure ()

logWithLevel logLevel =
  reinterpret $
  \ case
    Log' l (LogMsg msg) -> do
      if LogNull < l && l <= logLevel
        then consume $ LogMsg (prettyLog l msg)
        else pure ()


-- | log all messages, ignore any levels
--
-- Logging effect reinterpreted with Consume LogMsg effect

logWithoutLevel :: Sem (Logging ': r) a
                -> Sem (Consume LogMsg ': r) a
logWithoutLevel =
  reinterpret $
  \ case
    Log' _l msg -> consume msg


-- | ignore all log messages

noLoggingAtAll ::  Sem (Logging ': r) a
                -> Sem r a
noLoggingAtAll =
  interpret $
  \ case
    Log' _l _msg -> return ()

{-# INLINE noLoggingAtAll #-}
{-# INLINE logWithoutLevel #-}
{-# INLINE logWithLevel #-}


log'err
  , log'warn
  , log'info
  , log'verb
  , log'trc
  , log'dbg :: Member Logging r => Text -> Sem r ()
log'err  = log' LogErr  . LogMsg
log'warn = log' LogWarn . LogMsg
log'info = log' LogInfo . LogMsg
log'verb = log' LogVerb . LogMsg
log'trc  = log' LogTrc  . LogMsg
log'dbg  = log' LogDbg  . LogMsg

{-# INLINE log'err  #-}
{-# INLINE log'warn #-}
{-# INLINE log'info #-}
{-# INLINE log'verb #-}
{-# INLINE log'trc  #-}
{-# INLINE log'dbg  #-}

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
  log'trc msg
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
