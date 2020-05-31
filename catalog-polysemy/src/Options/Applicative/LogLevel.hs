module Options.Applicative.LogLevel
  ( -- option parsers
    optLogLevel
  , optQuiet
  , optErr
  , optWarn
  , optVerb
  , optTrc
  , optDbg
  -- * reexports
  , LogLevel (..)
  )
where

import Options.Applicative
import Polysemy.Logging     ( LogLevel (..) )

----------------------------------------

optQuiet :: Parser LogLevel
optQuiet =
  flag' LogNull
  ( long "quiet"
    <> short 'q'
    <> help "No output, even no errors or warnings"
  )

optErr :: Parser LogLevel
optErr =
  flag' LogErr
  ( long "errors"
    <> help "Only error messages (default)"
  )

optWarn :: Parser LogLevel
optWarn =
  flag' LogWarn
  ( long "warnings"
    <> short 'w'
    <> help "Only errors and warnings"
  )

optLog :: Parser LogLevel
optLog =
  flag' LogLog
  ( long "log-messages"
    <> short 'l'
    <> help "Only errors and warnings"
  )

optVerb :: Parser LogLevel
optVerb =
  flag' LogVerb
  ( long "verbose"
    <> short 'v'
    <> help "Verbose output"
  )

optTrc :: Parser LogLevel
optTrc =
  flag' LogTrc
  ( long "trace"
    <> short 't'
    <> help "Verbose and trace output"
  )

optDbg :: Parser LogLevel
optDbg =
  flag' LogDbg
  ( long "debug"
    <> help "Verbose, trace and debug output"
  )

optLogLevel :: Parser LogLevel
optLogLevel =
  optDbg <|> optTrc <|> optVerb <|> optLog <|> optWarn <|> optErr <|> optQuiet
  <|>
  pure LogErr


----------------------------------------
