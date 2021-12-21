module Options.Applicative.LogLevel
  ( -- option parsers
    optLogLevel
  , optQuiet
  , optErr
  , optWarn
  , optInfo
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
    <> help "Only error messages"
  )

optWarn :: Parser LogLevel
optWarn =
  flag' LogWarn
  ( long "warnings"
    <> short 'w'
    <> help "Only errors and warnings"
  )

optInfo :: Parser LogLevel
optInfo =
  flag' LogInfo
  ( long "info"
    <> short 'i'
    <> help "Info, error and warning messages (default)"
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
  optDbg <|> optTrc <|> optVerb <|> optInfo <|> optWarn <|> optErr <|> optQuiet
  <|>
  pure LogInfo


----------------------------------------
