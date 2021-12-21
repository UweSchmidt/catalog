----------------------------------------
--
-- option parsers for Host and Port

module Options.Applicative.HostPort
  ( optHost
  , optPort
  , optHostPort
  )
where

import Options.Applicative
import Data.Text (Text)

import qualified Data.Text as T

----------------------------------------

optHost :: Parser Text
optHost =
  T.pack <$>
  strOption
  ( long "host"
    <> short 'H'
    <> metavar "HOST"
    <> showDefault
    <> value "localhost"
    <> help "The host name of the server"
  )

optPort :: Parser Int
optPort =
  option auto
  ( long "port"
    <> short 'P'
    <> help "The port number of the server"
    <> showDefault
    <> value 3001
    <> metavar "PORT"
  )

optHostPort :: Parser (Text, Int)
optHostPort =
  (,) <$> optHost
      <*> optPort

----------------------------------------
