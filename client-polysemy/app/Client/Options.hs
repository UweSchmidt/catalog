module Client.Options
where

import Options.Applicative
import Options.Applicative.HostPort
import Options.Applicative.LogLevel

import Client.Commands
import Client.Commands.Interpreter
       ( defaultPath )

import Catalog.Workflow
       ( isoPathPos )

import Data.MetaData
       ( metaDataAt
       , prettyMD
       , allKeysMD
       , globKeysMD
       , selectByNames
       )

import Data.Prim
       hiding (argument)

import Text.SimpleParser
       ( parseMaybe
       , parseGlob
       )

-- import Data.Text (Text)

------------------------------------------------------------------------------

type Host = Text
type Port = Int

type ClientOpts = ((Host,Port), LogLevel)

version :: String
version = "0.2.8.2"

date :: String
date = "2020-04-15"

appname :: String
appname = "client-polysemy"

appInfo :: ParserInfo ClientOpts
appInfo =
  info (helper <*> optClient)
  ( fullDesc
    <> progDesc "query, modify and download collections from catalog"
    <> header ("catalog-" ++ appname ++ " - " ++ version ++ " (" ++ date ++ ")")
  )

optClient :: Parser ClientOpts
optClient = (,) <$> optHostPort <*> optLogLevel

type CmdParser r = Parser (CCommand r ())

cmdClient :: CmdParser r
cmdClient = subparser $
  command "ls"
    ( ( CcLs <$> pathP)
      `withInfo`
      ( "List subcollections, default PATH is: "
        <> show defaultPath
      )
    )
  <>
  command "ls-md"
    ( cmdLsmd
      `withInfo`
      ( "Show metadata for a collection or entry of a collection, "
        ++ "default PATH is: "
        ++ show defaultPath
      )
    )

cmdLsmd :: Parser (CCommand m ())
cmdLsmd =
  flip CcLsmd
  <$> option globParser
      ( long "keys"
        <> short 'k'
        <> metavar "GLOB-PATTERN"
        <> value allKeysMD
        <> help "Select metadata keys by a glob style pattern matching"
      )
  <*> pathPP

----------------------------------------

pathP :: Parser Path
pathP = pathP1 <|> pure defaultPath

pathP1 :: Parser Path
pathP1 = argument str (metavar "PATH")

pathPP :: Parser PathPos
pathPP = (^. isoPathPos) <$> pathP

pathPP1 :: Parser PathPos
pathPP1 = (^. isoPathPos) <$> pathP1

keyP :: Parser Name
keyP = argument globParser1 (metavar "KEY")

valP :: Parser Text
valP = argument str (metavar "VALUE")

partP :: Parser Name
partP = mkName <$> argument str (metavar "PART")
        <|>
        pure mempty

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

-- ----------------------------------------
--
-- app specific option parsers

imgReqReader :: ReadM ReqType
imgReqReader = eitherReader parse
  where
    parse arg =
      maybe
        (Left $ "Wrong image format: " ++ arg)
        Right
        (arg ^? prismString)

geoReader :: ReadM Geo
geoReader = eitherReader parse
  where
    parse arg =
      maybe
        (Left $ "Wrong geometry: " ++ arg)
        Right
        (readGeo' arg)

globParser :: ReadM [Name]
globParser = globParser' notNull
  where
    notNull arg [] = Left $ "No keys found for pattern: " ++ arg
    notNull _   xs = Right xs

globParser1 :: ReadM Name
globParser1 = globParser' single
  where
    single _   [x] = Right x
    single arg []  = Left $ "No key found for pattern: " ++ arg
    single arg xs  = Left $ "No unique key found for pattern: " ++ arg
                            ++ ", could be one of " ++ show xs

globParser' :: (String -> [Name] -> Either String a) -> ReadM a
globParser' check = eitherReader parse
  where
    parse arg =
      case parseMaybe parseGlob arg of
        Nothing -> Left $ "Wrong glob style pattern: " ++ arg
        Just gp -> check arg (globKeysMD gp)

------------------------------------------------------------------------------
