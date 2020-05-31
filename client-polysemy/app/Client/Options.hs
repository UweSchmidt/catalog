------------------------------------------------------------------------------

module Client.Options
where

import Options.Applicative
import Options.Applicative.HostPort
import Options.Applicative.LogLevel

import Client.Effects.ClientCmd
import Client.Effects.ClientCmd.Interpreter
       ( defaultPath )


import Data.MetaData
       ( allKeysMD
       , globKeysMD
       -- , metaDataAt
       -- , prettyMD
       -- , selectByNames
       )

import Data.Prim
       hiding (argument)

import Text.SimpleParser
       ( parseMaybe
       , parseGlob
       )

------------------------------------------------------------------------------

type Host = Text
type Port = Int

type ClientAct r = (ClientOpts, ClientCmd r ())
type ClientOpts  = ((Host,Port), LogLevel)

version :: String
version = "0.2.12.0"

date :: String
date = "2020-05-31"

appname :: String
appname = "client-polysemy"

clientAction :: IO (ClientAct r)
clientAction = execParser appInfo

appInfo :: ParserInfo (ClientAct r)
appInfo =
  info (helper <*> ((,) <$> optClient <*> cmdClient))
  ( fullDesc
    <> progDesc "query, modify and download collections from catalog"
    <> header (appname <> " - " <> version <> " (" <> date <> ")")
  )

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

optClient :: Parser ClientOpts
optClient = (,) <$> optHostPort <*> optLogLevel

type CmdParser r = Parser (ClientCmd r ())

cmdClient :: CmdParser r
cmdClient = subparser $
  command "ls"
    ( (CcLs <$> argPath)
      `withInfo`
      ( "List subcollections, default PATH is: "
        <> show defaultPath
      )
    )
  <>
  command "ls-md"
    ( ( flip CcLsmd
        <$> option globParser
            ( long "keys"
              <> short 'k'
              <> metavar "GLOB-PATTERN"
              <> value allKeysMD
              <> help "Select metadata keys by a glob style pattern matching"
            )
        <*> argPathPos
      )
      `withInfo`
      ( "Show metadata for a collection or entry of a collection, "
        ++ "default PATH is: "
        ++ show defaultPath
      )
    )
  <>
  command "set-md"
    ( ( CcSetmd1
        <$> argPathPos1
        <*> argKey
        <*> argValue
      )
      `withInfo`
      ( "Set metadata attr for a collection or entry of a collection, "
        ++ "default PATH is: "
        ++ show defaultPath
        ++ ", key may be given as glob pattern"
      )
    )
  <>
  command "del-md"
    ( ( CcDelmd1
        <$> argPathPos1
        <*> argKey
      )
      `withInfo`
      ( "Delete metadata attr for a collection or entry of a collection, "
        ++ "default PATH is: "
        ++ show defaultPath
        ++ ", key may be given as glob pattern"
      )
    )
  <>
  command "download"
    ( ( let dl reqtype' geo' dest' seqno' overwrite' path'
              = CcDownload path' reqtype' geo' dest' seqno' overwrite'

            img'variants :: String
            img'variants =
              intercalate ", " . map (^. isoString) $ imgReqTypes in
        dl
        <$> option imgReqReader
            ( long "variant"
              <> short 'i'
              <> help ( "The image variant, one of ["
                        <> img'variants
                        <> "], default: img"
                      )
              <> value RImg
              <> metavar "IMG-VARIANT"
            )
        <*> option geoReader
            ( long "geometry"
              <> short 'g'
              <> help "The image geometry: <width>x<height> or org (original size)"
              <> value (Geo 1 1)
              <> metavar "GEOMETRY"
            )
        <*> strOption
            ( long "dest"
              <> short 'd'
              <> metavar "DOWNLOAD-DIR"
              <> showDefault
              <> value "."
              <> help "The dir to store downloads"
            )
        <*> flag False True
            ( long "with-seq-no"
              <> short 'n'
              <> help ("Prefix downloaded images with a sequence number"
                       <> " (useful for digital photo frame to show "
                       <> " the pictures in collection order)"
                      )
            )
        <*> flag False True
            ( long "force"
              <> short 'f'
              <> help "Force destination file overwrite when downloading files"
            )
        <*> argPath1
      )
      `withInfo`
      "Download all images of a collection"
    )
  <>
  command "checksum"
    ( ( let cc onlyUpdate onlyMissing p n =
              CcCheckSum p n onlyUpdate onlyMissing
        in
        cc <$> optOnlyUpdate <*> optOnlyMissing <*> argPath1 <*> argPart
      )
      `withInfo`
      ( "Show checksums for image files "
        ++ "of a catalog entry for an image or a whole image dir"
      )
    )
  <>
  command "update-checksum"
    ( ( let cc onlyUpdate forceUpdate p n =
              CcUpdCSum p n onlyUpdate forceUpdate
        in
        cc <$> optOnlyUpdate <*> optForceUpdateP <*> argPath1 <*> argPart
      )
      `withInfo`
      ( "Compute, check and/or update checksums "
        ++ "of an image or a whole image dir"
      )
    )
  <>
  command "entry"
    ( (CcEntry <$> argPath)
      `withInfo`
      ( "Dump catalog entry, for testing and debugging, default Path is: "
        ++ show defaultPath
      )
    )
  <>
  command "snapshot"
    ( ( CcSnapshot
        <$> strOption ( long "message"
                        <> short 'm'
                        <> metavar "MESSAGE"
                        <> help "The git commit message"
                      )
      )
      `withInfo`
      ( "Take a snapshot of catalog" )
    )

----------------------------------------
--
-- argument parsers

argPath :: Parser Path
argPath = argPath1 <|> pure defaultPath

argPath1 :: Parser Path
argPath1 = argument str (metavar "PATH")

argPathPos :: Parser PathPos
argPathPos = (^. isoPathPos) <$> argPath

argPathPos1 :: Parser PathPos
argPathPos1 = (^. isoPathPos) <$> argPath1

argKey :: Parser Name
argKey = argument globParser1 (metavar "KEY")

argValue :: Parser Text
argValue = argument str (metavar "VALUE")

argPart :: Parser Name
argPart = mkName <$> argument str (metavar "PART")
        <|>
        pure mempty

-- ----------------------------------------
--
-- subcmd options parsers

optOnlyUpdate :: Parser Bool
optOnlyUpdate =
  flag False True
  ( long "only-update"
    <> help "only checksum updates with new files, no checks done"
  )

optOnlyMissing :: Parser Bool
optOnlyMissing =
  flag False True
  ( long "only-missing"
    <> help "Only show files with wrong or missing checksums"
  )

optForceUpdateP :: Parser Bool
optForceUpdateP =
  flag False True
  ( long "force-update"
    <> help "in case of checksum error update checksum with new value"
  )

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
