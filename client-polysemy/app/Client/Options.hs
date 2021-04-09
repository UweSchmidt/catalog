{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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
       ( MetaKey
       , allKeysMetaData
       , globKeysMetaData
       )

import Data.Prim
       hiding (argument)

import Text.SimpleParser
       ( parseMaybe
       , parseGlobNoCase
       )

import Client.Version

------------------------------------------------------------------------------

type Host = Text
type Port = Int

type ClientAct r = (ClientOpts, ClientCmd r ())
type ClientOpts  = ((Host, Port), LogLevel)

clientAction :: IO (ClientAct r)
clientAction = execParser appInfo

appInfo :: ParserInfo (ClientAct r)
appInfo =
  info (helper <*> ((,) <$> optClient <*> cmdClient))
  ( fullDesc
    <> progDesc "query, modify and download collections from catalog"
    <> header userAgent
  )

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

optClient :: Parser ClientOpts
optClient = (,) <$> optHostPort <*> optLogLevel

type CmdParser r = Parser (ClientCmd r ())

cmdClient :: CmdParser r
cmdClient = subparser $
  command "ls-sub"
    ( (CcLsSub <$> argPath)
      `withInfo`
      ( "List subnodes of collections and dirs, default PATH is: "
        <> show defaultPath
        <> ". Path may contain glob style patterns"
      )
    )
  <>
  command "ls-md"
    ( ( flip CcLsmd
        <$> option globParser
            ( long "keys"
              <> short 'k'
              <> metavar "GLOB-PATTERN"
              <> value allKeysMetaData
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
        <> "default PATH is: "
        <> show defaultPath
        <> ", key may be given as glob pattern"
      )
    )
  <>
  command "set-col-img"
    ( ( CcSetColImg
        <$> (second Just <$> argImgPath "IMG-PATH")
        <*> argPath1
      )
      `withInfo`
      ( "Set a collection image."
        <> " The image path must point to a .jpg image"
      )
    )
  <>
  command "del-col-img"
    ( ( CcSetColImg (mempty, Just (-1))
        <$> argPath1
      )
      `withInfo`
      ( "Clear the collection image." )
    )
  <>
  command "set-col-blog"
    ( ( CcSetColImg
        <$> (second Just <$> argImgPath "DOC-PATH")
        <*> argPath1
      )
      `withInfo`
      ( "Set a collection blog page."
        <> " The blog path must point to a .txt or .md document"
      )
    )
  <>
  command "del-col-blog"
    ( ( CcSetColImg (mempty, Just (-1))
        <$> argPath1
      )
      `withInfo`
      ( "Clear the collection blog document." )
    )
  <>
  command "del-md"
    ( ( CcDelmd1
        <$> argPathPos1
        <*> argKey
      )
      `withInfo`
      ( "Delete metadata attr for a collection or entry of a collection, "
        <> "default PATH is: "
        <> show defaultPath
        <> ", key may be given as glob pattern."
      )
    )
  <>
  command "download"
    ( ( let dl reqtype' geo' dest' seqno' overwrite' path'
              = CcDownload path' reqtype' geo' dest'' seqno' overwrite'
                where
                  dest''
                    | isempty dest' = lastPath path' ^. isoText
                    | otherwise     = dest'

            img'variants :: String
            img'variants =
              intercalate ", " . map (^. isoString) $ imgReqTypes in
        dl
        <$> option imgReqReader
            ( long "variant"
              <> short 'i'
              <> value RImg
              <> metavar "IMG-VARIANT"
              <> help ( "The image variant, one of ["
                        <> img'variants
                        <> "], default: img."
                      )
            )
        <*> option geoReader
            ( long "geometry"
              <> short 'g'
              <> value (Geo 1 1)
              <> metavar "GEOMETRY"
              <> help "The image geometry: <width>x<height> or org (original size)."
            )
        <*> strOption
            ( long "dest"
              <> short 'd'
              <> metavar "DOWNLOAD-DIR"
              <> value ""
              <> help ("The dir to store downloads"
                       <> " (default: ./<last collection name in PATH>)"
                      )
            )
        <*> flag False True
            ( long "with-seq-no"
              <> short 'n'
              <> help ("Prefix downloaded images with a sequence number"
                       <> " (useful for digital photo frame to show "
                       <> " the pictures in collection order)."
                      )
            )
        <*> flag False True
            ( long "force"
              <> short 'f'
              <> help "Force destination file overwrite when downloading files."
            )
        <*> argPath1
      )
      `withInfo`
      ( "Download all images of a collection"
        <> ". Glob style patterns are allowed in path."
      )
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
        <> "of a catalog entry for an image or a whole image dir"
        <> ". Glob style patterns are allowed in path."
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
        <> "of an image or a whole image dir"
        <> ". Glob style patterns are allowed in path."
      )
    )
  <>
  command "media"
    ( (CcMediaPath <$> argPath1)
      `withInfo`
      ( "Compute media path for a collection entry"
        <> " or media paths for an image entry."
      )
    )
  <>
  command "glob"
    ( (CcGlob <$> argPath)
      `withInfo`
      ( "Expand glob style patterns in path, default path is: "
        <> show defaultPath
        <> "."
      )
    )
  <>
  command "entry"
    ( (CcEntry <$> argPath)
      `withInfo`
      ( "Dump catalog entry (entries), for testing and debugging, default path is: "
        <> show defaultPath
        <> ". Glob style patterns are allowed in path."
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
      ( "Take a snapshot of catalog." )
    )
  <>
  command "undo-history"
    ( pure CcUndoList
      `withInfo`
      "List undo history."
    )
  <>
  command "undo"
    ( ( CcApplyUndo
        <$> argHid
      )
      `withInfo`
      ( "Reset to state before undo history entry number"
        <> " HISTORY-ID."
      )
    )
  <>
  command "drop-undo"
    ( ( CcDropUndo
        <$> ( argHid <|> pure 0 )
      )
      `withInfo`
      ( "Shorten undo history. Drop all edits older than entry number"
        <> " HISTORY-ID."
        <> " If no entry number given, drop all edits older than"
        <> " last 'catalog save' command"
      )
    )
  <>
  command "exif-update"
    ( ( let cc recUpdate forceUpdate p =
             CcExifUpdate p recUpdate forceUpdate
        in
          cc
          <$> flag False True
              ( long "recursive"
                <> short 'r'
                <> help ( "Recursively update all (sub-) directories." )
              )
          <*> flag False True
              ( long "force"
                <> short 'f'
                <> help ( "Force recomputing EXIF metadata even when no"
                          <> " file has been changed since last update."
                        )
              )
          <*> argPath1
      )
      `withInfo`
      ( "Update EXIF info for images and image dirs." )
    )
  <>
  command "check-meta"
    ( (CcCheckMeta <$> argPath)
      `withInfo`
      ( "Check consitency of image metadata, default path is: "
        <> show defaultPath
        <> ". Glob style patterns are allowed in path."
      )
    )
  <>
  command "geo-address"
    ( ( let cc forceUpdate p =
              CcGeoAddress p forceUpdate
        in
          cc
          <$> flag False True
              ( long "force"
                <> short 'f'
                <> help ( "Force recomputing address metadata even if it's"
                          <> " already set."
                        )
              )
          <*> argPath

      )
      `withInfo`
      ( "Import address data for GPS location of images/collections into metadata."
        <> " Path may point to a collection, a single image object"
        <> " or directory of images."
        <> " Globstyle patterns are allowed."
        <> " Open street map is used for reverse geo location service."
      )
    )

----------------------------------------
--
-- argument parsers

argHid :: Parser HistoryID
argHid = argument hidReader (metavar "HISTORY-ID")

argImgPath :: String -> Parser (Path, Int)
argImgPath path = argument imgPathReader (metavar path)

argPath :: Parser Path
argPath = argPath1 <|> pure defaultPath

argPath1' :: String -> Parser Path
argPath1' path = argument str (metavar path)

argPath1 :: Parser Path
argPath1 = argPath1' "PATH"

argPathPos :: Parser PathPos
argPathPos = (^. isoPathPos) <$> argPath

argPathPos1 :: Parser PathPos
argPathPos1 = (^. isoPathPos) <$> argPath1

argKey :: Parser MetaKey
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

hidReader :: ReadM HistoryID
hidReader = eitherReader parse
  where
    parse arg =
      maybe
        (Left $ "History number expected: " <> arg)
        Right
        arg'
      where
        arg' = readMaybe arg

imgPathReader :: ReadM (Path, Int)
imgPathReader = eitherReader parse
  where
    parse arg =
      maybe
        (Left $ "No image path format: " <> arg)
        Right
        arg'
      where
        arg' = fmap (p,) cx
        (p, cx) = (isoString # arg) ^. isoPathPos

imgReqReader :: ReadM ReqType
imgReqReader = eitherReader parse
  where
    parse arg =
      maybe
        (Left $ "Wrong image format: " <> arg)
        Right
        (arg ^? prismString)

geoReader :: ReadM Geo
geoReader = eitherReader parse
  where
    parse arg =
      maybe
        (Left $ "Wrong geometry: " <> arg)
        Right
        (readGeo' arg)

globParser :: ReadM [MetaKey]
globParser = globParser' notNull
  where
    notNull arg [] = Left $ "No keys found for pattern: " <> arg
    notNull _   xs = Right xs

globParser1 :: ReadM MetaKey
globParser1 = globParser' single
  where
    single _   [x] = Right x
    single arg []  = Left $ "No key found for pattern: " <> arg
    single arg xs  = Left $ "No unique key found for pattern: " <> arg
                            <> ", could be one of " <> show xs

globParser' :: (String -> [MetaKey] -> Either String a) -> ReadM a
globParser' check = eitherReader parse
  where
    parse arg =
      case parseMaybe parseGlobNoCase arg of
        Nothing -> Left $ "Wrong glob style pattern: " <> arg
        Just gp -> check arg (globKeysMetaData gp)

------------------------------------------------------------------------------
