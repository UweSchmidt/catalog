----------------------------------------
--
-- option parsers for Host and Port

module Options.Applicative.CatEnv
  ( optForceMDU
  , optJournal
  , optJsonArchive
  , optMountPath
  , optSaveBothIx
  , optGPSCache
  )
where

import Options.Applicative
import Data.Text (Text)

import qualified Data.Text as T

----------------------------------------

optJournal :: Parser (Maybe Text)
optJournal =
  toM <$>
  strOption
  ( long "journal"
    <> short 'j'
    <> metavar "JOURNAL"
    <> value ""
    <> help ( "Write journal of archive changes,"
              <> " \"1\" output to stdout, \"2\" to stderr,"
              <> " else to journal file."
            )
  )
  where
    toM :: String -> Maybe Text
    toM "" = Nothing
    toM x  = Just $ T.pack x

optMountPath :: Parser Text
optMountPath =
  T.pack <$>
  ( strOption
    ( long "mount-path"
      <> short 'm'
      <> metavar "MOUNT-PATH"
      <> showDefault
      <> value "."
      <> help "The mount path for the whole archive"
    )
  )

optJsonArchive :: Parser Text
optJsonArchive =
  T.pack <$>
  ( strOption
    ( long "archive"
      <> short 'a'
      <> metavar "ARCHIVE"
      <> showDefault
      <> value "catalog.json"
      <> help "The JSON archive file to be loaded, relative to mount path"
    )
  )

optGPSCache :: Parser Text
optGPSCache =
  T.pack <$>
  ( strOption
    ( long "gps-cache"
      <> metavar "CACHE-FILE"
      <> showDefault
      <> value "gps-cache.json"
      <> help ( "The JSON cache for storing GPS loc to address data."
                <> " This file will be stored in the same directory as the"
                <> " catalog ARCHIVE."
              )
    )
  )

optSaveBothIx :: Parser Bool
optSaveBothIx =
  switch
  ( long "save-hash-and-path-ix"
    <> short 'b'
    <> help "Save both hash and path index version of catalog"
  )

optForceMDU :: Parser Bool
optForceMDU =
  switch
  ( long "force-metadata-update"
    <> short 'f'
    <> help ( "Force metadata update when syncing catalog with image archive"
            <> " (recompute EXIF data for all images)"
            )
  )

----------------------------------------
