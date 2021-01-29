----------------------------------------
--
-- option parsers for Host and Port

module Options.Applicative.CatEnv
  ( optForceMDU
  , optJournal
  , optJsonArchive
  , optMountPath
  , optSaveBothIx
  )
where

import Options.Applicative
import Data.Text (Text)

import qualified Data.Text as T

----------------------------------------

optJournal :: Parser (Maybe Text)
optJournal =
  Just . T.pack <$>
  strOption
  ( long "journal"
    <> short 'j'
    <> metavar "JOURNAL"
    <> value "-"
    <> help "Write journal of archive changes to file. Default is stdout."
  )

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
