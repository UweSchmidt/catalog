----------------------------------------
--
-- option parsers for Host and Port

module Options
  ( serverOptions )
where

import Options.Applicative
       ( Parser
       , ParserInfo
       , fullDesc
       , header
       , info
       , progDesc
       , execParser
       , helper
       )
import Options.Applicative.LogLevel
       ( optLogLevel )

import Options.Applicative.CatEnv
       ( optForceMDU
       , optGPSCache
       , optJournal
       , optJsonArchive
       , optMountPath
       , optSaveBothIx
       , optNoSync
       )
import Options.Applicative.HostPort
       ( optPort )

import Catalog.CatEnv
       ( CatEnv
       , catForceMDU
       , catGPSCache
       , catJsonArchive
       , catMountPath
       , catJournal
       , catSaveBothIx
       , catNoSync
       , catHost
       , catPort
       , catLogLevel
       , catVersion
       , catVersDate
       , defaultCatEnv
       )
import Data.Prim.Prelude

import Network.HostName
       ( getHostName )

----------------------------------------

serverOptions :: IO CatEnv
serverOptions = do
  host <- getHostName
  execParser (appInfo (host ^. isoText))

appInfo :: Text -> ParserInfo CatEnv
appInfo host =
  info (catEnvParser host <**> helper)
  ( fullDesc
    <> progDesc "organize your photos"
    <> header ( ("servant-polysemy" <> " - "
                 <> (defaultCatEnv ^. catVersion)
                 <> " (" <> (defaultCatEnv ^. catVersDate) <> ")"
                ) ^. isoString
              )
  )

catEnvParser :: Text -> Parser CatEnv
catEnvParser hs =
  setEnv
  <$> optLogLevel
  <*> optPort
  <*> optJsonArchive
  <*> optMountPath
  <*> optJournal
  <*> optForceMDU
  <*> optSaveBothIx
  <*> optNoSync
  <*> optGPSCache
  where
    setEnv ll po ja mp jo fu sx ns gc =
      defaultCatEnv
      & catMountPath   .~ mp
      & catJsonArchive .~ ja
      & catJournal     .~ jo
      & catForceMDU    .~ fu
      & catSaveBothIx  .~ sx
      & catNoSync      .~ ns
      & catGPSCache    .~ gc
      & catHost        .~ hs
      & catPort        .~ po
      & catLogLevel    .~ ll

----------------------------------------
