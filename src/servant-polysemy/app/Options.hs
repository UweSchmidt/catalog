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

import Catalog.Version
       ( date
       , version
       )
import Catalog.CatEnv
       ( AppEnv
       , appEnvCat
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
       , defaultAppEnv
       )
import Data.Prim.Prelude

import Network.HostName
       ( getHostName )

----------------------------------------

serverOptions :: IO AppEnv
serverOptions = do
  host <- getHostName
  execParser (appInfo (host ^. isoText))

appInfo :: Text -> ParserInfo AppEnv
appInfo host =
  info (appEnvParser host <**> helper)
  ( fullDesc
    <> progDesc "organize your photos"
    <> header ("servant-polysemy" ++ " - " ++ version ++ " (" ++ date ++ ")")
  )

appEnvParser :: Text -> Parser AppEnv
appEnvParser hs =
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
      defaultAppEnv
      & appEnvCat . catMountPath   .~ mp
      & appEnvCat . catJsonArchive .~ ja
      & appEnvCat . catJournal     .~ jo
      & appEnvCat . catForceMDU    .~ fu
      & appEnvCat . catSaveBothIx  .~ sx
      & appEnvCat . catNoSync      .~ ns
      & appEnvCat . catGPSCache    .~ gc
      & appEnvCat . catHost        .~ hs
      & appEnvCat . catPort        .~ po
      & appEnvCat . catLogLevel    .~ ll

----------------------------------------
