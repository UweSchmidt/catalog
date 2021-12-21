----------------------------------------
--
-- option parsers for Host and Port

module Options
  ( serverOptions )
where

import Options.Applicative
       ( (<**>)
       , Parser
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
       , appEnvJournal
       , appEnvLogLevel
       , appEnvPort
       , catForceMDU
       , catGPSCache
       , catJsonArchive
       , catMountPath
       , catSaveBothIx
       , defaultAppEnv
       )
import Data.Prim
       ( (&)
       , (.~)
       )

----------------------------------------

serverOptions :: IO AppEnv
serverOptions = execParser appInfo

appInfo :: ParserInfo AppEnv
appInfo =
  info (appEnvParser <**> helper)
  ( fullDesc
    <> progDesc "organize your photos"
    <> header ("servant-polysemy" ++ " - " ++ version ++ " (" ++ date ++ ")")
  )

appEnvParser :: Parser AppEnv
appEnvParser =
  setEnv
  <$> optLogLevel
  <*> optPort
  <*> optJsonArchive
  <*> optMountPath
  <*> optJournal
  <*> optForceMDU
  <*> optSaveBothIx
  <*> optGPSCache
  where
    setEnv ll po ja mp jo fu sx gc =
      defaultAppEnv
      & appEnvLogLevel .~ ll
      & appEnvJournal  .~ jo
      & appEnvPort     .~ po
      & appEnvCat . catMountPath   .~ mp
      & appEnvCat . catJsonArchive .~ ja
      & appEnvCat . catForceMDU    .~ fu
      & appEnvCat . catSaveBothIx  .~ sx
      & appEnvCat . catGPSCache    .~ gc

----------------------------------------
