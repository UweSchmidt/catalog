----------------------------------------
--
-- option parsers for Host and Port

module Options
  ( serverOptions )
where

import Options.Applicative
import Options.Applicative.LogLevel
import Options.Applicative.CatEnv
import Options.Applicative.HostPort

import Catalog.Version
import Catalog.CatEnv
import Data.Prim

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
  where
    setEnv ll po ja mp jo fu sx =
      defaultAppEnv
      & appEnvLogLevel .~ ll
      & appEnvJournal  .~ jo
      & appEnvPort     .~ po
      & appEnvCat . catMountPath   .~ mp
      & appEnvCat . catJsonArchive .~ ja
      & appEnvCat . catForceMDU    .~ fu
      & appEnvCat . catSaveBothIx  .~ sx

----------------------------------------
