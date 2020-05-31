module Logger
  ( withCatLogger )
where

import Data.Prim.Prelude

import Control.Exception     ( bracket )
import Network.Wai.Logger    ( ApacheLogger
                             , IPAddrSource(..)
                             , LogType'(..)
                             , initLogger
                             , apacheLogger
                             , logRemover
                             )

import System.Log.FastLogger ( LogStr
                             , newTimeCache
                             , simpleTimeFormat
                             , fromLogStr
                             )


----------------------------------------------------------------

withCatLogger :: (Text -> IO ())
             -> (ApacheLogger -> IO a)
             -> IO a
withCatLogger out app = bracket setup teardown $ \(aplogger, _) ->
    app aplogger
  where
    setup = do
        tgetter <- newTimeCache simpleTimeFormat
        apf <- initLogger
               FromFallback
               (LogCallback (logStrToText out) logFlush)
               tgetter
        let aplogger = apacheLogger apf
            remover  = logRemover apf
        return (aplogger, remover)
    teardown (_, remover) = void remover

----------------------------------------------------------------

logStrToText :: (Text -> IO ()) -> LogStr -> IO ()
logStrToText out ls = out $ fromLogStr ls ^. isoString . isoText

logFlush :: IO ()
logFlush = return ()

----------------------------------------------------------------
