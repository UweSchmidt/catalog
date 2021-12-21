----------------------------------------------------------------

module Logger
  ( withCatLogger )
where

import Data.Prim.Prelude
       ( (^.)
       , IsoString(isoString)
       , IsoText(isoText)
       , Text
       , void
       , to
       )

import Control.Exception
       ( bracket )

import Network.Wai.Logger
       ( ApacheLogger
       , IPAddrSource(..)
       , LogType'(..)
       , initLogger
       , apacheLogger
       , logRemover
       )

import System.Log.FastLogger
       ( LogStr
       , newTimeCache
       , simpleTimeFormat
       , fromLogStr
       )

import qualified Data.Text as T

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
logStrToText out ls =
  out $ fromLogStr ls ^. isoString . isoText . to (T.dropEnd 1) -- rem \n

logFlush :: IO ()
logFlush = return ()

----------------------------------------------------------------
