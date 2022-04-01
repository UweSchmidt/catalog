module Main where

-- polysemy
import Polysemy
import Polysemy.Internal (send)
import Polysemy.Error
import Polysemy.Reader

-- polysemy-useful-stuff
import Polysemy.Consume
import Polysemy.FileSystem
import Polysemy.HttpRequest
import Polysemy.HttpRequest.SimpleRequests
import Polysemy.Logging

-- client-polysemy
import Catalog.Effects.CatCmd.ClientInterpreter
                                 ( evalClientCatCmd )
import Client.Effects.ClientCmd.Interpreter
                                 ( evalClientCmd )
import Client.Options            ( clientAction )
import GPS.Effects.GeoLocCmd     ( runReverseGeoLoc )

import Network.HTTP.Client       ( responseTimeoutNone )
import System.Exit               ( ExitCode(..)
                                 , exitWith
                                 )

import Data.Prim.Prelude

import qualified Data.Text       as T

----------------------------------------
--
-- | The main program
--
-- build client command from command line args
-- collect and configure all neccesary effect handlers
-- start the main action (send cmd)
-- report all errors
-- and terminate with exit code

main :: IO ()
main = do
  (((h, p), loglev), cmd) <- clientAction

  req <- catalogServerRequest h p

  res <- runM
         . writelnToStdout          -- Consume Text
         . logToStdErr              -- Consume LogMsg
         . logWithLevel             -- Logging
             loglev
         . runError  @Text          -- Error Text
         . runReader @Request       -- Reader Request
             req
         . basicFileSystem          -- FileStatus  (file system calls)
             ioExcToText
         . simpleHttpRequests       -- HttpRequest (catalog server, ...)
         . runReverseGeoLoc         -- Cached GeoLocCmd (HTTPs to openstreetmap)
         . evalClientCatCmd         -- CatCmd      (server calls)
         . evalClientCmd            -- ClientCmd   (client commands)
         $ do
             send cmd                    -- start action
               `catch`
               (\ msg -> do log'err msg  -- log all errors
                            throw msg    -- rethrow error
               )

  exitWith $
    either (const $ ExitFailure 1)
           (const   ExitSuccess)
           res

-- --------------------
--
-- create a Request for calling
-- catalog server at
-- "http://<host>:<port>/index.html"
-- the path component, here "index.html" will be overwritten
-- before executing a request to the catalog server
--
-- local requests, e.g. file system imports
-- may run a long time, so timeouts are disabled

catalogServerRequest :: Text -> Int -> IO Request
catalogServerRequest h p = do
  req <- parseRequest $
         "http://" ++ T.unpack h ++ ":" ++ show p ++ "/index.html"

  -- for requests to catalog server
  -- timeouts are disabled
  return $ req {responseTimeout = responseTimeoutNone}

----------------------------------------
