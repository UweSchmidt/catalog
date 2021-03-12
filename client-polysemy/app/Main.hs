{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}


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
import Client.Effects.ClientCmd
import Client.Effects.ClientCmd.Interpreter
import Client.Options
import GPS.Effects.GeoLocCmd

import Network.HTTP.Client
       ( Request(..)
       , responseTimeoutNone
       )
import System.Exit

import qualified Data.Text as T

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
         . runError  @ Text         -- Error Text
         . runReader @ Request      -- Reader Request
             req
         . basicFileSystem          -- FileStatus  (file system calls)
             ioExcToText
         . simpleHttpRequests       -- HttpRequest (catalog server, ...)
         . nominatimHttps           -- GeoLocCmd   (HTTPs to openstreetmap)
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

catalogServerRequest :: Text -> Int -> IO Request
catalogServerRequest h p = do
  req <- parseRequest $
         "http://" ++ T.unpack h ++ ":" ++ show p ++ "/index.html"

  -- for requests to catalog server
  -- timeouts are disabled
  return $ req {responseTimeout = responseTimeoutNone}

----------------------------------------
