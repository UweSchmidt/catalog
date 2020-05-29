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
import Polysemy.Logging
import Polysemy.HttpRequest.SimpleRequests

-- client-polysemy
import Catalog.Effects.CatCmd.ClientInterpreter
import Client.Effects.ClientCmd
import Client.Effects.ClientCmd.Interpreter
import Client.Options

import System.Exit

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
  ((hostPort, loglev), cmd) <- clientAction

  man <- newBasicManager
  res <- runM
         . writelnToStdout          -- Consume Text
         . logToStdErr              -- Consume LogMsg
         . logWithLevel             -- Logging
             loglev
         . runError  @Text          -- Error Text
         . runReader @HostPort      -- Reader HostPort
             hostPort
         . basicFileSystem          -- FileStatus (file system calls)
             ioExcToText
         . basicHttpRequests        -- HttpRequest
             httpExcToText
             man
         . evalClientCatCmd         -- CatCmd     (server calls)
         . evalClientCmd            -- ClientCmd  (client commands)
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


----------------------------------------
