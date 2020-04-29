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
import Polysemy.Input
import Polysemy.State
import Polysemy.Error

-- polysemy-useful-stuff
import Polysemy.Consume
import Polysemy.Consume.BGQueue
import Polysemy.HttpRequest
import Polysemy.Logging

-- old stuff
import System.Random (newStdGen, randomRs, randomRIO)
import Text.Read (readMaybe)

-- basic stuff
import Data.Text (Text)

import qualified Data.Text as T

----------------------------------------

trace :: Member Logging r => String -> Sem r ()
trace = log'verb . T.pack

----------------------------------------

newtype GuessConfig = GuessConfig { unGuessConfig :: (Int, (Int, Int)) }
  deriving Show

runListInputForever :: [i] -> Sem (Input i ': r) a -> Sem r a
runListInputForever is =
  fmap snd
    . runState (cycle is)
    . reinterpret
      (\ c -> case c of
          Input -> do
            gotten <- get
            let (s : ss) = gotten   -- safe because the state is infinite
            put ss
            pure s
      )


guessProg :: forall r. Members '[Input String, Input Int, Logging] r
          => GuessConfig -> Sem r [Bool]
guessProg config = go True []
  where
    -- Note: Because of ScopedTypeVariables,
    -- r and its constraints are available in the function body.
    go :: Bool -> [Bool] -> Sem r [Bool]
    go False record = return $ reverse record
    go True record = do
      let GuessConfig (guessesallowed, (minguess, maxguess)) = config
      trace ""
      trace $ "I'm thinking of a number between "
              <> show minguess
              <> " and "
              <> show maxguess
              <> ". Guesses allowed: "
              <> show guessesallowed
      correct <- input
      let
        doGuess :: Int -> Sem r (Maybe Int)
        doGuess remaining = do
          let guessnum = (guessesallowed - remaining) + 1
          trace $ "Enter guess #" <> show guessnum <> ":"
          maybeguess <- readMaybe <$> input
          case maybeguess of
            Just guess
              | guess > correct -> trace $ show guess <> " is too high."
              | guess < correct -> trace $ show guess <> " is too low."
              | otherwise -> return ()
            Nothing -> trace "That's not a valid number. You just wasted a guess!"
          return maybeguess

        doGuesses :: Int -> Sem r Bool
        doGuesses (-1) = trace "You win!" >> return True
        doGuesses 0 = do
          trace $ "You ran out of guesses. Game over! The number was "
                  <> show correct <> "."
          return False
        doGuesses remaining = do
          maybeguess <- doGuess remaining
          doGuesses $ if maybeguess == Just correct then (-1) else remaining - 1

        playAgain :: Sem r Bool
        playAgain = do
          trace ""
          trace "Play again? (Y/n)"
          (`elem` ["y", "yes", "hokay", "" :: String]) <$> input

      won <- doGuesses guessesallowed
      again <- playAgain
      trace $ if again
              then "Starting a new game!"
              else "Goodbye!"
      go again (won:record)


main :: IO ()
main = do
  rgen <- newStdGen
  let
    guessesallowed = 5
    guessmin = 1
    guessmax = 100
    bounds = (guessmin, guessmax)
    conf = GuessConfig (guessesallowed, bounds)
    randlist = randomRs (guessmin, guessmax) rgen
    inputlist = ["50", "75", "", "", "", "y","75"] :: [String]
    (progoutput :: [Text], progreturn)
            = run
            . runConsumeMonoid ((:[]) . logMsgToText)
            . logWithoutLevel
            . runListInputForever randlist
            . runListInputForever inputlist
            $ guessProg conf

  putStrLn "====== Result from pure program"
  mapM_ print progoutput
  putStrLn $ "Successful completion with result: " <> show progreturn

  putStrLn "\n"
  putStrLn "====== Running IO program"
  iresult <- runM
           . logToStdErr
           . logWithLevel LogDbg
           . runInputSem (embed $ randomRIO bounds)
           . runInputSem (embed getLine)
           $ guessProg conf
  putStrLn "====== Result from IO program"
  putStrLn $ "Completion with result: " <> show iresult

  putStrLn "\n"
  putStrLn "====== Running IO with BGQueue for output"

  qu <- startBGQueue
  iresult2 <- runM
           . logToBGQueue qu
           . logWithLevel LogDbg
           . runInputSem (embed $ randomRIO bounds)
           . runInputSem (embed getLine)
           $ guessProg conf
  putStrLn "====== Result from IO with BGQueue"
  putStrLn $ "Completion with result: " <> show iresult2

----------------------------------------

logme :: ( Member Logging r
         , Member (Error Text) r
         )
      => Sem r ()
logme = log'err "Mist" >> throw @Text "Kacke"

runLogme :: IO (Either Text ())
runLogme =
  runM
  . logToStdErr
  . logWithLevel LogDbg
  . runError
  $ logme

----------------------------------------
