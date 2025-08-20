-- Core game logic - learning to use the monad stack
module Wordle.Game where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (toUpper)
import Wordle.Types
import Wordle.Monad

-- Let me try to understand how to use the different parts of WordleM

-- Getting the current game state
getCurrentGuesses :: WordleM [GuessResult]
getCurrentGuesses = do
  gameState <- get  -- get the current state
  return (guesses gameState)

-- Getting info from config (that never changes)
getWordList :: WordleM [Text]
getWordList = do
  config <- ask  -- ask for the config
  return (wordList config)

-- Modifying the game state
addGuess :: GuessResult -> WordleM ()
addGuess newGuess = do
  modify $ \state -> state { guesses = newGuess : guesses state }

-- Throwing an error when something goes wrong
checkValidWord :: Text -> WordleM ()
checkValidWord word = do
  wordList <- getWordList
  if word `elem` wordList
    then return ()  -- word is valid, do nothing
    else throwError (InvalidWord word)  -- throw error

-- Doing IO operations (like printing)
printMessage :: String -> WordleM ()
printMessage msg = liftIO $ putStrLn msg

-- Let's create the core function that checks a guess
-- This is the heart of Wordle - comparing guess to secret word
checkGuess :: Text -> WordleM GuessResult
checkGuess guess = do
  state <- get
  let secret = secretWord state
  
  -- Make sure word is valid first
  checkValidWord guess
  
  -- Calculate the colors for each letter
  let results = calculateColors (T.unpack guess) (T.unpack secret)
  let guessResult = GuessResult guess results
  
  -- Add this guess to our state
  addGuess guessResult
  
  -- Print what happened
  printMessage $ "Guess: " ++ T.unpack guess
  printMessage $ "Result: " ++ show results
  
  return guessResult

-- Helper function to calculate colors
-- This is tricky logic - let me work through it step by step
calculateColors :: String -> String -> [LetterResult]
calculateColors guess secret = zipWith checkLetter guess [0..]
  where
    checkLetter guessChar pos
      | guessChar == (secret !! pos) = Green  -- exact match
      | guessChar `elem` secret = Yellow       -- in word, wrong spot
      | otherwise = Gray                       -- not in word