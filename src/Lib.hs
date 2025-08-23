module Lib
    ( someFunc
    ) where

import Wordle.Types
import Wordle.Monad
import Wordle.Game
import Wordle.Interactive
import Wordle.WordList
import Wordle.ColorLogic
import Data.Text (Text)
import qualified Data.Text as T

someFunc :: IO ()
someFunc = do
  putStrLn "Loading word list..."
  
  -- Load words from file
  wordList <- loadWordList "words"
  putStrLn $ "Loaded " ++ show (length wordList) ++ " words"
  
  -- Pick a random secret word
  secretWord <- pickRandomWord wordList
  putStrLn "Secret word selected! Good luck!"
  putStrLn "(Now using improved color logic that handles duplicate letters correctly!)"
  
  -- Start the interactive game!
  startGame wordList secretWord

-- Function to test color logic if needed
testColorLogic :: IO ()
testColorLogic = do
  putStrLn "=== Testing Color Logic ==="
  runTests