-- Test game with known secret word to demonstrate color logic
module Wordle.TestGame where

import Wordle.Types
import Wordle.Monad
import Wordle.Game
import Wordle.ColorLogic
import Data.Text (Text)
import qualified Data.Text as T

-- Test the game with a known secret word
testGameWithSecret :: Text -> [Text] -> [Text] -> IO ()
testGameWithSecret secret wordList guesses = do
  let config = GameConfig wordList 5
  let initialState = GameState secret [] 6 Normal False
  
  putStrLn $ "Testing with secret word: " ++ T.unpack secret
  putStrLn "Guesses and results:"
  
  result <- runWordleM initialState config $ do
    mapM_ testGuess guesses
    
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right _ -> putStrLn "Test completed!"
  where
    testGuess guess = do
      printMessage $ "\nGuess: " ++ T.unpack guess
      guessResult <- checkGuess guess
      printMessage $ "Result: " ++ show (letterResults guessResult)
      return guessResult

-- Run a demonstration
runDemo :: IO ()
runDemo = do
  putStrLn "=== Color Logic Demo in Real Game ==="
  
  let wordList = [T.pack "HELLO", T.pack "LLAMA", T.pack "SPEED", T.pack "ERASE", T.pack "ARRAY"]
  
  -- Test case that shows duplicate letter handling
  testGameWithSecret (T.pack "HELLO") wordList [T.pack "LLAMA"]