-- Interactive game where human can play
module Wordle.Interactive where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (toUpper)
import Wordle.Types
import Wordle.Monad
import Wordle.Game

-- Check if game is over (won or lost)
isGameOver :: WordleM Bool
isGameOver = do
  state <- get
  let numGuesses = length (guesses state)
  let maxAllowed = maxGuesses state
  
  -- Check if we won (last guess was all green)
  case guesses state of
    [] -> return False  -- no guesses yet
    (lastGuess:_) -> do
      let won = all (== Green) (letterResults lastGuess)
      let lost = numGuesses >= maxAllowed
      return (won || lost)

-- Get user input and convert to uppercase Text
getUserGuess :: WordleM Text
getUserGuess = do
  liftIO $ putStr "Enter your guess (or 'quit' to exit): "
  input <- liftIO getLine
  let upperInput = T.pack $ map toUpper input
  if upperInput == T.pack "QUIT"
    then throwError PlayerQuit  -- Player chose to quit
    else return upperInput

-- Main game loop
playGame :: WordleM ()
playGame = do
  gameOver <- isGameOver
  if gameOver
    then do
      state <- get
      case guesses state of
        [] -> printMessage "No guesses made yet"
        (lastGuess:_) -> do
          let won = all (== Green) (letterResults lastGuess)
          if won
            then printMessage "Congratulations! You won!"
            else do
              printMessage "Game over! The word was:"
              printMessage $ T.unpack (secretWord state)
    else do
      -- Get user's guess
      guess <- getUserGuess
      
      -- Try to process the guess
      result <- checkGuess guess `catchError` handleError
      
      -- Continue the game
      playGame
  where
    handleError err = do
      printMessage $ "Error: " ++ show err
      return $ GuessResult (T.pack "") []  -- dummy result

-- Get difficulty choice from user
chooseDifficulty :: IO Difficulty
chooseDifficulty = do
  putStrLn "Choose difficulty:"
  putStrLn "1. Easy (helpful hints)"
  putStrLn "2. Normal (standard Wordle)"
  putStrLn "3. Expert (I might lie once!)"
  putStr "Enter choice (1-3): "
  choice <- getLine
  case choice of
    "1" -> return Easy
    "2" -> return Normal
    "3" -> return Expert
    _ -> do
      putStrLn "Invalid choice, using Normal difficulty."
      return Normal

-- Start a new game
startGame :: [Text] -> Text -> IO ()
startGame wordList secret = do
  difficulty <- chooseDifficulty
  let config = GameConfig wordList 5
  let initialState = GameState secret [] 6 difficulty False
  
  result <- runWordleM initialState config $ do
    printMessage "Welcome to Wordle!"
    printMessage $ "Difficulty: " ++ show difficulty
    printMessage "Guess the 5-letter word. You have 6 attempts."
    printMessage ""
    playGame
    
  case result of
    Left PlayerQuit -> putStrLn "Thanks for playing!"
    Left err -> putStrLn $ "Game error: " ++ show err
    Right _ -> putStrLn "Thanks for playing!"