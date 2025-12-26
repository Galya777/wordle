module Wordle.Monad where

import Wordle.Types

-- Game state
data GameState = GameState
  { secretWord :: String
  , guesses :: [GuessResult]
  , maxGuesses :: Int
  , currentDifficulty :: Difficulty
  } deriving (Show, Eq)

-- Function for new state
novoGameState :: String -> Difficulty -> GameState
novoGameState duma dif = GameState
  { secretWord = duma
  , guesses = []
  , maxGuesses = 6
  , currentDifficulty = dif
  }
