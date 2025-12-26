module Wordle.Monad where

import Wordle.Types

-- Sustoianie na igrata
data GameState = GameState
  { secretWord :: String            
  , guesses :: [GuessResult]        
  , maxGuesses :: Int               
  , currentDifficulty :: Difficulty        
  } deriving (Show, Eq)

-- Funkcia za novo sustoianie
novoGameState :: String -> Difficulty -> GameState
novoGameState duma dif = GameState
  { secretWord = duma
  , guesses = []
  , maxGuesses = 6
  , currentDifficulty = dif
  }
