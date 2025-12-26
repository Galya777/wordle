-- Basic types for the wordle game
module Wordle.Types where

-- The three colors for each letter
data LetterResult 
  = Gray 
  | Yellow
  | Green
  deriving (Show, Eq)

-- A guess and its results
data GuessResult = GuessResult
  { guessWord :: String
  , results :: [LetterResult]
  } deriving (Show, Eq)

data Difficulty = Easy | Normal | Expert
  deriving (Show, Eq)

data GameMode = PlayerGuesses | ComputerGuesses
  deriving (Show, Eq)
