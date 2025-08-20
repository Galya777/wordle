-- Basic types for my Wordle game
module Wordle.Types where

import Data.Text (Text)

-- The three colors we show for each letter
data LetterResult 
  = Gray    -- not in word
  | Yellow  -- in word, wrong spot
  | Green   -- correct spot
  deriving (Show, Eq)


-- One guess with its colors
data GuessResult = GuessResult
  { guessWord :: Text           
  , letterResults :: [LetterResult]  
  } deriving (Show, Eq)

-- Game difficulty levels
data Difficulty 
  = Easy    -- helps with warnings
  | Normal  -- regular wordle
  | Expert  -- can lie once
  deriving (Show, Eq)

-- Who is guessing?
data GameMode
  = PlayerGuesses  
  | ComputerGuesses 
  deriving (Show, Eq)