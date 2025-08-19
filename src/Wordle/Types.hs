module Wordle.Types where

import Data.Text (Text)


data LetterResult 
  = Gray    -- ^ Letter is not in the word at all
  | Yellow  -- ^ Letter is in the word but wrong position  
  | Green   -- ^ Letter is in the correct position
  deriving (Show, Eq)


data GuessResult = GuessResult
  { guessWord :: Text           -- ^ The word that was guessed
  , letterResults :: [LetterResult]  -- ^ Color for each letter
  } deriving (Show, Eq)


data Difficulty 
  = Easy    -- ^ Warns about invalid moves
  | Normal  -- ^ Standard Wordle rules
  | Expert  -- ^ Can lie once
  deriving (Show, Eq)


data GameMode
  = PlayerGuesses  
  | ComputerGuesses 
  deriving (Show, Eq)