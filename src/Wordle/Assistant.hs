-- Assistant Mode: Computer tries to guess your word
module Wordle.Assistant where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sortBy, nub)
import Data.Ord (comparing)
import System.Random
import Wordle.Types
import Wordle.Monad
import Wordle.ColorLogic
import Wordle.Game (printMessage, addGuess)

-- Assistant's knowledge about the word
data AssistantKnowledge = AssistantKnowledge
  { knownPositions :: [Maybe Char]      -- What letters we know are in each position
  , mustContain :: [Char]               -- Letters that must be in the word (from Yellow)
  , cannotContain :: [Char]             -- Letters that are definitely not in the word (from Gray)
  , positionConstraints :: [(Int, [Char])] -- Position -> letters that CAN'T be there (from Yellow)
  } deriving (Show, Eq)

-- Initial empty knowledge
emptyKnowledge :: AssistantKnowledge
emptyKnowledge = AssistantKnowledge
  { knownPositions = replicate 5 Nothing
  , mustContain = []
  , cannotContain = []
  , positionConstraints = []
  }

-- Update knowledge based on guess result
updateKnowledge :: Text -> [LetterResult] -> AssistantKnowledge -> AssistantKnowledge
updateKnowledge guess results knowledge = 
  let guessChars = T.unpack guess
      updates = zip3 guessChars results [0..]
  in foldl processUpdate knowledge updates
  where
    processUpdate k (char, result, pos) = case result of
      Green -> k { knownPositions = updatePosition pos char (knownPositions k) }
      Yellow -> k { mustContain = addUnique char (mustContain k)
                  , positionConstraints = addConstraint pos char (positionConstraints k) }
      Gray -> k { cannotContain = addUnique char (cannotContain k) }

-- Helper functions
updatePosition :: Int -> Char -> [Maybe Char] -> [Maybe Char]
updatePosition pos char positions = 
  take pos positions ++ [Just char] ++ drop (pos + 1) positions

addUnique :: Eq a => a -> [a] -> [a]
addUnique x xs = if x `elem` xs then xs else x : xs

addConstraint :: Int -> Char -> [(Int, [Char])] -> [(Int, [Char])]
addConstraint pos char constraints = 
  case lookup pos constraints of
    Nothing -> (pos, [char]) : constraints
    Just chars -> (pos, addUnique char chars) : filter ((/= pos) . fst) constraints

-- Check if a word satisfies our current knowledge
satisfiesKnowledge :: AssistantKnowledge -> String -> Bool
satisfiesKnowledge knowledge word = 
  length word == 5 &&
  satisfiesPositions word &&
  satisfiesMustContain word &&
  satisfiesCannotContain word &&
  satisfiesPositionConstraints word
  where
    satisfiesPositions w = all checkPosition (zip [0..] w)
      where
        checkPosition (pos, char) = case knownPositions knowledge !! pos of
          Nothing -> True
          Just knownChar -> char == knownChar
    
    satisfiesMustContain w = all (`elem` w) (mustContain knowledge)
    
    satisfiesCannotContain w = not (any (`elem` w) (cannotContain knowledge))
    
    satisfiesPositionConstraints w = all checkConstraint (positionConstraints knowledge)
      where
        checkConstraint (pos, bannedChars) = not (w !! pos `elem` bannedChars)

-- Filter word list based on current knowledge
filterCandidates :: AssistantKnowledge -> [Text] -> [Text]
filterCandidates knowledge wordList = 
  filter (satisfiesKnowledge knowledge . T.unpack) wordList

-- Score words based on how good they are as guesses
-- Higher score = better guess
scoreWord :: [Text] -> String -> Int
scoreWord remainingWords word = 
  let letterFreqs = calculateLetterFrequencies remainingWords
      uniqueLetters = nub word
      letterScores = map (\c -> maybe 0 id (lookup c letterFreqs)) uniqueLetters
  in sum letterScores

-- Calculate letter frequencies in remaining candidate words
calculateLetterFrequencies :: [Text] -> [(Char, Int)]
calculateLetterFrequencies words = 
  let allChars = concatMap T.unpack words
      uniqueChars = nub allChars
  in map (\c -> (c, length (filter (== c) allChars))) uniqueChars

-- Choose the best next guess
chooseBestGuess :: AssistantKnowledge -> [Text] -> WordleM Text
chooseBestGuess knowledge wordList = do
  let candidates = filterCandidates knowledge wordList
  
  if null candidates
    then throwError (InvalidInput (T.pack "No valid words found! Something went wrong."))
    else if length candidates == 1
      then return (head candidates)  -- Only one possibility left
      else do
        -- Score all candidates and pick the best one
        let scoredCandidates = map (\w -> (w, scoreWord candidates (T.unpack w))) candidates
        let sortedCandidates = sortBy (comparing (negate . snd)) scoredCandidates
        let bestGuess = fst (head sortedCandidates)
        
        printMessage $ "🤖 I'm thinking... " ++ show (length candidates) ++ " possibilities remain."
        printMessage $ "🎯 My best guess based on letter frequency analysis..."
        
        return bestGuess

-- Assistant game loop
playAssistantGame :: WordleM ()
playAssistantGame = do
  state <- get
  let attemptsUsed = length (guesses state)
  if attemptsUsed >= maxGuesses state
    then do
      printMessage "🤖 I'm out of guesses! You win! 🎉"
      printMessage $ "The word was: " ++ T.unpack (secretWord state)
    else do
      -- Check if we've won
      case guesses state of
        (lastGuess:_) | all (== Green) (letterResults lastGuess) -> do
          printMessage $ "🤖 I got it! The word is: " ++ T.unpack (guessWord lastGuess)
          printMessage "🎉 Computer wins!"
        _ -> do
          -- Make next guess
          config <- ask
          let knowledge = extractKnowledge (guesses state)
          
          guess <- chooseBestGuess knowledge (wordList config)
          printMessage $ "🤖 My guess: " ++ T.unpack guess
          
          -- Get feedback from human
          feedback <- getHumanFeedback guess
          
          -- Process the feedback
          let guessResult = GuessResult guess feedback
          addGuess guessResult
          
          -- Continue the game
          playAssistantGame

-- Extract knowledge from previous guesses
extractKnowledge :: [GuessResult] -> AssistantKnowledge
extractKnowledge guesses = foldl updateFromGuess emptyKnowledge (reverse guesses)
  where
    updateFromGuess knowledge guessResult = 
      updateKnowledge (guessWord guessResult) (letterResults guessResult) knowledge

-- Get feedback from the human player
getHumanFeedback :: Text -> WordleM [LetterResult]
getHumanFeedback guess = do
  printMessage "Please provide feedback for each letter:"
  printMessage "G = Green (correct position), Y = Yellow (wrong position), R = Gray (not in word)"
  printMessage $ "For the word: " ++ T.unpack guess
  
  feedback <- liftIO $ do
    putStr "Enter feedback (e.g., GYRGG): "
    input <- getLine
    return $ map parseColor (take 5 input)
  
  if length feedback /= 5
    then do
      printMessage "Please enter exactly 5 letters (G/Y/R)"
      getHumanFeedback guess
    else return feedback
  where
    parseColor 'G' = Green
    parseColor 'g' = Green
    parseColor 'Y' = Yellow
    parseColor 'y' = Yellow
    parseColor 'R' = Gray
    parseColor 'r' = Gray
    parseColor _ = Gray  -- Default to Gray for invalid input