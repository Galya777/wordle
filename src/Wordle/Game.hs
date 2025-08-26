-- Core game logic - learning to use the monad stack
module Wordle.Game where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (toUpper)
import System.Random
import Wordle.Types
import Wordle.Monad
import Wordle.ColorLogic

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
  let diff = difficulty state
  
  -- Make sure word is valid first
  checkValidWord guess
  
  -- Calculate the colors for each letter (using correct logic)
  let results = calculateColorsCorrect (T.unpack guess) (T.unpack secret)
  
  -- Apply difficulty-specific modifications
  finalResults <- applyDifficultyRules results guess secret diff
  
  let guessResult = GuessResult guess finalResults
  
  -- Add this guess to our state
  addGuess guessResult
  
  -- Print what happened
  printMessage $ "Guess: " ++ T.unpack guess
  printMessage $ "Result: " ++ show finalResults
  
  -- Easy mode: give helpful warnings
  when (diff == Easy) $ giveEasyModeHints finalResults guess
  
  return guessResult

-- Note: We now use calculateColorsCorrect from Wordle.ColorLogic
-- which properly handles duplicate letters!

-- Apply difficulty-specific rules to the results
applyDifficultyRules :: [LetterResult] -> Text -> Text -> Difficulty -> WordleM [LetterResult]
applyDifficultyRules results guess secret diff = case diff of
  Easy -> return results    -- Easy mode: no changes, just helpful hints
  Normal -> return results  -- Normal mode: standard Wordle rules
  Expert -> expertModifyResults results  -- Expert mode: can lie once per game

-- Expert mode: randomly lie about one result (but only once per game)
expertModifyResults :: [LetterResult] -> WordleM [LetterResult]
expertModifyResults results = do
  state <- get
  if hasLied state
    then return results  -- already lied once, play fair now
    else do
      -- Decide randomly whether to lie this turn (30% chance)
      shouldLie <- liftIO $ randomRIO (1, 10 :: Int)
      if shouldLie <= 3  -- 30% chance
        then do
          -- Pick a random position to lie about
          pos <- liftIO $ randomRIO (0, length results - 1)
          let originalResult = results !! pos
          newResult <- case originalResult of
            Green -> return Yellow   -- lie: make green into yellow
            Yellow -> return Gray    -- lie: make yellow into gray
            Gray -> return Yellow    -- lie: make gray into yellow
          
          let liedResults = take pos results ++ [newResult] ++ drop (pos + 1) results
          
          -- Mark that we've lied
          modify $ \s -> s { hasLied = True }
          
          printMessage "🤔 Something feels... different about this result..."
          return liedResults
        else return results

-- Easy mode: give helpful hints
giveEasyModeHints :: [LetterResult] -> Text -> WordleM ()
giveEasyModeHints results guess = do
  -- First check for contradictions with previous guesses
  checkForContradictions guess results
  
  let greenCount = length $ filter (== Green) results
  let yellowCount = length $ filter (== Yellow) results
  let grayCount = length $ filter (== Gray) results
  
  when (greenCount > 0) $ 
    printMessage $ "💚 Great! You got " ++ show greenCount ++ " letters in the right position!"
  
  when (yellowCount > 0) $ 
    printMessage $ "💛 " ++ show yellowCount ++ " letters are in the word but wrong position."
  
  when (grayCount == 5) $ 
    printMessage "😅 None of those letters are in the word. Try completely different letters!"
  
  when (greenCount == 4) $ 
    printMessage "🔥 So close! Just one more letter to get right!"
  
  -- Special hint for duplicate letters
  let guessStr = T.unpack guess
  let duplicates = findDuplicateLetters guessStr
  when (not (null duplicates) && yellowCount + greenCount < length guessStr) $
    printMessage "💡 Tip: If you used duplicate letters, remember each letter in the secret word can only be matched once!"

-- Check for contradictions in Easy mode
checkForContradictions :: Text -> [LetterResult] -> WordleM ()
checkForContradictions currentGuess currentResults = do
  state <- get
  let previousGuesses = guesses state
  case findGameContradiction currentGuess currentResults previousGuesses of
    Just contradiction -> do
      printMessage "⚠️  WAIT! This seems to contradict your previous guesses:"
      printMessage $ "   " ++ contradiction
      printMessage "   (This might mean you made an error, or I'm in Expert mode and lied earlier! 🤔)"
    Nothing -> return ()

-- Find contradictions in the main game (similar to Assistant mode but different context)
findGameContradiction :: Text -> [LetterResult] -> [GuessResult] -> Maybe String
findGameContradiction currentGuess currentResults previousGuesses = 
  let currentWord = T.unpack currentGuess
      currentPairs = zip currentWord currentResults
  in checkAgainstAllPrevious currentPairs previousGuesses
  where
    checkAgainstAllPrevious :: [(Char, LetterResult)] -> [GuessResult] -> Maybe String
    checkAgainstAllPrevious _ [] = Nothing
    checkAgainstAllPrevious currentPairs (prevGuess:rest) = 
      case checkConsistencyWithPrevious currentPairs prevGuess of
        Just contradiction -> Just contradiction
        Nothing -> checkAgainstAllPrevious currentPairs rest
    
    checkConsistencyWithPrevious :: [(Char, LetterResult)] -> GuessResult -> Maybe String
    checkConsistencyWithPrevious currentPairs prevGuess = 
      let prevWord = T.unpack (guessWord prevGuess)
          prevResults = letterResults prevGuess
          prevPairs = zip prevWord prevResults
      in findInconsistency currentPairs prevPairs
    
    findInconsistency :: [(Char, LetterResult)] -> [(Char, LetterResult)] -> Maybe String
    findInconsistency current previous = 
      let conflicts = [conflict | (currChar, currResult) <- current,
                                  (prevChar, prevResult) <- previous,
                                  currChar == prevChar,
                                  Just conflict <- [detectInconsistency currChar currResult prevResult]]
      in if null conflicts then Nothing else Just (head conflicts)
    
    detectInconsistency :: Char -> LetterResult -> LetterResult -> Maybe String
    detectInconsistency char Gray Green = 
      Just $ "Letter '" ++ [char] ++ "' was Green in a previous guess, but now it's Gray"
    detectInconsistency char Gray Yellow = 
      Just $ "Letter '" ++ [char] ++ "' was Yellow in a previous guess, but now it's Gray"
    detectInconsistency char Green Gray = 
      Just $ "Letter '" ++ [char] ++ "' was Gray in a previous guess, but now it's Green"
    detectInconsistency char Yellow Gray = 
      Just $ "Letter '" ++ [char] ++ "' was Gray in a previous guess, but now it's Yellow"
    detectInconsistency _ _ _ = Nothing

-- Find duplicate letters in a guess
findDuplicateLetters :: String -> [Char]
findDuplicateLetters str = [c | c <- str, length (filter (== c) str) > 1]