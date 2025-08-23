-- Better color logic that handles duplicate letters correctly
module Wordle.ColorLogic where

import Data.Text (Text)
import qualified Data.Text as T
import Wordle.Types

-- The old simple logic (has bugs with duplicates)
calculateColorsSimple :: String -> String -> [LetterResult]
calculateColorsSimple guess secret = zipWith checkLetter guess [0..]
  where
    checkLetter guessChar pos
      | guessChar == (secret !! pos) = Green  -- exact match
      | guessChar `elem` secret = Yellow       -- in word, wrong spot
      | otherwise = Gray                       -- not in word

-- The correct logic that handles duplicates properly
calculateColorsCorrect :: String -> String -> [LetterResult]
calculateColorsCorrect guess secret = 
  let
    -- Step 1: Mark all exact matches (Green) first
    exactMatches = zipWith (\g s -> g == s) guess secret
    
    -- Step 2: Count available letters (excluding those used for Green)
    availableLetters = countAvailableLetters secret exactMatches
    
    -- Step 3: Process each position, tracking what we've used
    (results, _) = foldl processPosition ([], availableLetters) (zip3 guess secret [0..])
  in
    reverse results  -- we built the list backwards
  where
    -- Count how many of each letter are available for Yellow matches
    countAvailableLetters secret exactMatches = 
      let secretWithoutExact = [s | (s, isExact) <- zip secret exactMatches, not isExact]
      in foldr (\c acc -> insertOrIncrement c acc) [] secretWithoutExact
    
    -- Process one position at a time
    processPosition (results, available) (guessChar, secretChar, pos) =
      if guessChar == secretChar
        then (Green : results, available)  -- Exact match
        else case lookupAndDecrement guessChar available of
          Just newAvailable -> (Yellow : results, newAvailable)  -- Found and used
          Nothing -> (Gray : results, available)  -- Not available

-- Helper functions for counting letters
insertOrIncrement :: Char -> [(Char, Int)] -> [(Char, Int)]
insertOrIncrement c [] = [(c, 1)]
insertOrIncrement c ((x, count) : xs)
  | c == x = (x, count + 1) : xs
  | otherwise = (x, count) : insertOrIncrement c xs

lookupAndDecrement :: Char -> [(Char, Int)] -> Maybe [(Char, Int)]
lookupAndDecrement _ [] = Nothing
lookupAndDecrement c ((x, count) : xs)
  | c == x && count > 1 = Just ((x, count - 1) : xs)
  | c == x && count == 1 = Just xs  -- Remove entry when count reaches 0
  | otherwise = fmap ((x, count) :) (lookupAndDecrement c xs)

-- Test function to compare the two approaches
testColorLogic :: String -> String -> IO ()
testColorLogic guess secret = do
  putStrLn $ "Testing: guess='" ++ guess ++ "' secret='" ++ secret ++ "'"
  putStrLn $ "Simple:  " ++ show (calculateColorsSimple guess secret)
  putStrLn $ "Correct: " ++ show (calculateColorsCorrect guess secret)
  putStrLn ""

-- Run some tests to show the difference
runTests :: IO ()
runTests = do
  putStrLn "=== Color Logic Tests ==="
  putStrLn ""
  
  -- Test case 1: Basic case (should be same)
  testColorLogic "HELLO" "WORLD"
  
  -- Test case 2: Duplicate letters - this shows the bug
  testColorLogic "LLAMA" "HELLO"  -- Second L should be Gray, not Yellow
  
  -- Test case 3: Multiple duplicates
  testColorLogic "ERASE" "SPEED"  -- Only one E should be Yellow
  
  -- Test case 4: All same letter
  testColorLogic "AAAAA" "ABCDE"  -- Only first A should be Yellow