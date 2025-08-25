-- Test script to demonstrate duplicate letter handling
-- This shows the difference between old and new color logic

import Data.Text (Text)
import qualified Data.Text as T

-- Old simple logic (has bugs with duplicates)
calculateColorsSimple :: String -> String -> [String]
calculateColorsSimple guess secret = zipWith checkLetter guess [0..]
  where
    checkLetter guessChar pos
      | guessChar == (secret !! pos) = "Green"   -- exact match
      | guessChar `elem` secret = "Yellow"       -- in word, wrong spot
      | otherwise = "Gray"                       -- not in word

-- New correct logic (handles duplicates properly)
calculateColorsCorrect :: String -> String -> [String]
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
        then ("Green" : results, available)  -- Exact match
        else case lookupAndDecrement guessChar available of
          Just newAvailable -> ("Yellow" : results, newAvailable)  -- Found and used
          Nothing -> ("Gray" : results, available)  -- Not available

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

-- Test cases that show the bug
testCase :: String -> String -> String -> IO ()
testCase description guess secret = do
  putStrLn $ "=== " ++ description ++ " ==="
  putStrLn $ "Guess: " ++ guess ++ ", Secret: " ++ secret
  putStrLn $ "Old (buggy):  " ++ show (calculateColorsSimple guess secret)
  putStrLn $ "New (fixed):  " ++ show (calculateColorsCorrect guess secret)
  putStrLn ""

main :: IO ()
main = do
  putStrLn "🎯 DUPLICATE LETTER HANDLING TEST"
  putStrLn "This shows how our improved color logic handles duplicate letters correctly!"
  putStrLn ""
  
  testCase "Case 1: Multiple same letters in guess, one in secret" 
           "AAAAA" "ABCDE"
  
  testCase "Case 2: Two L's in guess, two in secret (different positions)"
           "LLAMA" "HELLO"
  
  testCase "Case 3: Multiple E's in guess, two in secret"
           "ERASE" "SPEED"
  
  testCase "Case 4: Duplicate T's in guess, two in secret"
           "TATTY" "JETTY"
  
  putStrLn "✅ The 'New (fixed)' results are what our game now uses!"
  putStrLn "✅ Notice how duplicate letters are handled correctly - only the available ones get Yellow!"