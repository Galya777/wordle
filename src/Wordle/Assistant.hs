module Wordle.Assistant where

import Data.List (nub)
import Wordle.Types
import Wordle.Monad
import Wordle.ColorLogic

-- Assistant for finding words
pomogniMi :: [String] -> [GuessResult] -> [String]
pomogniMi dumi opiti = filter podhodiashta dumi
  where
    podhodiashta d = all (proveriDuma d) opiti

-- Check if a word matches the result of a guess
proveriDuma :: String -> GuessResult -> Bool
proveriDuma d (GuessResult g res) =
  let ochakvanRes = proveriCvetove g d
  in ochakvanRes == res

-- Main function for assistant mode
asistentCikul :: [String] -> [GuessResult] -> IO ()
asistentCikul vsiachkiDumi opiti = do
  let kandidati = pomogniMi vsiachkiDumi opiti
  putStrLn $ "Found words: " ++ show (length kandidati)
  if length kandidati <= 10
    then print kandidati
    else print (take 10 kandidati ++ ["..."])

  if null kandidati
    then putStrLn "Error: no words found!"
    else do
      putStrLn "What was your guess?"
      opitDuma <- getLine
      putStrLn "What are the colors? (G=Green, Y=Yellow, R=Gray, e.g. GYYRG)"
      cvetoveInput <- getLine
      
      let novOpit = GuessResult opitDuma (map parseCvet cvetoveInput)
      asistentCikul vsiachkiDumi (opiti ++ [novOpit])

parseCvet :: Char -> LetterResult
parseCvet 'G' = Green
parseCvet 'Y' = Yellow
parseCvet 'R' = Gray
parseCvet _ = Gray
