module Wordle.Assistant where

import Data.List (nub)
import Wordle.Types
import Wordle.Monad
import Wordle.ColorLogic

-- Pomoshtnik za namirane na dumi
pomogniMi :: [String] -> [GuessResult] -> [String]
pomogniMi dumi opiti = filter podhodiashta dumi
  where
    podhodiashta d = all (proveriDuma d) opiti

-- Proverka dali duma otgovaria na rezultat ot opit
proveriDuma :: String -> GuessResult -> Bool
proveriDuma d (GuessResult g res) = 
  let ochakvanRes = proveriCvetove g d
  in ochakvanRes == res

-- Glavna funkcia za asistent rejim
asistentCikul :: [String] -> [GuessResult] -> IO ()
asistentCikul vsiachkiDumi opiti = do
  let kandidati = pomogniMi vsiachkiDumi opiti
  putStrLn $ "Namereni dumi: " ++ show (length kandidati)
  if length kandidati <= 10
    then print kandidati
    else print (take 10 kandidati ++ ["..."])
  
  if null kandidati
    then putStrLn "Greshka: ne namerih dumi!"
    else do
      putStrLn "Koi beshe tvoia opit?"
      opitDuma <- getLine
      putStrLn "Kakvi sa cvetovete? (G=Green, Y=Yellow, R=Gray, npr. GYYRG)"
      cvetoveInput <- getLine
      
      let novOpit = GuessResult opitDuma (map parseCvet cvetoveInput)
      asistentCikul vsiachkiDumi (opiti ++ [novOpit])

parseCvet :: Char -> LetterResult
parseCvet 'G' = Green
parseCvet 'Y' = Yellow
parseCvet 'R' = Gray
parseCvet _ = Gray
