module Wordle.Game where

import System.Random
import Wordle.Types
import Wordle.Monad
import Wordle.ColorLogic
import Data.Char (toUpper)

-- Glaven cikul na igrata
igraiIgra :: GameState -> IO ()
igraiIgra sustoianie = do
  if length (guesses sustoianie) >= maxGuesses sustoianie
    then do
      putStrLn "ZAGUBI! Dumata beshe:"
      putStrLn (secretWord sustoianie)
    else do
      putStrLn $ "Opit " ++ show (length (guesses sustoianie) + 1) ++ "/" ++ show (maxGuesses sustoianie)
      putStrLn "Vuvedi duma (5 bukvi):"
      vhod <- getLine
      let duma = map toUpper vhod
      
      if length duma /= 5
        then do
          putStrLn "Greshka: Dumata triabva da e 5 bukvi!"
          igraiIgra sustoianie
        else do
          let cvetove = proveriCvetove duma (secretWord sustoianie)
          let novOpit = GuessResult duma cvetove
          let novoSustoianie = sustoianie { guesses = guesses sustoianie ++ [novOpit] }
          
          pokajiRezultat cvetove
          
          if all (== Green) cvetove
            then putStrLn "BRAVO! SPECHELI!"
            else igraiIgra novoSustoianie

-- Pokazva cvetovete po po-razbiram nachin
pokajiRezultat :: [LetterResult] -> IO ()
pokajiRezultat [] = putStrLn ""
pokajiRezultat (r:rs) = do
  case r of
    Green  -> putStr " [G] "
    Yellow -> putStr " [Y] "
    Gray   -> putStr " [.] "
  pokajiRezultat rs
