module Wordle.Game where

import System.Random
import Wordle.Types
import Wordle.Monad
import Wordle.ColorLogic
import Data.Char (toUpper)

-- Main game loop
igraiIgra :: GameState -> IO ()
igraiIgra sustoianie = do
  if length (guesses sustoianie) >= maxGuesses sustoianie
    then do
      putStrLn "YOU LOSE! The word was:"
      putStrLn (secretWord sustoianie)
    else do
      putStrLn $ "Attempt " ++ show (length (guesses sustoianie) + 1) ++ "/" ++ show (maxGuesses sustoianie)
      putStrLn "Enter a word (5 letters):"
      vhod <- getLine
      let duma = map toUpper vhod

      if length duma /= 5
        then do
          putStrLn "Error: The word must be 5 letters!"
          igraiIgra sustoianie
        else do
          let cvetove = proveriCvetove duma (secretWord sustoianie)
          let novOpit = GuessResult duma cvetove
          let novoSustoianie = sustoianie { guesses = guesses sustoianie ++ [novOpit] }

          pokajiRezultat cvetove

          if all (== Green) cvetove
            then putStrLn "GREAT! YOU WIN!"
            else igraiIgra novoSustoianie

-- Displays the colors in an understandable way
pokajiRezultat :: [LetterResult] -> IO ()
pokajiRezultat [] = putStrLn ""
pokajiRezultat (r:rs) = do
  case r of
    Green  -> putStr " [G] "
    Yellow -> putStr " [Y] "
    Gray   -> putStr " [.] "
  pokajiRezultat rs
