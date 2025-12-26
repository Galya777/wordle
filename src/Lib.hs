module Lib (someFunc) where

import Wordle.Types
import Wordle.Monad
import Wordle.Game
import Wordle.WordList
import Wordle.Assistant

someFunc :: IO ()
someFunc = do
  putStrLn "Zaredete dumi..."
  vsiachkiDumi <- zarediDumi "words"
  
  putStrLn "Izberete rejim: 1 - Igraesh ti, 2 - Asistent"
  izbor <- getLine
  
  if izbor == "1"
    then do
      tainaDuma <- izberiSluchainaDuma vsiachkiDumi
      let nachalo = novoGameState tainaDuma Normal
      igraiIgra nachalo
    else do
      putStrLn "Asistent rejim startiran."
      asistentCikul vsiachkiDumi []
