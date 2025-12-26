module Lib (someFunc) where

import Wordle.Types
import Wordle.Monad
import Wordle.Game
import Wordle.WordList
import Wordle.Assistant

someFunc :: IO ()
someFunc = do
  putStrLn "Loading words..."
  vsiachkiDumi <- zarediDumi "words"

  putStrLn "Choose mode: 1 - You play, 2 - Assistant"
  izbor <- getLine

  if izbor == "1"
    then do
      tainaDuma <- izberiSluchainaDuma vsiachkiDumi
      let nachalo = novoGameState tainaDuma Normal
      igraiIgra nachalo
    else do
      putStrLn "Assistant mode started."
      asistentCikul vsiachkiDumi []
