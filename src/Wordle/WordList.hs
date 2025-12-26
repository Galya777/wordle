module Wordle.WordList where

import System.Random
import Data.Char (toUpper)

-- Loading words from file
zarediDumi :: FilePath -> IO [String]
zarediDumi pateka = do
  sadarjanie <- readFile pateka
  let redove = lines sadarjanie
  return $ map (map toUpper) (filter (/= "") redove)

-- Choosing a random word
izberiSluchainaDuma :: [String] -> IO String
izberiSluchainaDuma dumi = do
  index <- randomRIO (0, length dumi - 1)
  return (dumi !! index)
