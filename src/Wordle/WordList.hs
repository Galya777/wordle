module Wordle.WordList where

import System.Random
import Data.Char (toUpper)

-- Zarejdane na dumi ot fail
zarediDumi :: FilePath -> IO [String]
zarediDumi pateka = do
  sadarjanie <- readFile pateka
  let redove = lines sadarjanie
  return $ map (map toUpper) (filter (/= "") redove)

-- Izbirane na sluchaina duma
izberiSluchainaDuma :: [String] -> IO String
izberiSluchainaDuma dumi = do
  index <- randomRIO (0, length dumi - 1)
  return (dumi !! index)
