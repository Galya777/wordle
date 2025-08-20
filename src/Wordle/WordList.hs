-- Loading words from file and picking random ones
module Wordle.WordList where

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Random
import Data.Char (toUpper)

-- Read all words from the file
loadWordList :: FilePath -> IO [Text]
loadWordList filePath = do
  content <- TIO.readFile filePath
  let wordLines = T.lines content
  -- Convert to uppercase and filter out empty lines
  let words = filter (not . T.null) $ map (T.map toUpper) wordLines
  return words

-- Pick a random word from the list
pickRandomWord :: [Text] -> IO Text
pickRandomWord [] = error "Empty word list!"
pickRandomWord words = do
  randomIndex <- randomRIO (0, length words - 1)
  return (words !! randomIndex)

-- Load words and pick a random one in one go
loadAndPickRandomWord :: FilePath -> IO Text
loadAndPickRandomWord filePath = do
  words <- loadWordList filePath
  pickRandomWord words