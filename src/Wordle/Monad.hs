-- | This module shows how we use MTL to combine different "effects"
module Wordle.Monad where

import Control.Monad.State
import Control.Monad.Reader  
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Wordle.Types


data GameState = GameState
  { secretWord :: Text            
  , guesses :: [GuessResult]        
  , maxGuesses :: Int               
  , difficulty :: Difficulty        
  , hasLied :: Bool                 
  } deriving (Show, Eq)

-- | Game configuration
data GameConfig = GameConfig
  { wordList :: [Text]              -- ^ All possible words
  , wordLength :: Int               -- ^ word length to guess
  } deriving (Show, Eq)

-- | Error types
data GameError 
  = InvalidWord Text                -- ^ Word not in dictionary
  | ContradictsPrevious Text        -- ^ Guess contradicts previous answers
  | GameOver                        -- ^ No more guesses allowed
  | InvalidInput Text               -- ^ Bad user input
  deriving (Show, Eq)



-- - StateT: Remember game state
-- - ReaderT: Access configuration  
-- - ExceptT: Handle errors
-- - IO: Read files, print to screen
type WordleM = StateT GameState (ReaderT GameConfig (ExceptT GameError IO))

-- | Helper function to run our WordleM computation
runWordleM :: GameState -> GameConfig -> WordleM a -> IO (Either GameError (a, GameState))
runWordleM initialState config action = 
  runExceptT $ runReaderT (runStateT action initialState) config