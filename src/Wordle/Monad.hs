-- My attempt at using MTL for the wordle game
module Wordle.Monad where

import Control.Monad.State
import Control.Monad.Reader  
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Wordle.Types

-- What I need to track during the game
data GameState = GameState
  { secretWord :: Text            
  , guesses :: [GuessResult]        
  , maxGuesses :: Int               
  , difficulty :: Difficulty        
  , hasLied :: Bool                 
  } deriving (Show, Eq)

-- Settings that don't change during game
data GameConfig = GameConfig
  { wordList :: [Text]              
  , wordLength :: Int               
  } deriving (Show, Eq)

-- Things that can go wrong
data GameError 
  = InvalidWord Text                
  | ContradictsPrevious Text        
  | GameOver                        
  | PlayerQuit                      -- Player chose to quit
  | InvalidInput Text               
  deriving (Show, Eq)


-- My monad stack - combines state, config, errors, and IO
-- Still figuring out how this works exactly
type WordleM = StateT GameState (ReaderT GameConfig (ExceptT GameError IO))

-- Function to actually run the monad
runWordleM :: GameState -> GameConfig -> WordleM a -> IO (Either GameError (a, GameState))
runWordleM initialState config action = 
  runExceptT $ runReaderT (runStateT action initialState) config