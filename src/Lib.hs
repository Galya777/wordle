module Lib
    ( someFunc
    ) where

import Wordle.Types
import Wordle.Monad
import Data.Text (Text)
import qualified Data.Text as T

someFunc :: IO ()
someFunc = do
  putStrLn "Welcome to Wordle!"

  

  let config = GameConfig [T.pack "HELLO", T.pack "WORLD"] 5
  let initialState = GameState (T.pack "HELLO") [] 6 Normal False
  
  result <- runWordleM initialState config $ do
    return "Game initialized successfully!"
    
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (msg, finalState) -> do
      putStrLn msg
      putStrLn $ "Final state: " ++ show finalState