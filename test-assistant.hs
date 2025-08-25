-- Test script to demonstrate Assistant Mode
-- Let's pretend the secret word is "SMILE"

import Wordle.Assistant
import Wordle.ColorLogic
import Data.Text (Text)
import qualified Data.Text as T

-- Simulate what feedback the human would give for each guess
simulateFeedback :: String -> String -> [String]
simulateFeedback guess secret = 
  let results = calculateColorsCorrect guess secret
  in map colorToString results
  where
    colorToString Green = "G"
    colorToString Yellow = "Y"
    colorToString Gray = "R"

-- Test the assistant's knowledge system
testAssistantLogic :: IO ()
testAssistantLogic = do
  putStrLn "=== Testing Assistant Mode Logic ==="
  putStrLn "Secret word: SMILE"
  putStrLn ""
  
  let secret = "SMILE"
  let knowledge0 = emptyKnowledge
  
  -- First guess: SOARE
  let guess1 = "SOARE"
  let feedback1 = calculateColorsCorrect guess1 secret
  let knowledge1 = updateKnowledge (T.pack guess1) feedback1 knowledge0
  
  putStrLn $ "Guess 1: " ++ guess1
  putStrLn $ "Feedback: " ++ show feedback1
  putStrLn $ "Knowledge: " ++ show knowledge1
  putStrLn ""
  
  -- Second guess: Let's see what satisfies the knowledge
  let testWords = ["SMILE", "SLIME", "SPILT", "SPLIT"]
  putStrLn "Testing candidate words:"
  mapM_ (\w -> putStrLn $ w ++ ": " ++ show (satisfiesKnowledge knowledge1 w)) testWords

main = testAssistantLogic