#!/usr/bin/env runhaskell

import Wordle.ColorLogic

main :: IO ()
main = do
  putStrLn "=== Demonstrating the Color Logic Fix ==="
  putStrLn ""
  
  putStrLn "Case 1: Secret='HELLO', Guess='LLAMA'"
  putStrLn "  Old logic: Both L's would be Yellow (wrong!)"
  putStrLn "  New logic: First L is Yellow, second L is Gray (correct!)"
  testColorLogic "LLAMA" "HELLO"
  
  putStrLn "Case 2: Secret='SPEED', Guess='ERASE'"
  putStrLn "  Old logic: Both E's might be Yellow (wrong!)"
  putStrLn "  New logic: Only one E can be Yellow (correct!)"
  testColorLogic "ERASE" "SPEED"
  
  putStrLn "Case 3: Secret='ABCDE', Guess='AAAAA'"
  putStrLn "  Old logic: All A's would be Green/Yellow (wrong!)"
  putStrLn "  New logic: Only first A is Green, rest are Gray (correct!)"
  testColorLogic "AAAAA" "ABCDE"