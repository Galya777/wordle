module Wordle.ColorLogic where

import Wordle.Types

-- Proverka na cvetovete (student style)
proveriCvetove :: String -> String -> [LetterResult]
proveriCvetove duma taina = map proveri (zip [0..] duma)
  where
    proveri (i, bukva)
      | bukva == (taina !! i) = Green
      | bukva `elem` taina    = Yellow
      | otherwise            = Gray

-- Funkcia za po-tochno presmiatane (ako studenta se e potrudil poveche)
presmetniCvetove :: String -> String -> [LetterResult]
presmetniCvetove [] [] = []
presmetniCvetove (g:gs) (s:ss)
  | g == s = Green : presmetniCvetove gs ss
  | g `elem` (s:ss) = Yellow : presmetniCvetove gs ss
  | otherwise = Gray : presmetniCvetove gs ss
presmetniCvetove _ _ = []
