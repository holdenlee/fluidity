module RandomUtil where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import System.Random

{-| Given a random number in [0,1], and a list of results with corresponding probabilities, choose a result. For example, 
    chooseByProbs x [(1,0.3), (2,0.7)]
chooses 1 if 0<=x<=0.3, and 2 if 0.3<x<=1.
-}
chooseByProbs :: Double -> [(a,Double)] -> a 
chooseByProbs r li = 
    case li of
      [(i,c)] -> i
      (i,c):rest ->
            if r <= c 
            then i
            else chooseByProbs (r - c) rest
      [] -> undefined 

normalizeList :: [(a, Double)] -> [(a, Double)]
normalizeList li = 
    let
        s = L.sum $ L.map snd li
    in
      L.map (\(x,y) -> (x,y/s)) li
