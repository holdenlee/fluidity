{-# OPTIONS

  -XExistentialQuantification
  -XRank2Types
#-}

{-# LANGUAGE TemplateHaskell #-}

module Main where
import System.Environment
import Control.Monad
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.MultiMap as MM
import Data.Maybe
import Data.Char
import qualified Data.Set as S
import Data.Array
import Data.Tuple
import Data.Graph.Inductive as G
import System.Random
import Control.Lens hiding ((|>))

import Utilities
import ParseUtilities
import GraphUtils

import Mind
import PTScan
import Workspace
import PatternMatchers
import Generalizor

logg :: (a -> a) -> (a -> IO b) -> (IO a -> IO a)
logg f disp ia = 
    do
      x <- ia
      let result = f x
      disp result
      return result

initMind :: [Int] -> [(Int, Agent (Mind Workspace mes) mes)] -> StdGen -> Mind Workspace mes
initMind li ags g = 
    Mind { _workspace = listToWorkspace li,
           _temp =50,
           _agents = M.empty |> insertMultiple ags,
           _active = map fst ags,
           _slipnet = G.empty,
           _followers = MM.empty,
           _rng = g}

main = runOnList [1,1,2,1,2,3,1,2,3,4] 20

runOnList li n = do
  g <- getStdGen
  let mind = initMind li [(1, (Agent replicator)), (2, Agent ranger), (3, Agent generalizor)] g
  repeatTimes n (logg runOneCycle (\m -> putStrLn $ show $ _workspace m)) (return mind)

test1 = runOnList [1,1,2,2,1,2,3,3,3,1,2,3,4,4,4,4]

