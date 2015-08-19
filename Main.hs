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

logg :: (a -> a) -> (a -> IO b) -> (IO a -> IO a)
logg f disp ia = 
    do
      x <- ia
      let result = f x
      disp result
      return result

main = do
  let li =[1,1,2,1,2,3,1,2,3,4]
  let wk = listToWorkspace li
  g <- getStdGen
  let mind = Mind { _workspace =wk,
                    _temp =50,
                    _agents = M.empty |> insertMultiple [(1, (Agent replicator)), (2, Agent ranger)],
                    _active = [1, 2],
                    _slipnet = G.empty,
                    _followers = MM.empty,
                    _rng = g}
  repeatTimes 20 (logg runOneCycle (\m -> putStrLn $ show $ _workspace m)) (return mind)
