{-# OPTIONS

  -XExistentialQuantification
  -XTupleSections
#-}

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
import Data.Tree as T

import Utilities
import ParseUtilities
import IMap
import PTScan
import Workspace
import SeekWhence
--import Formulas

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
  let world = World { workspace =wk,
                      temp =50,
                      coderack = iEmpty |> iInsert replicator |> iInsert ranger,
                      slipnet = G.empty,
                      rng = g}
  repeatTimes 20 (logg pickAndRun (\wo -> putStrLn $ show $ workspace wo)) (return world)
