{-# OPTIONS

  -XExistentialQuantification
  -XRank2Types
#-}

{-# LANGUAGE TemplateHaskell #-}

-- | Agents created from formulas. Each of these agents has a set of formulas, and it tries to apply the formulas to build upon existing structures. For example, 'ranger' would try to apply the formula [1..n]++[n+1] = [1..(n+1)] to build longer ranges.
module PatternMatchers where

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

import TreeState
import Utilities
import ParseUtilities
import IMap
import GraphUtils
import RandomUtils

import Mind
import Workspace

rangerPattern = parsePattern "range(?1,?2)"
singlePattern = parsePattern "List(?1)"
replPattern = parsePattern "replicate(?1,?2)"

data PMMemory = PMMemory {_childStructs :: [Int]}

makeLenses ''PMMemory

makePatternMatcher' :: String -> (MTreeState', MTreeState', [Int] -> Bool) -> Agent' (Mind Workspace mes) PMMemory mes
makePatternMatcher' myName _ =
    Agent' {_name = myName,
            --look at total strength of child structures
            _scout = \w a ->
                     let 
                         childIDs = _childStructs $ _memory a
                     in
                       (L.sum $ L.map (\cid -> let str = w ^. (workspace . board . nodeIndex cid) in lclamp 0 $ fromIntegral (_length str) + totalMod (_modifiers str)) childIDs, a),
--check the actual way the numbers work! maybe should be exponential not linear?
            _act = undefined,
            _memory = PMMemory [],
            _inbox = []}
                                              
                                                

