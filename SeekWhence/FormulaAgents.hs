{-# OPTIONS

  -XExistentialQuantification
  -XRank2Types
#-}

-- | Agents created from formulas. Each of these agents has a set of formulas, and it tries to apply the formulas to build upon existing structures. For example, 'ranger' would try to apply the formula [1..n]++[n+1] = [1..(n+1)] to build longer ranges.
module FormulaAgents where

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

import Utilities
import ParseUtilities
import IMap
import GraphUtils
import RandomUtils

import Mind
