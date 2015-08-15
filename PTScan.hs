{-# OPTIONS

  -XExistentialQuantification
  -XRank2Types
#-}

module PTScan where
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

import Mind

--Carries out one "cycle" of the mind.

scout :: Int -> Mind wksp mem -> (Double, Mind wksp mem)
scout n w = 
    case (getAgent n w) of 
      Agent a' -> 
          case (_scout a') w a' of
            (d, a2) -> (d, setAgent n (Agent a2) w)

scoutAll :: Mind wksp mem -> ([(Int, Double)], Mind wksp mem)
scoutAll w = 
    --for all the active indices, run scout. Add the activations to a list, and update the mind at each step.
    for (_active w) ([],w) (\i (li,w1) ->
         case (scout i w1) of 
           (d, w2) -> ((i,d):li, w2))
