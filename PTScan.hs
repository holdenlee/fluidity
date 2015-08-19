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
import RandomUtils

import Mind

--Carries out one "cycle" of the mind.

{-| Ask the nth agent for its activation level (and allow it to do scouting). -}
scoutOne :: Int -> Mind wksp mem -> (Double, Mind wksp mem)
scoutOne n w = 
    case (getAgent n w) of 
      Agent a' -> 
          case (_scout a') w a' of
            (d, a2) -> (d, setAgent n (Agent a2) w)

{-| Ask every active agent to scout. -}
scoutAll :: Mind wksp mem -> ([(Int, Double)], Mind wksp mem)
scoutAll w = 
    --for all the active indices, run scout. Add the activations to a list, and update the mind at each step.
    for (_active w) ([],w) (\i (li,w1) ->
         case (scoutOne i w1) of 
           (d, w2) -> ((i,d):li, w2))

{-| Calculate activation from temperature and self-reported activation -}
tempActivation :: Double -> Double -> Double
tempActivation t a = exp ((log a) * t)
--should this involve depth in some way? Or maybe indirectly?

calcActivation :: Mind wksp mes -> Double -> Double
calcActivation m d = tempActivation (_temp m) d

calcActivations :: Mind wksp mes -> [(Int, Double)] -> [(Int, Double)]
calcActivations m = L.map (mapSnd (calcActivation m))

--{-| Get a random number in (0,1) from mind, and update the rng seed.-}
--getRandom :: Mind wksp mes -> (Double, Mind wksp mes)
--getRandom m = 
--    let (r, newGen) = randomR (0,1) (_rng m)
--    in (r, m{_rng = newGen})

{-| Given the self-reported activations, pick a codelet and run -}
pickAndRun :: ([(Int, Double)], Mind wksp mem) -> Mind wksp mem
pickAndRun (li, m) = 
    let
        (r, m') = getRandom (0,1) m
        chosen = li |> calcActivations m' |> normalizeList |> chooseByProbs r
    in
      runAgent chosen m'

runOneCycle = scoutAll |>> pickAndRun
