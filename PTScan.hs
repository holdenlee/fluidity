module GraphUtils where
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


data Codelet w a = Codelet { act :: (w, a, Double) -> (Maybe (w, a, Double), [Codelet w a]),
                             activation :: Double,
                             memory :: a}

actC :: w -> Codelet w a -> (Maybe (w, a, Double), [Codelet w a])
actC wk c = act c (wk, memory c, activation c)

data IMap v = IMap {_map :: M.Map Int v,
                    _curID :: Int}

iEmpty :: IMap v
iEmpty = IMap M.empty 0

iInsert :: v -> IMap v -> IMap v
iInsert val im = IMap (_map im |> M.insert (_curID im) val) (_curID im + 1)

iInsertK :: Int -> v -> IMap v -> IMap v
iInsertK k val im = im{_map = (_map im |> M.insert k val)}

iInserts :: [v] -> IMap v -> IMap v
iInserts vals im = foldIterate iInsert vals im

iDelete :: Int -> IMap v -> IMap v
iDelete k im = im {_map = _map im |> M.delete k}

type Coderack w a = IMap (Codelet w a)

codelets :: Coderack w a -> M.Map Int (Codelet w a)
codelets = _map

--should a concept be able to produce codelets? Or have a codelet that monitors activation of concepts?
data Concept = Concept { id :: Int,
                         name :: String,
                         c_activation :: Double
                         depth :: Double
                       }

type Relation = Concept --no need have a different type
                         
type Slipnet = G.Gr Concept Relation

data World w a g = World { workspace :: w,
                           temp :: Double,
                           coderack :: Coderack w a,
                           slipnet :: Slipnet,
                           rng :: g}

tempActivation :: Double -> Double -> Double
tempActivation t a = exp ((log a) * t)

z :: Double -> Coderack w a -> Double
z t crack = L.sum $ L.map (tempActivation t . activation) (M.elems $ codelets crack)

gcfr :: Double -> Double -> [(Int, Codelet w a)] -> Int
gcfr t rand pairs =
    case pairs of
      [] -> 0 --uh-oh.
      (i,c):rest -> 
          let 
              energy = tempActivation t $ activation c
          in
            if rand < energy
            then i
            else gcfr t (rand - energy) rest
        

getCodeletFromRandom :: Double -> Double -> Coderack w a -> Int
getCodeletFromRandom t rand crack = gcfr t rand $ M.assocs $ codelets crack

pickCodelet :: (RandomGen g) => g -> Double -> Coderack w a -> (g, Int)
pickCodelet gen t crack =
    let
        zTotal = z t crack
        (r, newGen) = randomR (0, zTotal) gen 
    in
      (newGen, getCodeletFromRandom t r crack)

{-
World { workspace :: w,
                           temp :: Double,
                           coderack :: Coderack w a,
                           slipnet :: Slipnet,
                           rng :: g}
-}
execCodelet :: (RandomGen g) => Int -> World w a g -> Coderack w a -> World w a g
execCodelet i world crack =
    let
        c = lookup2 i $ codelets crack
        (updated, children) = actC (workspace world) c
    in
      --case c of 
      --  Nothing -> (world, crack) --shouldn't happen
      case updated of
        Nothing -> world {coderack = crack |> iDelete i |> iInserts children}
        Just (newWorkspace, newMem, newActiv) -> 
                   world {workspace = newWorkspace,
                          coderack = crack |> iInsertK i (c{activation = newActiv,
                                                           memory = newMem})
                                           |> iInserts children}
