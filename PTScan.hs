{-# OPTIONS

  -XExistentialQuantification
#-}

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

data Codelet' w a = Codelet' { act :: Double -> w -> a -> (Maybe (w, a), [Codelet w]),
                               _activation :: a -> World w -> Double,
                               memory :: a}

data Codelet w = forall a. (Show a) => Codelet {codelet::Codelet' w a} 

activation :: World w -> Codelet w -> Double
activation wo c = case c of Codelet c' -> _activation c' (memory c') wo

actC :: World w -> Codelet' w a -> (Maybe (w, a), [Codelet w])
actC wo c = let m = memory c in act c (_activation c m wo) (workspace wo) m

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

type Coderack w = IMap (Codelet w)

codelets :: Coderack w -> M.Map Int (Codelet w)
codelets = _map

--should a concept be able to produce codelets? Or have a codelet that monitors activation of concepts?
data Concept = Concept { id :: Int,
                         name :: String,
                         c_activation :: Double,
                         depth :: Double
                       }

type Relation = Concept --no need have a different type
                         
type Slipnet = G.Gr Concept Relation

data World w = forall g. RandomGen g => World { workspace :: w,
                                                temp :: Double,
                                                coderack :: Coderack w,
                                                slipnet :: Slipnet,
                                                rng :: g}

tempActivation :: Double -> Double -> Double
tempActivation t a = exp ((log a) * t)

z :: World w -> Coderack w -> Double
z wo crack = L.sum $ L.map (tempActivation (temp wo) . activation wo) (M.elems $ codelets crack)

gcfr :: World w -> Double -> [(Int, Codelet w)] -> Int
gcfr wo rand pairs =
    case pairs of
      [] -> 0 --uh-oh.
      (i,c):rest -> 
          let 
              energy = tempActivation (temp wo) $ activation wo c
          in
            if rand < energy
            then i
            else gcfr wo (rand - energy) rest
        

getCodeletFromRandom :: World w -> Double -> Coderack w -> Int
getCodeletFromRandom wo rand crack = gcfr wo rand $ M.assocs $ codelets crack

pickCodelet :: (RandomGen g) => g -> World w -> Coderack w -> (g, Int)
pickCodelet gen wo crack =
    let
        zTotal = z wo crack
        (r, newGen) = randomR (0, zTotal) gen 
    in
      (newGen, getCodeletFromRandom wo r crack)

{-
World { workspace :: w,
                           temp :: Double,
                           coderack :: Coderack w a,
                           slipnet :: Slipnet,
                           rng :: g}
-}
execCodelet :: Int -> World w -> Coderack w -> World w
execCodelet i world crack =
    case lookup2 i $ codelets crack of
      Codelet c' ->
          let
              (updated, children) = actC world c'
          in
            --case c of 
            --  Nothing -> (world, crack) --shouldn't happen
            case updated of
              Nothing -> world {coderack = crack |> iDelete i |> iInserts children}
              Just (newWorkspace, newMem) -> 
                  world {workspace = newWorkspace,
                         coderack = crack |> iInsertK i (Codelet (c'{memory = newMem}))
                                          |> iInserts children}
