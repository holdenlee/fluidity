{-# OPTIONS

  -XExistentialQuantification
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

-- * Codelets

data Codelet' w a = Codelet' { act :: Double -> World w -> a -> (Maybe (World w, a), [Codelet w]),
                               _activation :: a -> World w -> Double,
                               memory :: a}
-- _activation should be salience and depend on importance and unhappiness
-- should have a salience that's accessible by an outside party...

--https://www.haskell.org/haddock/doc/html/ch03s08.html
{-| 
  A Codelet has 
    * some internal memory (hidden, of some type a), 
    * an activation function that depends on its memory and the outside world, 
    * and an action. 
  The action takes an activation value, the workspace, its memory, and returns maybe the updated workspace and memory, and a list of child codelets. If it returns Nothing, then it has self-destructed.
-} 
data Codelet w = forall a. (Show a) => Codelet {codelet::Codelet' w a} 

activation :: World w -> Codelet w -> Double
activation wo c = case c of Codelet c' -> _activation c' (memory c') wo

actC :: World w -> Codelet' w a -> (Maybe (World w, a), [Codelet w])
actC wo c = let m = memory c in act c (_activation c m wo) wo m

-- * Coderack

type Coderack w = IMap (Codelet w)

codelets :: Coderack w -> M.Map Int (Codelet w)
codelets = _map

-- * Slipnet

--should a concept be able to produce codelets? Or have a codelet that monitors activation of concepts?
data Concept = Concept { _id :: Int,
                         name :: String,
                         c_activation :: Double,
                         depth :: Double
                       } deriving (Show, Eq, Ord)

type Relation = Concept --no need have a different type
                         
type Slipnet = G.Gr Concept Relation

activationToFlow :: Double -> Double
activationToFlow = id

-- | 'diffuse' spreads activation by the given amount (value between 0 and 1).
diffuse :: Double -> G.Gr Concept Relation -> G.Gr Concept Relation
diffuse prop g =
    let
        --get the ID of the edge, and find the node that's labeled with that ID. (All edges are reified by nodes.)
        getEdgeFlow::Relation -> Double
        getEdgeFlow e = fromMaybe 0 $ fmap (activationToFlow . c_activation) $ lab g $ _id e
        --sum flows from outgoing edges, for each node of g
        degrees = M.fromList $ L.map (appendFun (sum . (L.map (getEdgeFlow . third)). (out g))) $ nodes g
    in
      --need to deal with division by 0...
      gmap (\(ins, nd, v, outs) -> (ins, nd, v{c_activation = sum $ (L.map (\(e, _) -> prop * (getEdgeFlow e) / (lookup2 (_id v) degrees) + (1 - prop) * (c_activation v)) ins)}, outs)) g
        

-- * World

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

pickCodelet :: (RandomGen g) => g -> World w -> (g, Int)
pickCodelet gen wo =
    let
        crack = coderack wo
        zTotal = z wo crack
        (r, newGen) = randomR (0, zTotal) gen
    in
      (newGen, getCodeletFromRandom wo r crack)

execCodelet :: Int -> World w -> World w
execCodelet i world =
  let 
     crack = coderack world
  in
    case lookup2 i $ codelets crack of
      Codelet c' ->
          let
              (updated, children) = actC world c'
          in
            --case c of 
            --  Nothing -> (world, crack) --shouldn't happen
            case updated of
              Nothing -> world {coderack = crack |> iDelete i |> iInserts children}
              Just (newWorld, newMem) -> 
                  newWorld {coderack = crack |> iInsertK i (Codelet (c'{memory = newMem}))
                                             |> iInserts children}

pickAndRun :: World w -> World w
pickAndRun wo = 
  case wo of 
   World w1 w2 w3 w4 gen -> 
    let
        (newGen, n) = pickCodelet gen wo
        newW = World w1 w2 w3 w4 gen
    in
      execCodelet n newW      

{- workspace: (don't quite get this)
Neighboring objects are probabilistically selected (biased towards salient objects) and scanned for similarities or relationships. Promising ones are reified as inter-object bonds. It is speed-biased towards sameness bonds.
-}
