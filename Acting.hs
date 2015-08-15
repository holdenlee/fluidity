{-# OPTIONS

  -XExistentialQuantification
  -XRank2Types
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

import Utilities
import ParseUtilities
import IMap
import GraphUtils

{-
data World = Actors [Actor World]

data Actor w = Actor {act :: World -> World}
-}

data Actor w = Actor {_name :: String,
                      _act :: w -> w}

instance Show (Actor w) where
    show a = _name a

data World = World {_actors :: [Actor World]} deriving Show

a1 = Actor {_name = "Cloner", _act = \w -> w{_actors = _actors w ++ [a1]}}

a2 = Actor {_name = "Killer", _act = \w -> w{_actors = tail (_actors w)}}

w :: World
w = World {_actors = [a1, a2]}

act :: Int -> World -> World
act n w = let a = (_actors w)!!n
          in (_act a) w

main = do
  let w1 = act 0 w
  let w2 = act 1 w
  mapM putStrLn (map show [w,w1,w2])


--data Actor w = Actor {act :: World -> World}


