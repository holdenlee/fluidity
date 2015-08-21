{-# OPTIONS

  -XExistentialQuantification
  -XRank2Types
#-}

module Actions where
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
import qualified Data.Graph.Inductive as G
import System.Random
import Control.Lens

import Utilities
import ParseUtilities
import IMap
import GraphUtils
import RandomUtils

import Mind

sendMessage :: Int -> mes -> (Mind wksp mes, Agent' (Mind wksp mes) mem mes) -> (Mind wksp mes, Agent' (Mind wksp mes) mem mes)
sendMessage i msg (m, a) = (m & ((agentIndex i) . ainbox) %~ (msg:), a)
