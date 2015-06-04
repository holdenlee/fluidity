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
import Data.Tree as T

import Utilities
import ParseUtilities
import IMap

data Atom = AStr String | AInt Int | AVar Int

type Formula = T.Tree Atom

type Structure = T.Tree Atom

data EdgeType = Group 

data Workspace = Workspace {board :: G.Gr Structure EdgeType,
                            tops :: S.Set Int --the nodes of the toplevel structures
                           }

singletonList :: Int -> Formula
singletonList n = T.Node (AStr "List") [T.Node (AInt n) []]

listToWorkspace :: [Int] -> Workspace
listToWorkspace li = Workspace (G.empty |> insNodes (zip [1..length li] $ map singletonList li)) (S.fromList [1..length li])
--each is represented by a singleton list

makeFormulaFunc :: Atom -> ([Int] -> Formula)
makeFormulaFunc f = (T.Node f) . (map (\x -> T.Node (AInt x) []))

--figure out how to do this with template haskell :P
_range' = AStr "range"
_range = makeFormulaFunc _range'

_drange' = AStr "drange"
_drange = makeFormulaFunc _drange'

_list' = AStr "List"
_list = makeFormulaFunc _list'

_replicate' = AStr "replicate"
_replicate = makeFormulaFunc _replicate'

ranger :: Formula -> Formula -> Maybe Formula
ranger f g = 
    case f of T.Node _range' [T.Node (AInt a) [], T.Node (AInt b) []] -> 
                  case g of T.Node _list' [T.Node (AInt c) []] ->
                                if c == b+1 then Just (_range [a,c]) else Nothing
                            T.Node _range' [T.Node (AInt c) [], T.Node (AInt d) []] ->
                                if c == b+1 then Just (_range [a,d]) else Nothing
                            _ -> Nothing
              _ -> Nothing

replicator :: Formula -> Formula -> Maybe Formula
replicator f g = 
    case f of T.Node _replicate' [T.Node (AInt t) [], T.Node (AInt n) []] -> 
                  case g of T.Node _list' [T.Node (AInt n') []] ->
                                if n' == n then Just (_replicate [t+1,n]) else Nothing
                            T.Node _replicate' [T.Node (AInt t') [], T.Node (AInt n') []] ->
                                if n' == n then Just (_range [t+t',n]) else Nothing
                            _ -> Nothing
              _ -> Nothing

--Formula -> Formula -> Maybe Formula
