{-# OPTIONS

  -XExistentialQuantification
  -XTupleSections
#-}

module Workspace where
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
import Formulas

data Structure = Structure {_formula :: T.Tree Atom, 
                            _start :: Int,
                            _end :: Int}

instance Show Structure where
    show str = showFormula symLib $ _formula str

_length :: Structure -> Int
_length str = _end str - _start str + 1

data EdgeType = Group deriving (Show)

data Workspace = Workspace {_list :: [Int],
                            board :: G.Gr Structure EdgeType,
                            tops :: S.Set Int, --the nodes of the toplevel structures
                            atTop :: MM.MultiMap Int Int
                           }

instance Show Workspace where
    show = prettify . board

--get id of top structure at index
getTopStructureAt :: Int -> Workspace -> Int
getTopStructureAt i wk =
    (MM.lookup i $ atTop wk)!!0
--this is not good - let's assume only 1 atTop structure for each.

--add a structure on top of structures
addFormulaOn :: [Int] -> Formula -> Workspace -> (Workspace, Int)
addFormulaOn li f wk = 
    let
        b = board wk
        newN = (newNodes 1 b)!!0
        oldStrs = L.map (appendFun (fromJust . (G.lab b))) li 
        nowHiddens = L.concatMap (\(i,old) -> L.map (,i) [(_start old)..(_end old)]) oldStrs
        --list should be in order!
        newStart = _start $ fromJust $ G.lab b $head li
        newEnd = _end $ fromJust $ G.lab b $ last li
        str = Structure{_formula = f, _start = newStart, _end=newEnd}
    in
      wk{board = board wk |> G.insNode (newN, str)
                          |> (G.insEdges $ L.map (\x -> (newN, x, Group)) li),
         tops = tops wk |> foldIterate S.delete li,
         atTop = atTop wk |> foldIterate (MM.delete) (L.map fst nowHiddens)
                          |> foldIterate (uncurry MM.insert) (L.map (,newN) [newStart..newEnd])} |> (,newN)


singletonList :: Int -> Formula
singletonList n = T.Node (AStr "List") [T.Node (AInt n) []]

singletonStr :: (Int, Int) -> Structure
singletonStr (i, n) = Structure{_formula = singletonList n,
                             _start = i,
                             _end = i}

listToWorkspace :: [Int] -> Workspace
listToWorkspace li = Workspace{_list = li,
                               board = (G.empty |> insNodes (zip [1..length li] $ map singletonStr $ enumerate li)), 
                               tops = (S.fromList [1..length li]),
                               atTop = MM.fromList $ zip [1..length li] [1..length li]}
--each is represented by a singleton list

makeFormulaFunc :: Atom -> ([Int] -> Formula)
makeFormulaFunc f = (T.Node f) . (map (\x -> T.Node (AInt x) []))

--makeFormulaFunc2 :: Atom -> ([Atom] -> Formula)
--makeFormulaFunc2 f = (T.Node f) . (map (\x -> T.Node x []))

--figure out how to do this with template haskell :P
_range' = AStr "range"
_range = makeFormulaFunc _range'

_drange' = AStr "drange"
_drange = makeFormulaFunc _drange'

_mlist' = AStr "List"
_mlist = makeFormulaFunc _mlist'

_replicate' = AStr "replicate"
_replicate = makeFormulaFunc _replicate'

_concatMap' = AStr "concatMap"
--_concatMap = makeFormulaFunc2 _concatMap'

_apply' = AStr "->"
--_apply = makeFormulaFunc2 _apply' 
--need to do reflection!

--this is annoying to write out. Find nicer way?
rangerf :: Formula -> Formula -> Maybe Formula
rangerf f g = 
    case f of T.Node _range' [T.Node (AInt a) [], T.Node (AInt b) []] -> 
                  case g of T.Node _mlist' [T.Node (AInt c) []] ->
                                if c == b+1 then Just (_range [a,c]) else Nothing
                            T.Node _range' [T.Node (AInt c) [], T.Node (AInt d) []] ->
                                if c == b+1 then Just (_range [a,d]) else Nothing
                            _ -> Nothing
              T.Node _mlist' [T.Node (AInt a) []] -> 
                  case g of T.Node _mlist' [T.Node (AInt c) []] ->
                                if c == a+1 then Just (_range [a,c]) else Nothing
                            _ -> Nothing 
              _ -> Nothing

replicatorf :: Formula -> Formula -> Maybe Formula
replicatorf f g = 
    case f of T.Node _replicate' [T.Node (AInt t) [], T.Node (AInt n) []] -> 
                  case g of T.Node _mlist' [T.Node (AInt n') []] ->
                                if n' == n then Just (_replicate [t+1,n]) else Nothing
                            T.Node _replicate' [T.Node (AInt t') [], T.Node (AInt n') []] ->
                                if n' == n then Just (_replicate [t+t',n]) else Nothing
                            _ -> Nothing
              T.Node _mlist' [T.Node (AInt a) []] -> 
                  case g of T.Node _mlist' [T.Node (AInt c) []] ->
                                if c == a then Just (_replicate [2,a]) else Nothing
                            _ -> Nothing
              _ -> Nothing

--Should each structure have a strength? Relative importance? Happiness? Salience? (weighted average of importance and unhappiness)
--Formula -> Formula -> Maybe Formula

--get the next top structure in list order (as opposed to the next *element*)
getNextStructure :: Int -> Workspace -> Maybe Int
getNextStructure i wk = 
    let
        b = board wk 
    in
      (MM.lookup (_end (fromJust $ G.lab b i) + 1) (atTop wk)) `mindex` 0

combineRuleToAction :: (Formula -> Formula -> Maybe Formula) -> Int -> Workspace -> Maybe (Workspace, Int)
combineRuleToAction f i wk = 
    do
      let b = board wk
      l <- G.lab b i
      i' <- getNextStructure i wk
      l' <- G.lab b i'
      -- l' <- (MM.lookup atTop (_end l + 1)) `index` 0
      result <- f (_formula l) (_formula l')
      return (addFormulaOn [i,i'] result wk)

(.|) :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
f .| g = \x -> 
         case f x of 
           Just y -> Just y
           Nothing -> 
               case g x of
                 Just z -> Just z
                 Nothing -> Nothing

(.&) :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
(.&) = (>=>)

tryDo :: (a -> Maybe a) -> a -> Maybe a
tryDo f x = case f x of
              Just y -> Just y
              Nothing -> Just x

moveRight :: Workspace -> Int -> Maybe Int
moveRight = flip getNextStructure

