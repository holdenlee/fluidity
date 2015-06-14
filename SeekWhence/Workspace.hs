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
import Functions
import TreeState
import MathParser

data Structure = Structure {_formula :: T.Tree Atom, 
                            _start :: Int,
                            _end :: Int,
                            _strength :: Double
                           }

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
        str = Structure{_formula = f, _start = newStart, _end=newEnd, _strength = fromIntegral $ newEnd - newStart + 1}
        --default strength is length
    in
      wk{board = board wk |> G.insNode (newN, str)
                          |> (G.insEdges $ L.map (\x -> (newN, x, Group)) li),
         tops = tops wk |> foldIterate S.delete li,
         atTop = atTop wk |> foldIterate (MM.delete) (L.map fst nowHiddens)
                          |> foldIterate (uncurry MM.insert) (L.map (,newN) [newStart..newEnd])} |> (,newN)

singletonStr :: (Int, Int) -> Structure
singletonStr (i, n) = Structure{_formula = _singleton n,
                             _start = i,
                             _end = i,
                             _strength = 1}

listToWorkspace :: [Int] -> Workspace
listToWorkspace li = Workspace{_list = li,
                               board = (G.empty |> insNodes (zip [1..length li] $ map singletonStr $ enumerate li)), 
                               tops = (S.fromList [1..length li]),
                               atTop = MM.fromList $ zip [1..length li] [1..length li]}
--each is represented by a singleton list

rangerPattern = parsePattern "range(?1,?2)"
singlePattern = parsePattern "List(?1)"
replPattern = parsePattern "replicate(?1,?2)"

--this is annoying to write out. Find nicer way?
rangerf :: Formula -> Formula -> Maybe Formula
rangerf f g = 
    let
        singlef = patternMatch' singlePattern f
        rangef = patternMatch' rangerPattern f
        singleg = patternMatch' singlePattern g
        rangeg = patternMatch' rangerPattern g
    in
      case (singlef, rangef, singleg, rangeg) of
        (_, Just [a,b], Just [c], _) -> if c == b+1 then Just (_range2 a c) else Nothing
        (_, Just [a,b], _ , Just [c,d]) -> if c == b+1 then Just (_range2 a d) else Nothing
        (Just [a], _, Just [b], _) -> if b== a + 1 then Just (_range2 a b) else Nothing
        _ -> Nothing

replicatorf :: Formula -> Formula -> Maybe Formula
replicatorf f g = 
    let
        singlef = patternMatch' singlePattern f
        replf = patternMatch' replPattern f
        singleg = patternMatch' singlePattern g
        replg = patternMatch' replPattern g
    in
      case (singlef, replf, singleg, replg) of
        (_, Just [t,n], Just [n'], _) -> if n == n' then Just (_replicate2 (t+1) n) else Nothing
        (_, Just [t,n], _ , Just [t',n']) -> if n == n' then Just (_replicate2 (t+t') n) else Nothing
        (Just [n], _, Just [n'], _) -> if n==n' then Just (_replicate2  2 n) else Nothing
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

