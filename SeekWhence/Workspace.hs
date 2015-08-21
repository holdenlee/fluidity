{-# OPTIONS

  -XExistentialQuantification
  -XTupleSections
  -XRank2Types
#-}

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}

module Workspace where
import System.Environment
import Control.Monad
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.MultiMap as MM
import Data.Maybe
import Data.Char
import qualified Data.Set as S
import Data.Tuple
import Data.Graph.Inductive as G
import System.Random
import Data.Tree as T
import Control.Lens hiding ((|>))

import Utilities
import ParseUtilities
import IMap
import Formulas
import Functions
import TreeState
import MathParser
import Modifiers

data Structure = Structure {_structureFormula :: T.Tree Atom, 
                            _structureStart :: Int,
                            _structureEnd :: Int,
                            _structureModifiers :: ModifierMap
                           }

makeFields ''Structure

instance Show Structure where
    show str = showFormula symLib $ (view formula) str

_length :: Structure -> Int
_length str = (view end) str - (view start) str + 1

data EdgeType = Group deriving (Show)

data Workspace = Workspace {_list :: [Int],
                            _board :: G.Gr Structure EdgeType,
                            _tops :: S.Set Int, --the nodes of the toplevel structures
                            _atIndex :: MM.MultiMap Int Int,
                            _atTop :: MM.MultiMap Int Int
                           }
--Should make more efficient by making a priority queue and only looking at the top few... TODO.

makeLenses ''Workspace

instance Pointed Workspace where
    point = {_list = [],
             _board = G.empty,
             _tops = S.empty,
             _atIndex = MM.empty,
             _atTop = MM.empty}

mmi :: (Ord a) => a -> Lens (MM.MultiMap a b) (MM.MultiMap a b) [b] b 
mmi i = lens (MM.! i) (flip (MM.insert i))

instance Show Workspace where
    show = prettify . _board

{-| get id of top structure at index -}
getTopStructureAt :: Int -> Workspace -> Int
getTopStructureAt i wk =
    (MM.lookup i $ _atTop wk)!!0
--let's assume for now there's only 1 atTop structure for each.

{-| add a structure on top of structures -}
addFormulaOn :: [Int] -> Formula -> Workspace -> (Workspace, Int)
addFormulaOn li f wk = 
    let
        b = _board wk
        newN = (newNodes 1 b)!!0
        oldStrs = L.map (appendFun (fromJust . (G.lab b))) li 
        nowHiddens = L.concatMap (\(i,old) -> L.map (,i) [(old ^. start)..(old ^. end)]) oldStrs
        --list should be in order!
        newStart = (view start) $ fromJust $ G.lab b $ head li
        newEnd = (view end) $ fromJust $ G.lab b $ last li
        str = Structure{_structureFormula = f, _structureStart = newStart, _structureEnd=newEnd, _structureModifiers = M.empty}
    in
      wk |> (over board (G.insNode (newN, str) |>> 
                        (G.insEdges $ L.map (\x -> (newN, x, Group)) li)))
         |> (over tops (foldIterate S.delete li))
         |> (over atTop (foldIterate (MM.delete) (L.map fst nowHiddens) .
                        (foldIterate (uncurry MM.insert) (L.map (,newN) [newStart..newEnd]))))
         |> (over atIndex (foldIterate ((flip MM.insert) newN) [newStart..newEnd]))
         |> (,newN)

singletonStr :: (Int, Int) -> Structure
singletonStr (i, n) = Structure{_structureFormula = _singleton n,
                                _structureStart = i,
                                _structureEnd = i,
                                _structureModifiers = M.empty}

listToWorkspace :: [Int] -> Workspace
listToWorkspace li = Workspace{_list = li,
                               _board = (G.empty |> insNodes (zip [1..length li] $ map singletonStr $ enumerate li)), 
                               _tops = (S.fromList [1..length li]),
                               _atTop = MM.fromList $ zip [1..length li] [1..length li],
                               _atIndex = MM.fromList $ zip [1..length li] [1..length li]}
--each is represented by a singleton list

{-| get the next top structure in list order (as opposed to the next *element*) -}
getNextTop :: Int -> Workspace -> Maybe Int
getNextTop i wk = 
    let
        b = _board wk 
    in
      (MM.lookup ((view end) (fromJust $ G.lab b i) + 1) (_atTop wk)) `mindex` 0

{-| get the previous top structure in list order (as opposed to the next *element*) -}
getPrevTop :: Int -> Workspace -> Maybe Int
getPrevTop i wk = 
    let
        b = _board wk 
    in
      (MM.lookup ((view start) (fromJust $ G.lab b i) - 1) (_atTop wk)) `mindex` 0

