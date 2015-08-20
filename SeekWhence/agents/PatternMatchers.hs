{-# OPTIONS

  -XExistentialQuantification
  -XRank2Types
  -XTupleSections
#-}

{-# LANGUAGE TemplateHaskell #-}

-- | Agents created from formulas. Each of these agents has a set of formulas, and it tries to apply the formulas to build upon existing structures. For example, 'ranger' would try to apply the formula [1..n]++[n+1] = [1..(n+1)] to build longer ranges.
module PatternMatchers where

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
import Control.Lens hiding ((|>))

import TreeState
import Utilities
import ParseUtilities
import IMap
import GraphUtils
import RandomUtils
import Formulas
import MaybeUtils
import Functions

import Mind
import Workspace

-- * Pattern Matchers in the abstract

data PMMemory = PMMemory {_childStructs :: S.Set Int} deriving Show
--only include the structures at the top.

makeLenses ''PMMemory

getPMActivations :: Bool -> Mind Workspace mes -> [Int] -> [((Int, Structure), Double)]
getPMActivations b w ids = 
    map (\cid -> 
                 let 
                     str = w ^. (workspace . board . nodeIndex cid) 
                 in 
                   ((cid, str), lclamp 0.5 $ fromIntegral (_length str) + totalMod (_modifiers str))) ids ++ 
        (if b then (map (\cid -> 
                 let 
                     str = w ^. (workspace . board . nodeIndex cid) 
                 in 
                   ((cid, str), lclamp 0.5 $ fromIntegral (_length str) + totalMod (_modifiers str)))
            (filter (\x -> null $ ((_atIndex $ _workspace w) MM.! x) `L.intersect` ids) [1..(length $ _list $ _workspace w)])) else [])

{-| Find the activation of child structures. The bool is whether to include singletons. -}
getChildPMActivations :: Bool -> Mind Workspace mes -> Agent' (Mind Workspace mes) PMMemory mes -> [((Int, Structure), Double)]
getChildPMActivations b w a = getPMActivations b w (S.toList $ _childStructs $ _memory a)
        

makeCombineRule' :: (Show a) => (MTreeState', MTreeState', [Int] -> [Int] -> Maybe a) -> Formula -> Formula -> Maybe a
makeCombineRule' (m1, m2, f) f1 f2 = 
    case (patternMatch' m1 f1, patternMatch' m2 f2) of
      (Just li1, Just li2) -> f li1 li2 --`debug` ("CR:"++(show (li1, li2)))
      _ -> Nothing --`debug` ("No!" ++ (show (patternMatch' m1 f1, patternMatch' m2 f2)))

--debugging...
makeCombineRules' :: (Show a) => [(MTreeState', MTreeState', [Int] -> [Int] -> Maybe a)] -> Formula -> Formula -> Maybe a
makeCombineRules' li f1 f2 = foldl (orElse) Nothing (map (\x -> makeCombineRule' x f1 f2) li) -- |> debugShow

{-| Given a rule for combining formulas, tries to combine the formulas at the given location in the workspace. If successful it returns Just the new workspace along with the index of the new formula. Else returns Nothing. -}
combineRuleToAction :: (Formula -> Formula -> Maybe Formula) -> (Int, Structure) -> (Int, Structure) -> Workspace -> Maybe (Workspace, Int)
combineRuleToAction f (n1, str1) (n2, str2) wk = 
      do
        newStr <- f (_formula str1) (_formula str2) 
        return $ addFormulaOn [n1,n2] newStr wk        
--make it report success?

makeFormulaAgent :: String -> (Formula -> Formula -> Maybe Formula) -> Agent' (Mind Workspace mes) PMMemory mes
makeFormulaAgent myName action = 
    Agent' {_name = myName,
            --look at total strength of child structures
            _scout = \w a -> (,a) $ sum $ map snd $ getChildPMActivations False w a,
            _act = \w a ->
                   let
                       r::Double
                       (r,w') = getRandom (0,1) w -- x::Double
                       r'::Double
                       (r',w'') = getRandom (0,1) w' 
                       structs = getChildPMActivations True w a -- |> debugShow
                       (n1, str1) = (chooseByProbs r $ normalizeList structs) 
                       l_r = L.filter (\((i, _), _) -> or $ map (\x -> i `elem` ((w ^. (workspace.atIndex)) MM.! x)) [(_start (str1) - 1), (_end (str1) + 1)]) structs 
--careful of overlapping
                   in
                     if null l_r 
                     then (w'', a)
                     else 
                         let 
                             (n2, str2) = (chooseByProbs r' $ normalizeList l_r)
                             ((n1',str1'), (n2', str2')) = (if (_start str1 < _start str2) then id else swap) ((n1,str1), (n2, str2)) -- |> debugShow
                         in
                           case (combineRuleToAction action) (n1', str1') (n2', str2') (_workspace w) of
                             Just (wk', i) -> 
                                 (w'' |> set workspace wk', 
                                  a & memory . childStructs %~ (S.delete n1' .
                                                                S.delete n2' .
                                                                S.insert i))
                             Nothing -> (w'',a),
            _memory = PMMemory S.empty,
            _inbox = []}

makePatternMatcher' :: String -> [(MTreeState', MTreeState', [Int] -> [Int] -> Maybe Formula)] -> Agent' (Mind Workspace mes) PMMemory mes
makePatternMatcher' myName li = makeFormulaAgent myName (makeCombineRules' li)
                                              
-- * Concrete instantiations

rangerPattern = parsePattern "range(?1,?2)"
singlePattern = parsePattern "List(?1)"
replPattern = parsePattern "replicate(?1,?2)"

ranger :: Agent' (Mind Workspace mes) PMMemory mes
ranger = makePatternMatcher' "ranger" 
                             (map (\(x,y) -> (x,y,\li1 li2 -> if last li1 + 1 == head li2 then Just (_range2 (head li1) (last li2)) else Nothing)) ((,) <$> [rangerPattern, singlePattern] <*> [rangerPattern, singlePattern]))
--assume the right lengths!

replicator :: Agent' (Mind Workspace mes) PMMemory mes
replicator =
    let
        f = \li1 li2 -> if (li1!!1) == (li2!!0) then Just $ _replicate2 ((li1!!0) + 1) (li1!!1) else Nothing
    in
      makePatternMatcher' "replicator"
                          [(replPattern, singlePattern, f),
                           (singlePattern, replPattern, flip f),
                           (replPattern, replPattern, \li1 li2 -> if (li1!!1) == (li2!!1) then Just $ _replicate2 ((li1!!0)+(li2!!0)) (li1!!1) else Nothing),
                           (singlePattern, singlePattern, \li1 li2 -> if (li1!!0) == (li2!!0) then Just $ _replicate2 2 (li1!!0) else Nothing)]
