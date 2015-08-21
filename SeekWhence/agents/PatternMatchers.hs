{-# OPTIONS

  -XExistentialQuantification
  -XRank2Types
  -XTupleSections
#-}

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}

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
import Modifiers

-- * Pattern Matchers in the abstract

data PMMemory = PMMemory {_pMMemoryChildStructs :: S.Set Int,
                          _pMMemoryModifiers :: ModifierMap} deriving Show
--only include the structures at the top.

instance Pointed PMMemory where
    point = PMMemory S.empty M.empty

makeFields ''PMMemory

filterIDs :: (Int -> Int -> Bool) -> Mind Workspace mes -> [Int] -> [Int]
filterIDs f m = filter (\i -> 
                            let str = m ^. (workspace . board . nodeIndex i)
                            in f (str ^. begin) (str ^. end))
{-filter (\(_,str) -> f (str ^. begin) (str ^. end)) . map (appendFun (\i -> m ^. (workspace . board . nodeIndex i)))
-}
{-
filter (\i -> 
                            let str = m ^. (workspace . board . nodeIndex i)
                            in f (str ^. begin) (str ^. end))
-}

mapIDs :: Mind Workspace mes -> [Int] -> [(Int, Structure)]
mapIDs m = map (appendFun (\i -> m ^. (workspace . board . nodeIndex i)))

{-| given list of base list indices, keep only the ones which avoid avoidIDs-}
filterNonIntersecting:: Mind Workspace mes -> [Int] -> [Int] -> [Int]
filterNonIntersecting m avoidIDs ids = filter (\x -> null $ ((m ^. (workspace . atIndex)) MM.! x) `L.intersect` avoidIDs) ids

getPMActivations :: Bool -> Mind Workspace mes -> [Int] -> [((Int, Structure), Double)]
getPMActivations b w ids = 
    map ((\(cid, str) -> ((cid, str), lclamp 0.5 $ fromIntegral (_length str) + totalMod (str ^. modifiers)))) $
         mapIDs w
          (ids ++ (if b then filterNonIntersecting w ids [1..(length $ _list $ _workspace w)] else []))

{-| Find the activation of child structures. The bool is whether to include singletons. -}
getChildPMActivations :: Bool -> Mind Workspace mes -> Agent' (Mind Workspace mes) PMMemory mes -> [((Int, Structure), Double)]
getChildPMActivations b w a = getPMActivations b w (S.toList $ (a ^. (memory . childStructs)))  

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
        newStr <- f (str1 ^. formula) (str2 ^. formula) 
        return $ addFormulaOn [n1,n2] newStr wk        
--make it report success?

-- ? (HasChildStructs mem) =>
{-| 
  f1 is the options for the first structure. 
  f2 is the options for the second one, given the list of options for first structure and the first structure chosen.
  combineF is how to combine the two structures.
-}
chooseTwoAndTryCombine :: ((Mind Workspace mes) -> Agent' (Mind Workspace mes) PMMemory mes -> [((Int, Structure), Double)]) -> 
                          ([((Int, Structure), Double)] -> Structure -> (Mind Workspace mes) -> Agent' (Mind Workspace mes) PMMemory mes -> [((Int, Structure), Double)]) ->
                          (Formula -> Formula -> Maybe Formula) -> 
                          Action (Mind Workspace mes) PMMemory mes
chooseTwoAndTryCombine f1 f2 combineF w a =
    let
        r::Double
        (r,w') = getRandom (0,1) w -- x::Double
        r'::Double
        (r',w'') = getRandom (0,1) w' 
        structs = f1 w a
        (n1, str1) = (chooseByProbs r $ normalizeList structs) 
        l_r = f2 structs str1 w a 
--careful of overlapping
    in
      if null l_r 
      then (w'', a)
      else 
          let 
              (n2, str2) = (chooseByProbs r' $ normalizeList l_r)
              ((n1',str1'), (n2', str2')) = (if (str1 ^. begin < str2 ^. begin) then id else swap) ((n1,str1), (n2, str2))
          in
            case (combineRuleToAction combineF) (n1', str1') (n2', str2') (_workspace w) of
              Just (wk', i) -> (w'' |> set workspace wk'
                                    |> over (workspace . tops) (S.delete n1' . S.delete n2' . S.insert i),
                                a & memory . childStructs %~ (S.delete n1' . S.delete n2' . S.insert i))
--modify the workspace, and modify the memory of agent a to remove the previous childStructs from memory
              Nothing -> (w'',a)
                          

makeFormulaAgent :: String -> (Formula -> Formula -> Maybe Formula) -> Agent' (Mind Workspace mes) PMMemory mes
makeFormulaAgent myName action = 
    Agent' {_name = myName,
            --look at total strength of child structures
            _scout = \w a -> (,a) $ sum $ map snd $ getChildPMActivations False w a,
            _act = chooseTwoAndTryCombine
                           (getChildPMActivations True)
                           (\structs str1 w a -> L.filter (\((i, _), _) -> or $ map (\x -> i `elem` ((w ^. (workspace.atIndex)) MM.! x)) [((str1 ^. begin) - 1), ((str1 ^. end) + 1)]) structs)
                           action,
            _memory = point,
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
