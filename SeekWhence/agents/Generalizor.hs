{-# OPTIONS

  -XExistentialQuantification
  -XRank2Types
  -XTupleSections
#-}

{-# LANGUAGE TemplateHaskell #-}

-- | Agent who tries to generalize a formula.
module Generalizor where

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
import Control.Monad.Trans.State
import qualified Data.Tree as T

import TreeState
import Utilities
import ParseUtilities
import IMap
import GraphUtils
import RandomUtils
import Formulas
import MaybeUtils
import Functions
import Search
import PatternMatchers
import TreeState

import Mind 
import Workspace

generalize :: Formula -> Formula -> Maybe Formula
generalize f1 f2 = 
    do
            let startTPs = (start f1, start f2)
            let unused = max (getUnusedVar f1) (getUnusedVar f2)
            let ms = repeatUntilState (M.empty, True) (\(_,b) -> not b) (generalizeStep unused)
            ((sub, _), (tp1,tp2)) <- runStateT ms startTPs
            let f1' = zipUp tp1
            if hasInt sub
            --get the first! 
            then 
                let (y,z) = (mapMaybe (\x -> case x of 
                                               (AInt y, AInt z) -> Just (y,z)
                                               _ -> Nothing) $ M.assocs sub)!!0
                in
                  return $ _concatMap (_fun (_var unused) f1') (_mlist [_num y, _num z])
            else 
                return $ _concatReplicate f1' (_num 2) 

specialize :: Atom -> Formula -> Formula -> Maybe Formula
specialize var f1 =
    patternMatch (formulaToPattern $ fmap (\a -> if a==var then AStr "?1" else a) f1) >=> (mlookup 0)

concatMapPattern = parsePattern "concatMap(fun(?0,?1),?2)"

trivialPattern = parsePattern "?0"

--can't match [?args] right now...
extendConcatMap :: Formula -> Formula -> Maybe Formula
extendConcatMap =
    let
        combineF f = (\li1 li2 -> do
                        val <- specialize (T.rootLabel (li1!!0)) (li1!!1) (li2!!0)
                        case (li1!!2) of
                          T.Node (AStr "List") li3 -> return $ _concatMap (_fun (li1!!0) (li1!!1)) (_mlist (f val li3))
                          _ -> Nothing)
    in
      makeCombineRules [(concatMapPattern, trivialPattern, combineF (\v li -> li ++ [v])),
                        (trivialPattern, concatMapPattern, flip $ combineF (:))]

generalizorf :: Formula -> Formula -> Maybe Formula
generalizorf f1 f2 = 
    case (patternMatch' singlePattern f1, patternMatch' singlePattern f2) of
      (Nothing, Nothing) -> generalize f1 f2 `orElse` extendConcatMap f1 f2
      _ -> Nothing

--Bool is whether to keep going.
generalizeStep :: Int -> MStater (TPath2 Atom, TPath2 Atom) (Substitution2, Bool)
generalizeStep k (sub, b) = 
    StateT (\(tp1, tp2) -> 
                let 
                    --do 1 step of depth-first search on both fomulas
                    ((at1, s1), tp1') = runState dFSStep2 tp1 
                    ((at2, s2), tp2') = runState dFSStep2 tp2 
                in
                  if s1 /=s2 
                  then Nothing  --the arities of the formulas at the current node do not match 
                  else 
                      if s1 == "done" --indicate that we are to stop
                      then Just ((sub, False), (tp1',tp2')) 
                      else 
                          case (cur tp1') of
                            AVar m -> --if it's a variable
                                   case M.lookup (AVar m) sub of
                                     Just vn -> 
                                         if cur tp2' == vn then Just ((sub, True), (tp1', tp2')) else Nothing --make sure the variables match up.
                                     Nothing -> 
                                         case (cur tp2') of 
                                           AVar n -> Just ((M.insert (AVar m) (AVar n) sub, True), (tp1', tp2')) --if this is the first time it is encountered, add to map.
                                           _ -> Nothing --if it's not a variable, fail
                            AInt m -> 
                                case (cur tp2') of
                                  AInt n -> if m==n 
                                            then Just ((sub, True), (tp1', tp2')) 
                                            else
                                                case M.lookup (AInt m) sub of
                                                  Just vn -> 
                                                      if AInt n == vn then Just ((sub, True), (tp1' |> changeMe (_var k), tp2' |> changeMe (_var k))) else Nothing --make sure the ints match up.
                                                  Nothing -> 
                                                      if hasInt sub 
                                                      then Nothing --if there's already an integer, fail. (Only generalize 1 integer at a time, at least for now.)
                                                      else Just ((M.insert (AInt m) (AInt n) sub, True), (tp1' |> changeMe (_var k), tp2' |> changeMe (_var k)))
                                  _ -> Nothing 
                            _ -> if (cur tp1' == cur tp2') then Just ((sub, True), (tp1', tp2')) else Nothing
           )

generalizor :: Agent' (Mind Workspace mes) PMMemory mes
generalizor = 
    Agent' {_name = "generalizor",
            --look at total strength of top-level structures excluding singletons
            _scout = \w a -> (,a) $ sum $ map snd $ getPMActivations False w (filter (> (length $ _list $ _workspace w)) (S.toList $ _tops $ _workspace w)),
            _act = chooseTwoAndTryCombine
                           (\w a -> getPMActivations False w (filter (> (length $ _list $ _workspace w)) (S.toList $ _tops $ _workspace w)))
                           (\structs str1 w a -> getPMActivations False w (filterIDs (\l r -> r == str1 ^. begin - 1 || l == str1 ^. end + 1) w (map (fst . fst) structs)))
                           generalizorf,
            _memory = point,
            _inbox = []}

--hacky right now
--scout (\w a -> (,a) $ sum $ map snd $ getPMActivations False w (map fst $ G.labNodes $ _board $ _workspace w))
--only combines 2

{-
import Generalizor
import Data.Maybe
import Formulas
import MathParser

let f3 = fromJust $ generalizorf (_range2 1 2) (_range2 1 3)

putStrLn (showFormula symLib f3)
-}
