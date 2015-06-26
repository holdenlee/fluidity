{-# OPTIONS

  -XExistentialQuantification
  -XTupleSections
#-}

module Codelets1 where
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
import Control.Monad.Trans.State

import Utilities
import ParseUtilities
import IMap
import PTScan
import Workspace
import Formulas
import Search
import SeekWhence
import TreeState
import Functions

ranger = combineRuleToLRCodelet rangerf "range"
replicator = combineRuleToLRCodelet replicatorf "replicate"
generalizor = combineRuleToLRCodelet generalizorf ""
--activation of this is always 1 right now...

{-
getUnusedVar :: Formula -> Int
getUnusedVar f = 
    let
        fl = T.flatten f
        fl' = 0:(mapMaybe (\x -> case x of
                                    AInt m -> Just m
                                    _ -> Nothing) 
                 fl)
    in
      (L.maximum fl') + 1-}

type Substitution2 = M.Map Atom Atom

doNothing :: MState s ()
doNothing = StateT (\x -> Just ((), x))


generalizorf :: Formula -> Formula -> Maybe Formula
generalizorf f1 f2 = do
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


repeatUntilState :: b -> (b -> Bool) -> MStater a b -> MState a b
repeatUntilState startB testB ms = do
  curB <- ms startB
  if testB curB
    then return curB
    else repeatUntilState curB testB ms

hasInt :: Substitution2 -> Bool
hasInt = (any (\x -> case x of 
                       AInt _ -> True
                       _ -> False)) . M.keys

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

{-
generalizorf' :: (TPath2 Atom, TPath2 Atom) -> Maybe Formula
generalizorf' tp1 tp2 = 

generalizorf :: Formula -> Formula -> Maybe Formula
generalizorf f1 f2 = 
 -}   

{-
--n is unused var. mp is mapping between ints
generalize' :: Int -> (TPath Formula Atom, TPath Formula Atom, Maybe (Int,Int), Bool) -> Maybe (TPath Formula Atom, TPath Formula Atom, Maybe (Int,Int),Bool)
generalize' i (p1,p2,mp,b) = 
    if b --if finished
    then Just (p1,p2,mp,b)
    else
        case cur p1 of
          AInt m -> 
              case cur p2 of
                AInt n -> 
                    if m==n 
                    then 
                        let
                            (p1',b1) = dFSStep (p1,False)
                            (p2',b2) = dFSStep (p2,False)
                        in
                          generalize' i (p1',p2',mp,b1 && b2) --what happens if out of sync?
                    else 
                        case mp of
                          Just (m,n) -> 
                            let
                                (p1',b1) = dFSStep (p1 |> changeMe (T.Node (AVar i) []),False)
                                (p2',b2) = dFSStep (p2 |> changeMe (T.Node (AVar i) []),False)
                            in
                              generalize' i (p1',p2',mp,b1 && b2) --what happens if out of sync?
                          Nothing -> 
                            let
                                (p1',b1) = dFSStep (p1 |> changeMe (T.Node (AVar i) []),False)
                                (p2',b2) = dFSStep (p2 |> changeMe (T.Node (AVar i) []),False)
                            in
                              generalize' i (p1',p2',Just (m,n),b1 && b2) --what happens if out of sync?
                _ -> Nothing
          _ -> if cur p1 == cur p2 
               then 
                   let
                       (p1',b1) = dFSStep (p1,False)
                       (p2',b2) = dFSStep (p2,False)
                            in
                              generalize' i (p1',p2',mp,b1 && b2) --what happens if out of sync?
               else Nothing
-}
{-
generalize :: Formula -> Formula -> Maybe Formula
generalize f1 f2 = 
    do
      let m = max (getUnusedVar f1, getUnusedVar f2)
      (p1,p2,mp,b) <- generalize' m (start f1, start f2, Nothing, False)
      return $ _concatMap [_apply (AVar m) -}

--generalizor = combineRuleToLRCodelet generalize "range"

{-ranger' = Codelet' {act = \ac wo i ->
                          let 
                              f1 = (wkFToWoF (combineRuleToAction rangerf i)) . fst --`mPair` return
                              f2 = return `mPair` (moveRight (workspace wo))
                          in
                            (tryDo (f1 .| f2) (wo,i), []),
                    _activation = \i wo -> (fromIntegral i) / (fromIntegral $ length $ _list (workspace wo)),
                    memory = 1}

ranger = Codelet ranger'-}

