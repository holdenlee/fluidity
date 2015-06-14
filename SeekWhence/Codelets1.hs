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

ranger = combineRuleToLRCodelet rangerf "range"
replicator = combineRuleToLRCodelet replicatorf "replicate"

type Substitution2 = M.Map Atom Atom

doNothing :: MState s ()
doNothing = StateT (\x -> Just ((), x))

generalizeStep :: MStater (TPath2 Atom, TPath2 Atom) (Substitution2, Bool)
generalizeStep (sub, b) = 
    StateT (\(tp1, tp2) -> 
                let 
--State c (b,String)
                    ((at1, s1), tp1') = runState dFSStep2 tp1 
                    ((at2, s2), tp2') = runState dFSStep2 tp2 
                in
                  --Just ((sub, False), (tp1',tp2')))
                  if s1 /=s2 
                  then Nothing 
                  else 
                      if s1 == "done" 
                      then Just ((sub, False), (tp1',tp2')) 
                      else 
                         if (cur tp1' `M.member` sub) 
                         then 
                             if M.lookup (cur tp1') sub == Just (cur tp2') then Just ((sub, True), (tp1', tp2')) else Nothing
                         else
                             case cur tp1' of 
                               AVar m -> 
                                   case M.lookup (AVar m) sub of
                                     Just (AVar n) -> Just ((M.insert (AVar m) (AVar n) sub, True), (tp1', tp2'))
                                     _ -> Nothing
--if the integers match
                               AInt m -> 
                                   if any (\x -> case x of 
                                                   AInt _ -> True
                                                   _ -> False) $ M.keys sub 
                                   then Nothing
                                   else 
                                       case M.lookup (AInt m) sub of
                                         Just (AInt n) -> Just ((M.insert (AInt m) (AInt n) sub, True), (tp1', tp2'))
                                         _ -> Nothing)

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

