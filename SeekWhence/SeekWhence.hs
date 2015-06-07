{-# OPTIONS

  -XExistentialQuantification
  -XTupleSections
#-}

module SeekWhence where
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
import PTScan
import Workspace
import Formulas
import Search

wkFToWoF :: (Workspace -> Maybe (Workspace, Int)) -> World Workspace -> Maybe (World Workspace, Int)
wkFToWoF f world = 
    do
      (newWk, i) <- f (workspace world)
      return (world{workspace= newWk},i)

mPair :: (a -> Maybe a) -> (b -> Maybe b) -> ((a,b) -> Maybe (a,b))
mPair f g (x,y) = 
    case f x of 
      Nothing -> Nothing
      Just x' ->
          case g y of
            Nothing -> Nothing
            Just y' -> Just (x',y')

getTotalLengths :: String -> Workspace -> Int
getTotalLengths name wk =
    let
        count str = 
            case (_formula str) of T.Node (AStr name) li -> _length str
                                   _ -> 0
    in
      sum $ L.map (count . snd) $ labNodes (board wk)

{-
eval :: Formula -> [Int]
eval f = case f of
           T.Node (AStr "List") li -> L.map eval li
           T.Node (AStr "range") li -> atomsToRange li
           T.Node _ li -> getBaseSeq (last li)
-}
atomsToInts :: [Formula] -> [Int]
atomsToInts = map (\x -> 
                       case x of
                         T.Node (AInt y) [] -> y
                         _ -> 0)

atomsToRange li = let li' = atomsToInts li in [(li'!!0)..(li'!!1)]

atomsToDRange li = let li' = atomsToInts li in reverse [(li'!!0)..(li'!!1)]

--should fix this with a better typing system...
--things get more complicated when you allow map2. Ignore this for now.
getBaseSeq :: Formula -> [Int]
getBaseSeq f =
    case f of 
      T.Node (AStr "List") li -> atomsToInts li
      T.Node (AStr "range") li -> atomsToRange li
      T.Node (AStr "$") li -> atomsToRange li --NEED TO FIX
      T.Node _ li -> getBaseSeq (last li)
--replicate is not in the same framework...

combineRuleToLRCodelet :: (Formula -> Formula -> Maybe Formula) -> String -> Codelet Workspace
combineRuleToLRCodelet f name = Codelet (Codelet' {act = \ac wo i ->
                          let 
                              f1 = (wkFToWoF (combineRuleToAction f i)) . fst --`mPair` return
                              f2 = return `mPair` (moveRight (workspace wo))
                          in
                            (tryDo (f1 .| f2) (wo,i), []),
                    _activation = \i wo -> 1 + (fromIntegral $ getTotalLengths name (workspace wo)),
--(fromIntegral i) / (fromIntegral $ length $ _list (workspace wo)),
                    memory = 1})

ranger = combineRuleToLRCodelet rangerf "range"
replicator = combineRuleToLRCodelet replicatorf "replicate"

getUsedVars :: Formula -> S.Set Int
getUsedVars (T.Node x li) = 
    S.unions $
               (case x of 
                  AVar n -> S.singleton n
                  _ -> S.empty):(L.map getUsedVars li)

getMax :: S.Set Int -> Int
getMax s = 
    case (fmap fst $ S.maxView $ s) of
      Nothing -> 1
      Just n -> n+1

getUnusedVar :: Formula -> Int
getUnusedVar = getMax . getUsedVars

--justAns :: State a s -> s
--justAns x = snd . runState x

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
