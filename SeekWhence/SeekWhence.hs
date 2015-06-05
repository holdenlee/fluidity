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

wkFToWoF :: (Workspace -> Maybe Workspace) -> World Workspace -> Maybe (World Workspace)
wkFToWoF f world = 
    do
      newWk <- f (workspace world)
      return world{workspace= newWk}

mPair :: (a -> Maybe a) -> (b -> Maybe b) -> ((a,b) -> Maybe (a,b))
mPair f g (x,y) = 
    case f x of 
      Nothing -> Nothing
      Just x' ->
          case g y of
            Nothing -> Nothing
            Just y' -> Just (x',y')

ranger' = Codelet' {act = \ac wo i ->
                          let 
                              f1 = (wkFToWoF (combineRuleToAction rangerf i)) `mPair` return
                              f2 = return `mPair` (moveRight (workspace wo))
                          in
                            (tryDo (f1 .| f2) (wo,i), []),
                    _activation = \i wo -> (fromIntegral i) / (fromIntegral $ length $ _list (workspace wo)),
                    memory = 1}

ranger = Codelet ranger'
