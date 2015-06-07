{-# OPTIONS

  -XExistentialQuantification
  -XTupleSections
#-}

module Functions where
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

nodeToTree :: a -> T.Tree a
nodeToTree x = T.Node x []

makeF2 :: Atom -> Formula -> Formula -> Formula
makeF2 at x y = T.Node at [x, y]

_var :: Int -> Formula 
_var n = nodeToTree (AVar n)

_num :: Int -> Formula
_num n = nodeToTree (AInt n)

_range' = AStr "range"

_range ::  Formula -> Formula -> Formula
_range = makeF2 _range'

_range2 x y = _range (_num x) (_num y)

_drange' = AStr "drange"

_drange ::  Formula -> Formula -> Formula
_drange = makeF2 _drange'

_mlist' = AStr "List"
_mlist = T.Node _mlist'

_singleton :: Int -> Formula
_singleton n = _mlist [_num n]

_replicate' = AStr "replicate"
_replicate = makeF2 _replicate'

_replicate2 x y = _replicate  (_num x) (_num y)

_fun' = AStr "fun"
_fun = makeF2 _fun'

_concatMap' = AStr "concatMap"
_concatMap = makeF2 _concatMap'

_apply' = AStr "$"
_apply = makeF2 _apply'

{-
makeFormulaFunc :: Atom -> ([Int] -> Formula)
makeFormulaFunc f = (T.Node f) . (map (\x -> T.Node (AInt x) []))

-}
