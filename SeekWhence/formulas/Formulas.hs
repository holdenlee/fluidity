module Formulas where
{-import System.Environment
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
import System.Random-}
import qualified Data.Map.Strict as M
import qualified Data.Tree as T

import Utilities
import Search

data Atom = AStr String | AInt Int | AVar Int deriving (Show, Eq, Ord)

type Formula = T.Tree Atom

--map from symbol to display rule
type SymbolLib = M.Map String String

symLib :: SymbolLib
symLib = M.fromList [("range", "[?1..?2]"),
                     ("replicate", "(replicate ?1 ?2)"),
                     ("concatMap", "(concatMap ?1 ?2)"),
                     ("concatReplicate", "(concat $ replicate ?1 ?2)"),
                     ("fun", "(\\?1 -> ?2)"),
                     ("List", "[?args]")
                    ]

