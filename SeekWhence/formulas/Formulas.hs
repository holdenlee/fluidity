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
import MathParser
import Search

data Atom = AStr String | AInt Int | AVar Int deriving (Show, Eq)

type Formula = T.Tree Atom

--map from symbol to display rule
type SymbolLib = M.Map String String

symLib :: SymbolLib
symLib = M.fromList [("range", "[?1..?2]"),
                     ("replicate", "(replicate ?1 ?2)"),
                     ("->", "(\\?1 -> ?2)"),
                     ("List", "[?args]")
                    ]

showFormula :: SymbolLib -> Formula -> String
showFormula slib f = 
  let 
  --get the symbol at the root 
    rt = 
        case root f of
          AStr str -> str
          AInt n -> (show n)
          AVar i -> "n_" ++ (show i)
  --look up the displayrule,
  --if you can't find it, use the default provided
    def = 
      if (null (children f)) then rt else (rt ++ "(" ++ "?args" ++ ")")
    drule = tryWithDefault (\sym -> M.lookup sym slib) def rt
  in (case (parseDispExpr drule (fmap (showFormula slib) (children f))) of
    Right s -> s
    Left _ -> "error")
