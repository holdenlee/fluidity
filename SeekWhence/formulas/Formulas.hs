module Formulas where

import qualified Data.Map.Strict as M
import qualified Data.Tree as T
import qualified Data.Set as S

import Utilities
import Search

{-| An atom is either the name of a function, an integer (the only basic type right now), or a variable. -}
data Atom = AStr String | AInt Int | AVar Int deriving (Show, Eq, Ord)

{-| A formula is a tree of atoms, for example, [1..5] would be Node (AStr [Node (AInt 1) [], Node (AInt 5) []]) -}
type Formula = T.Tree Atom

{-| map from symbol to display rule -}
type SymbolLib = M.Map String String

{-| All functions that Mind comprehends are here.-}
symLib :: SymbolLib
symLib = M.fromList [("range", "[?1..?2]"),
                     ("replicate", "(replicate ?1 ?2)"),
                     ("concatMap", "(concatMap ?1 ?2)"),
                     ("concatReplicate", "(concat $ replicate ?1 ?2)"),
                     ("fun", "(\\?1 -> ?2)"),
                     ("List", "[?args]")
                    ]

atomToInt :: Atom -> Int
atomToInt at = case at of
                 AInt x -> x
                 AVar y -> y
                 _ -> -1

formulaToInt :: Formula -> Int
formulaToInt = atomToInt . root

getUsedVars :: Formula -> S.Set Int
getUsedVars (T.Node x li) = 
    S.unions $
               (case x of 
                  AVar n -> S.singleton n
                  _ -> S.empty):(map getUsedVars li)

getMax :: S.Set Int -> Int
getMax s = 
    case (fmap fst $ S.maxView $ s) of
      Nothing -> 1
      Just n -> n+1

getUnusedVar :: Formula -> Int
getUnusedVar = getMax . getUsedVars

--should use template haskell to generate these...
isAStr (AStr _) = True
isAStr _ = False

isAInt (AInt _) = True
isAInt _ = False

isAVar (AVar _) = True
isAVar _ = False
