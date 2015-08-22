{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XMultiWayIf
 -XFlexibleInstances
 -XFlexibleContexts
#-}

module TreeState (Substitution, Substitution2, MState, TreeState, TreeState', MStater, MTreeState, MTreeState', put2, replaceVal, joinWith, repeatUntilState, graftPattern, matchJustSymbol, matchJustSymbol', isInteger, formulaToPattern, parsePattern, patternMatch, patternMatch', hasInt) where
import System.Environment
import Control.Monad
import Data.Tree
import Control.Monad.Trans.State
import Data.Map.Strict as M
import Data.List as L

import Utilities
import Search
import MathParser
import Formulas

{-| A substitution tells us which atoms (typically variables) to replace with which formulas.-}
type Substitution = M.Map Atom Formula
type Substitution2 = M.Map Atom Atom

{-| A state that has a Maybe b. -}
type MState s b = StateT s Maybe b
{-| The state is given by a formula (the subtree that the parser is looking at). -}
type TreeState c = StateT Formula Maybe c
{-| The state is a formula, and the return value is a substitution. This is the workhorse of pattern matching in a tree. -}
type TreeState' = TreeState Substitution
type MStater s b = b -> MState s b
type MTreeState c = c -> TreeState c
type MTreeState' = Substitution -> TreeState'

-- * Basic functions for MState
put2 :: s -> b -> MState s b
put2 s' b' = state $ (\_ -> (b',s'))

replaceVal :: MState s c -> b -> MState s b
replaceVal s1 x = s1 >> (return x)

{-| Given a way to generate children from a state (ex. if a state is a tree), and a list of functions corresponding to the children, chain them together to make another function. -}
joinWith :: (s -> [s]) -> [MStater s b] -> MStater s b
joinWith childs staters b1 =
    do
      s1 <- get
      let ss = childs s1
      --put the state in and then do g.
      (foldl1 (>=>) $ L.map (\(si, g) -> (put2 si) >=> g) $ zip ss staters) b1

repeatUntilState :: b -> (b -> Bool) -> MStater a b -> MState a b
repeatUntilState startB testB ms = do
  curB <- ms startB
  if testB curB
    then return curB
    else repeatUntilState curB testB ms

-- * Functions for MTreeState.
{-| joinWith specialized to MTreeState. Give the function corresponding to the root and to the children.  -}
graftPattern :: MTreeState c -> [MTreeState c] -> MTreeState c
graftPattern rootF childFs = joinWith (\input -> input:(children input)) (rootF:childFs)

{-| try to match the current node (root of the current tree) with the atom. Returns Nothing if it fails. -}
matchJustSymbol :: Atom -> TreeState ()
matchJustSymbol atom = StateT (\f -> if root f == atom 
                                     then Just ((),f) 
                                     else Nothing)

matchJustSymbol' :: Atom -> MTreeState'
matchJustSymbol' atom = replaceVal (matchJustSymbol atom)

matchVar :: Atom -> Substitution -> TreeState Substitution
matchVar x s = StateT (\f->
  case M.lookup x s of
      Nothing -> Just (M.insert x f s, f) --insert the "x=f" in the substitution
      Just f2 ->
        if f2 == f
          then Just (s, f) --"x=f" is already in the substitution
          else Nothing --"x=g, g/=f" is in the substitution, so fail.
      )

--from http://rosettacode.org/wiki/Determine_if_a_string_is_numeric#Haskell
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

formulaToPattern :: Formula -> MTreeState'
formulaToPattern tr = 
    let 
      symb = root tr
      cs = children tr
      patts = fmap formulaToPattern cs
      rootPattern = case symb of
                      AStr ('?':_) -> matchVar symb --vs. JustVar
                      _     -> matchJustSymbol' symb
    in
      graftPattern rootPattern patts 

{-| Use this to create pattern matchers, for example 
     parsePattern "range(?1,?2)"
See PatternMatchers.hs.
-}
parsePattern :: String -> MTreeState'
parsePattern = formulaToPattern . parseFun

patternMatch :: MTreeState' -> Formula -> Maybe [Formula]
patternMatch mts f = 
    do
      subs <- evalStateT (mts M.empty) f
      return $ elems subs

patternMatch' :: MTreeState' -> Formula -> Maybe [Int]
patternMatch' x y = fmap (L.map formulaToInt) $ patternMatch x y
                                     
hasInt :: Substitution2 -> Bool
hasInt = (any (\x -> case x of 
                       AInt _ -> True
                       _ -> False)) . M.keys
