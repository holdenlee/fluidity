{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
#-}

module MathParser (parseFun, parseDispExpr, showFormula) where
import System.Environment
import Control.Monad
import Data.Tree
import Data.List as L
import Search
import Data.Map as M
import Text.ParserCombinators.Parsec

import Utilities
import Formulas

type Crumbs = TPath Formula Atom

funTree :: Crumbs -> GenParser Char st Crumbs
funTree t = 
  do {
    char '('; 
    (funTree (down t))
  } <|>
  do {
    num <- many1 digit;
    funTree (changeMe (Node (AInt $ read num) []) t)
  } <|>
  do {
    str <- word;
    case str of
      'n':'_':num ->  funTree (changeMe (Node (AVar $ read num) []) t)
      _ -> (funTree (changeMe (Node (AStr str) []) t))
  } <|>
  do {
    oneOf ", "; -- disallowed \n 
    (funTree (next t))
  } <|>
  do {
    char ')';
    (funTree (up t))
  } <|>
  do {
    eof;
    return t
  }

word :: GenParser Char st String
word = 
    many1 (noneOf " (),")
    
parseFun' :: String -> Either ParseError Formula
parseFun' input = 
  let 
    t = parse (funTree emptyPath) "(unknown)" input
  in
    case t of 
      Right tp -> Right (curTree tp)
      Left err -> Left err

parseFun :: String -> Formula
parseFun = justRight . parseFun'

num :: GenParser Char st String
num = 
    many1 (digit)

dispExprTree :: String -> [String] -> GenParser Char st String
dispExprTree s args = 
  do {
    try (string "?args");
    dispExprTree (s ++ (intercalate ", " args)) args
  } <|>
  do {
    char '?'; 
    d <- many1 (digit);
    dispExprTree (s ++ (args !! (((read d)::Int) - 1))) args
  } <|>
  do {
    t <- anyChar;
    dispExprTree (s ++ [t]) args
  } <|>
  do {
    eof;
    return s
  }


parseDispExpr :: String -> [String] -> Either ParseError String
parseDispExpr input args = parse (dispExprTree "" args) "(unknown)" input

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
      if (L.null (children f)) then rt else (rt ++ "(" ++ "?args" ++ ")")
    drule = tryWithDefault (\sym -> M.lookup sym slib) def rt
  in (case (parseDispExpr drule (fmap (showFormula slib) (children f))) of
    Right s -> s
    Left _ -> "error")
