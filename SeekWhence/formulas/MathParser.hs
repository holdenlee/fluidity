{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
#-}

module MathParser (parseFun, parseDispExpr) where
import System.Environment
import Control.Monad
import Data.Tree
import Data.List
import Search
import Text.ParserCombinators.Parsec

type Crumbs = TPath (Tree String) String

funTree :: Crumbs -> GenParser Char st Crumbs
funTree t = 
  do {
    char '('; 
    (funTree (down t))
  } <|>
  do {
    str <- word;
    (funTree (changeMe (Node str []) t))
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
    
parseFun :: String -> Either ParseError (Tree String)
parseFun input = 
  let 
    t = parse (funTree emptyPath) "(unknown)" input
  in
    case t of 
      Right tp -> Right (curTree tp)
      Left err -> Left err

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

main = do 
  args <- getArgs
  case (parseFun (args !! 0)) of 
    Left err -> putStrLn "Error"
    Right t  -> putStrLn (show t)
