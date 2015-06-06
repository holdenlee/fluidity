{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XMultiWayIf
 -XFlexibleInstances
 -XFlexibleContexts
#-}

module Search (Searchable, children, root, graft, SearchPath, TPath, path, meTree, cur, curTree, up, down, prev, next, start, changeMe, emptyPath, node) where
import System.Environment
import Control.Monad
import Data.Tree

class Searchable a b | a -> b where
      children :: a -> [a]
      root :: a -> b 
      graft :: b -> [a] -> a
node :: (Searchable a b) => b -> a
node x = graft x []

(-:):: a -> (a -> b) -> b
x -: f = f x

instance Searchable (Tree a) a where 
         children ta = (case ta of Node _ c -> c)
         root ta = (case ta of Node a1 _ -> a1)
         graft top ts = Node top ts

--another way to make an instance: for a concrete a with function f: a-> a
--instance Searchable a a where 
--         children ta = f ta
--         root ta = ta
--         graft top ts = top

--breadcrumbs  | c -> a b
class (Searchable a b) => SearchPath a b c | c -> a b  where
      curTree :: c -> a
      cur :: c -> b
      down :: c -> c
      up :: c -> c
      next :: c -> c
      prev :: c -> c
      hasNext :: c -> Bool
      hasPrev :: c -> Bool
      hasChild :: c -> Bool
      start :: a -> c
      changeMe :: a -> c -> c
      atTop :: c -> Bool
      emptyPath :: c

--basically a list zipper inside. probably a little overkill.
data TNode a b = TNode {meN :: b
             , leftN :: [a]
             , rightN :: [a]
             }

instance Show b => Show (TNode a b) where
         show x = show (meN x)

data TPath a b = TPath {path :: [TNode a b]
             , meTree :: Maybe a
             }
             
instance (Show a , Show b) => Show (TPath a b) where
         show x = "(" ++ show (path x) ++ ", " ++ show (meTree x) ++")"

--do not call on empty tree!
--tip :: TPath a b -> b
--tip x = meN (head (path x))

left :: TPath a b -> [a]
left x = leftN (head (path x))

right :: TPath a b -> [a]
right x = rightN (head (path x))

update :: TPath a b -> TNode a b -> Maybe a -> TPath a b
update t n tr = 
  case (path t) of 
    (_:parents) -> TPath (n:parents) tr
--error case

--a natural transformation
maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing = []

parentName :: (TPath a b) -> b
parentName t = case (path t) of
                 (parent:ancestors) ->  meN parent


instance (Searchable a b) => SearchPath a b (TPath a b) where
         --have to join up! Note this can be made more efficient
         curTree t = 
           case (meTree t) of (Just a) -> a
         cur t = root (curTree t)
         down t = 
           case (meTree t) of 
             Nothing -> t
             Just tr -> TPath ((TNode (root tr) [] (children tr)):(path t)) Nothing
--also add nth down
         up t = 
           let
             childList =
               case (meTree t) of 
                 Nothing -> (reverse (left t)) ++ (right t)
                 Just c -> (reverse (left t)) ++ [c] ++ (right t) 
             parentTree = graft (parentName t) childList
             (_:ancestors) = (path t)
           in 
             TPath ancestors (Just parentTree)
         next t =
           let 
             toAppend = maybeToList (meTree t)
           in
             case (path t) of 
               (_:_) ->
                 case (right t) of 
                   [] -> update t (TNode (parentName t) (toAppend ++ (left t)) []) Nothing
                   (hd:rest) -> update t (TNode (parentName t) (toAppend ++ (left t)) rest) (Just hd)
               [] -> t
         prev t =
           let 
             toAppend = maybeToList (meTree t)
           in
             case (path t) of 
               (_:_) ->
                 case (left t) of 
                   [] -> update t (TNode (parentName t) [] (toAppend ++ (right t))) Nothing
                   (hd:rest) -> update t (TNode (parentName t) rest (toAppend ++ (right t))) (Just hd)
         hasNext t = case (right t) of 
           [] -> False
           _  -> True
         hasPrev t = case (left t) of 
           [] -> False
           _  -> True
         hasChild t = case (meTree t) of 
           Nothing -> False
           Just tr -> 
             case (children tr) of
               [] -> False
               _  -> True
         start x = (TPath [] (Just x))
         changeMe s x = x{meTree = Just s}
         atTop t = case (path t) of
           [] -> True
           _  -> False
         emptyPath = TPath [] Nothing
