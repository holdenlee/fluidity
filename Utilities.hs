{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module Utilities  where
import System.Environment
import Control.Monad
import Data.Tree
import qualified Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Hashable
import Data.Maybe
import Debug.Trace

--combinators

doIf :: Bool -> (a -> a) -> (a -> a)
doIf p f = (\x -> if p then f x else x)

ifelselist:: [(Bool, a)] -> a -> a
ifelselist li y = 
  case li of 
    [] -> y
    ((b,x):li2) -> if b then x else ifelselist li2 y

iflist :: [(Bool, a)] -> a
iflist li = 
  case li of 
    ((b,x):li2) -> if b then x else iflist li2

loopUntilFail :: (a -> Maybe a) -> a -> a
loopUntilFail f x = 
  case (f x) of 
    Just y -> loopUntilFail f y
    Nothing -> x

repeatTimes :: Int -> (a-> a) -> a -> a
repeatTimes n f x = 
    if n <=0 then x else repeatTimes (n-1) f (f x)

while :: (a -> Bool) -> (a -> a) -> a -> a
while p f x = 
  if (p x) then while p f (f x) else x

loopUntil :: (a -> Bool) -> (a -> a) -> a -> a    
loopUntil p f x = 
  if (p x) then x else loopUntil p f (f x)

stopBefore :: (a -> Bool) -> (a -> a) -> a -> a    
stopBefore p f x = 
  if (p (f x)) then x else stopBefore p f (f x)

justRight :: Either a b -> b
justRight (Right x) = x

lookup2 :: (Ord a) => a -> Map.Map a b -> b
lookup2 x h = fromJust (Map.lookup x h)

lookupWithDefault:: (Ord a) => a -> Map.Map a b -> b -> b
lookupWithDefault x h y = fromMaybe y (Map.lookup x h)
  
--Maps and Lists

insertMultiple :: (Ord a) => [(a, b)] -> Map.Map a b -> Map.Map a b 
insertMultiple li h = foldl (\hm -> (\(x,y) -> Map.insert x y hm)) h li


listUpdateFun :: Int -> (a-> a) -> [a] -> [a]
listUpdateFun n f li = listUpdate n (f (li !! n)) li

listUpdatesFun :: (Int -> Bool) -> (a-> a) -> [a] -> [a]
listUpdatesFun p f li = map (\(i,x) -> doIf (p i) f x) (zip [1..] li)

replaceSublist :: Int -> Int -> [a] -> [a] -> [a]
replaceSublist m n li li2 =
      let 
        front = take m li2
        back = drop (max m n) li2
      in
        front ++ li ++ back

listUpdate :: Int -> a -> [a] -> [a]
listUpdate n x li = replaceSublist n (n+1) [x] li

filterJust:: [Maybe a] -> [a]
filterJust li = map fromJust (filter isJust li)

filterZip:: (b->Bool) -> [a] -> [b] -> [(a,b)]
filterZip p as bs = filter (\(x,y) -> p y) (zip as bs)

cofilter :: (b->Bool) -> [a] -> [b] -> ([a],[b])
cofilter p as bs = unzip (filterZip p as bs)

lookupList:: (Ord a) => [a] -> Map.Map a b -> [Maybe b]
lookupList as mp = fmap (flip Map.lookup mp) as

lookupList2:: (Ord a) => [a] -> Map.Map a b -> [b]
lookupList2 as mp = fmap (flip lookup2 mp) as

third :: (a,b,c) -> c
third (_,_,z) = z

mapThr :: (c->d) -> (a,b,c) -> (a,b,d)
mapThr f (x,y,z) = (x,y,f z)

inj2:: a -> b -> (a,b)
inj2 x y = (x,y)


inj1:: b -> a -> (a,b)
inj1 y x = (x,y)

mlookup :: Int -> [a] -> Maybe a
mlookup n li = if 0<=n && n<(length li) then Just (li!!n) else Nothing 

tryWithDefault::(a->Maybe b) -> b-> a -> b
tryWithDefault f def x = 
  case (f x) of
    Nothing -> def
    Just y -> y

foldIterate:: (a->c->c)->[a]->c->c
foldIterate f as x = foldl (flip f) x as

foldIterate2:: (a->b->c->c)->[a]->[b]->c->c
foldIterate2 f as bs x = foldl (\y -> \(xa, xb) -> f xa xb y) x (zip as bs)

for :: [a] -> b -> (a -> b -> b) -> b
for li x0 f = foldl (flip f) x0 li

--for :: a -> (a -> a) -> (a -> Bool) -> (a -> a) -> a

--NOTE: if you're doing this a lot, use array instead
sublist :: Int -> Int -> [a] -> [a]
sublist m n ls =  take (n-m) . drop m $ ls
-- >=m, <n

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 = zipWith

infixr 0 <|
(<|) :: (a -> b) -> a -> b
(<|) = ($)

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

--debug x y = x
debug = flip trace

debugShow x = trace (show x) x

debugSummary x f = trace (show (f x)) x

enumerate :: [a] -> [(Int, a)]
enumerate li = zip [1..] li

zenumerate :: [a] -> [(Int, a)]
zenumerate li = zip [0..] li

emap :: ((Int,a)->b) -> [a] -> [b]
emap f li = map f (enumerate li)

zemap :: ((Int,a)->b) -> [a] -> [b]
zemap f li = map f (zenumerate li)

keepi :: (Int -> Bool) -> [a] -> [a]
keepi f li = map snd (filter (f.fst) (enumerate li))

appendFun :: (a -> b) -> a -> (a,b)
appendFun f x = (x, f x)

mindex:: [a] -> Int -> Maybe a
mindex li i = 
    if i>=0 && i<length li then Just (li!!i) else Nothing
