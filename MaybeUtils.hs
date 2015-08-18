module MaybeUtils where

import Data.Maybe

(.|) :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
f .| g = \x -> 
         case f x of 
           Just y -> Just y
           Nothing -> 
               case g x of
                 Just z -> Just z
                 Nothing -> Nothing

(.&) :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
(.&) = (>=>)

{-
tryDo :: (a -> Maybe a) -> a -> Maybe a
tryDo f x = case f x of
              Just y -> Just y
              Nothing -> Just x
-}
