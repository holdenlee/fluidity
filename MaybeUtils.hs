module MaybeUtils where

import Data.Maybe
import Control.Monad

--http://hackage.haskell.org/package/syb-0.5.1/docs/src/Data-Generics-Aliases.html#orElse
-- | Left-biased choice on maybes
orElse :: Maybe a -> Maybe a -> Maybe a
x `orElse` y = case x of
                 Just _  -> x
                 Nothing -> y

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
