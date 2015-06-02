module GraphUtils where
import System.Environment
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
import Search

import Utilities
import ParseUtilities


getEdges:: G.Gr a b -> Node -> Node -> [b]
getEdges g s t = map snd $ filter (\(v,l) -> v==t) $ lsuc g s

modEdge:: (Eq b) => Edge -> (b-> b) -> G.Gr a b -> G.Gr a b
modEdge (v,w) f g = g |> delEdge (v,w) |> (insEdges $ map (\elab -> (v,w,f elab)) (getEdges g v w))

modEdges:: (Eq b) => [Edge] -> (b-> b) -> G.Gr a b -> G.Gr a b
modEdges es f g = g |> delEdges es |> (insEdges $ concat $ map (\(v,w) -> map (\elab -> (v,w,f elab)) (getEdges g v w)) es)
--f v w elab

mkBidir :: b -> [LNode a] -> [LEdge b] -> G.Gr a b
mkBidir def vs es = 
    let
        es' = map (\(x,y,z) -> (y,x,def)) es
        es2 = L.unionBy (\(x,y,_) (x1,y1,_) -> x==x1 && y==y1) es es'
    in 
      G.mkGraph vs es2
