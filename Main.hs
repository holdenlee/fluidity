{-# OPTIONS

  -XExistentialQuantification
  -XRank2Types
#-}

{-# LANGUAGE TemplateHaskell #-}

module Main where
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
import System.Random
import Control.Lens

import Utilities
import ParseUtilities
import GraphUtils

import Mind
import PTScan
