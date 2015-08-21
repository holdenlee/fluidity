{-# OPTIONS

  -XExistentialQuantification
  -XRank2Types
#-}

{-# LANGUAGE TemplateHaskell #-}

module Modifiers where

import Data.Maybe
import qualified Data.Map.Strict as M
import Control.Lens

type ModifierMap = M.Map String Double

totalMod :: ModifierMap -> Double
totalMod = sum . M.elems

modifier :: String -> Lens' ModifierMap Double
modifier modName = lens (fromMaybe 0 . M.lookup modName) (flip (M.insert modName))

decay :: String -> Double -> ModifierMap -> ModifierMap
decay str mu = M.adjust (*mu) str

decayF :: (String -> Bool) -> Double -> ModifierMap -> ModifierMap
decayF cond mu = M.mapWithKey (\str val -> if cond str then mu*val else val)


