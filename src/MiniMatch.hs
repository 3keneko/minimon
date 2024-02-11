{-# LANGUAGE RecordWildCards #-}

module MiniMatch where

import System.Random
import Graphics.Gloss

data Phase = Receiving | Dealing | Messaging deriving (Show, Eq)

data MiniMatch = MiniMatch {
  ourPoke :: !Minimon,
  themPoke :: !Minimon,
  phase :: !Phase
} deriving (Eq)


mapToRange :: Float -> Int
maptToRange = (+ 1) . round . (* 32 000)


showModel :: MiniMatch -> Picture


updateModel :: ViewPort -> Float -> MiniMatch -> MiniMatch
updateModel _ flt (MiniMatch { .. }) =
  case phase of
    Dealing ->
      let gen = mkStdGen $ mapToRange flt
          choice = uniformR (0::Int, 2::Int) gen
