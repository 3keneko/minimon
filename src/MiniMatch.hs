{-# LANGUAGE RecordWildCards #-}

module MiniMatch where

import System.Random (mkStdGen, uniformR)
import Minimon
import Graphics.Gloss (display, Picture, pictures)
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Display (displayMinimon, WhichMinimon(..))

data Phase =
  Dealing
  | NoGoodDeal
  | MessageDeal1
  | MessageDeal2
  | Win
  | Receiving
  | NoGoodRec
  | MessageReceive1
  | MessageReceive2
  | Lose deriving (Eq)

data MiniMatch = MiniMatch {
  ourPoke :: !Minimon,
  themPoke :: !Minimon,
  phase :: !Phase,
  currAtt :: Maybe Attack,
  endPhase :: Bool
} deriving (Eq)


mapToRange :: Float -> Int
mapToRange = (+ 1) . round . (* 32000)

showModel :: [Picture] -> MiniMatch -> Picture
showModel pictures mm@(MiniMatch{ .. }) = case phase of
  Dealing -> showDeal pictures mm


updateModel :: ViewPort -> Float -> MiniMatch -> MiniMatch
updateModel _ flt mm@(MiniMatch { .. }) =
  case phase of
    Dealing ->
      let gen = mkStdGen $ mapToRange flt
          (choice,_) = uniformR (0::Int, 2::Int) gen
      in updateDeal choice mm


updateDeal :: Int -> MiniMatch -> MiniMatch
updateDeal i (MiniMatch { .. }) =
  case roundz i ourPoke themPoke of
    Left "No more attacks" -> MiniMatch { phase=NoGoodDeal, .. }
    Left "Pokemon died" -> MiniMatch {endPhase=True, phase=MessageDeal1, ..}
    Right (_, m2, att) -> MiniMatch {currAtt = Just att, themPoke=m2, ..}

showDeal :: [Picture] -> MiniMatch -> Picture
showDeal pics (MiniMatch {..})  =
  pictures $ pics ++ displayMinimon ourPoke Lower
  ++ displayMinimon themPoke Upper
