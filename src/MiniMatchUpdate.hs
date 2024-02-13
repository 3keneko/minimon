{-# LANGUAGE RecordWildCards #-}

module MiniMatchUpdate where

import Graphics.Gloss.Data.ViewPort (ViewPort)
import System.Random (uniformR, StdGen)
-- import Display (displayMinimon, WhichMinimon(..), displayDialogue)
import MiniMatch (Phase(..), Turn(..), MiniMatch(..), turn)
import Minimon (roundz)

updateModel :: ViewPort -> Float -> MiniMatch -> MiniMatch
updateModel _ _ mm@(MiniMatch { .. }) =
  case phase of
    Dealing -> updateDeal choice newGen mm
    Receiving -> updateRec choice newGen mm
    MessageDeal1 -> upMess1 newGen mm
    MessageReceive1 -> upRec1 newGen mm
    Win -> mm
    Lose -> mm
    NoGoodDeal -> MiniMatch { randomSeed=newGen, phase=Dealing, .. }
    NoGoodRec -> MiniMatch { randomSeed=newGen, phase=Receiving, .. }
    where (choice, newGen) = uniformR (0::Int, 2::Int) randomSeed


upMess1 :: StdGen -> MiniMatch -> MiniMatch
upMess1 gen (MiniMatch{..})= MiniMatch {randomSeed=gen, phase=Receiving, .. }

upRec1 :: StdGen -> MiniMatch -> MiniMatch
upRec1 gen (MiniMatch{..}) = MiniMatch {randomSeed=gen, phase=Dealing, ..}
-- mapToRange :: Float -> Int

-- mapToRange = (+ 1) . round . (* 32000)

updatePlay :: Turn -> Int -> StdGen -> MiniMatch -> MiniMatch
updatePlay t i newGen mm@(MiniMatch { .. }) =
  case roundz i (f mm) (g mm) of
    Left "No more attacks" -> MiniMatch {  phase=stuckPhase, randomSeed=newGen, ..}
    Left "Pokemon died" -> MiniMatch { endPhase=True, phase=specialPhase, randomSeed=newGen, .. }
    Right (_, m2, att) -> finalM t m2 att
    _ -> error "Unexpected return from roundz function"
    where (f, g, specialPhase, stuckPhase) = turn t
          finalM s m att = if s == Ours then MiniMatch { phase=MessageDeal1, currAtt=Just att, themPoke=m, randomSeed=newGen, .. } else
            MiniMatch { phase=MessageReceive1, currAtt = Just att, ourPoke=m, randomSeed=newGen, .. }

updateDeal :: Int -> StdGen -> MiniMatch -> MiniMatch
updateDeal = updatePlay Ours

updateRec :: Int -> StdGen -> MiniMatch -> MiniMatch
updateRec = updatePlay Their
  -- case roundz i ourPoke themPoke of
  --   Left "No more attacks" -> MiniMatch { phase=NoGoodDeal, randomSeed=newGen, .. }
  --   Left "Pokemon died" -> MiniMatch {endPhase=True, phase=Win, randomSeed=newGen, ..}
  --   Right (_, m2, att) -> MiniMatch {phase=MessageDeal1, currAtt = Just att, themPoke=m2, randomSeed=newGen, ..}
  --   _ -> error "weird"
