module Simulation where

import MiniMatch (MiniMatch (..))
import MinimonTypes (MiniType(..))
import Data.Map.Strict (Map(..))
-- import Data.Function (fix)
import MiniMatchUpdate (updateNoAnimate)
import Minimon (Minimon(..))
import Data.Vector (Vector (..))

eitherDied :: MiniMatch -> Bool
eitherDied = endPhase

upEither :: MiniMatch -> Either MiniMatch MiniMatch
upEither minimatch = if endPhase minimatch then
  Left minimatch else Right minimatch

getWinner :: MiniMatch -> MiniType
getWinner mm = if hp (ourPoke mm) <= 0
  then minitype (themPoke mm)
  else minitype (ourPoke mm)

decideWinner :: MiniMatch -> MiniType
decideWinner m =
  let res = updateNoAnimate m
    in if endPhase res
       then getWinner res
       else decideWinner res

cartProdNoDup :: Eq a => [a] -> [(a,a)]
cartProdNoDup x =
  filter (uncurry (/=)) $ (,) <$> x <*> x

candidates :: [(MiniType, MiniType)]
candidates = cartProdNoDup [(Fire)..(Plant)]

getTournamentWinner :: Vector MiniMatch -> Map MiniType Int
getTournamentWinner matches =
  let winners = decideWinner <$> matches
