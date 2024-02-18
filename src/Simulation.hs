{-# LANGUAGE OverloadedLists #-}
module Simulation where

import System.Random (randomIO)
import MiniMatch (MiniMatch (..), mkMatch)
import MinimonTypes (MiniType(..), types)
import Data.Map.Strict (Map, fromList, adjust)
import MiniMatchUpdate (updateNoAnimate)
import Minimon (Minimon(..))
-- import Data.Vector (Vector)
-- import qualified Data.Vector as V
import Control.Monad (forM, liftM)

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

getTournamentStats :: [MiniMatch] -> Map MiniType Int
getTournamentStats matches =
  let winners = decideWinner <$> matches
      mapzies = fromList $ zip types (repeat 0)
      in foldr (adjust (+1)) mapzies winners

mkTournamentAndGetResults :: IO (Map MiniType Int)
mkTournamentAndGetResults = do
  -- first part: We make the tournament
  matches <- forM candidates $ \(c1, c2) ->
       (randomIO :: IO Int) >>= \i
         ->  return $ mkMatch c1 c2 i
  return $ getTournamentStats matches
