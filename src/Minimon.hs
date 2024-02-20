{-# LANGUAGE RecordWildCards #-}

module Minimon
  (Attack(..),
  Minimon(..),
  makeDmg,
  strike,
  use,
  anyTooUsed,
  pickAttack,
  roundz,
  computeDamage,
  effOfAtt
  ) where

import MinimonTypes (MiniType(..), effCoeff, effectiveness, Effectivness)
import Data.Vector ((!), Vector)

data Attack = Attack {
  damage :: !Int,
  number :: !Int,
  the_type :: !MiniType,
  attackName :: !String
} deriving (Eq, Show)

data Minimon = Minimon {
  hp :: !Int,
  minitype :: !MiniType,
  attacks :: !(Vector Attack),
  name :: !String
} deriving (Eq, Show)



computeDamage :: Attack -> Minimon -> Int
computeDamage Attack{..}  =
  (*  damage) .  effCoeff the_type . minitype

makeDmg :: Int -> Int -> MiniType -> MiniType -> Int
makeDmg hpi val min_type att_type =
  hpi - val * effCoeff att_type min_type

-- Checks if an attack is effective agains a certain minimon
effOfAtt :: Attack -> Minimon -> Effectivness
effOfAtt = (.minitype)  . effectiveness . the_type

strike :: Attack -> Minimon -> Minimon
strike (Attack dam _ typ _)  (Minimon h mintyp att n) =
  Minimon (makeDmg h dam mintyp typ) mintyp att n

-- attackedBy :: Minimon -> Minimon -> Int -> Minimon

-- uncurredFlippedStrike :: (Minimon, Attack) -> Maybe (Minimon, Attack)
-- uncurredFlippedStrike = (uncurry . flip) strike

use :: Attack -> Attack
use (Attack { .. }) = Attack { number=number-1, .. }


anyTooUsed :: Foldable t => t Attack -> Bool
anyTooUsed = any (\x -> number x < 0)


pickAttack :: Int -> Minimon -> Maybe (Attack, Minimon)
pickAttack i (Minimon { .. }) =
  let att = attacks ! i
      newAtt = fmap (\x -> if x == att then use x else x) attacks
  in if anyTooUsed newAtt then Nothing else Just (att, Minimon {attacks=newAtt, ..} )


stillAlive :: (Minimon, Minimon, Attack) -> Either String (Minimon, Minimon, Attack)
stillAlive t@(_, m2, _) = if hp m2 > 0 then Right t else Left "Pokemon died"


roundz :: Int -> Minimon -> Minimon -> Either String (Minimon, Minimon, Attack)
roundz i m1 m2 = case pickAttack i m1 of
  Just (att, m) -> stillAlive (m, strike att m2, att)
  Nothing -> Left "No more attacks"


-- round :: Integer -> Minimon -> Minimon -> Either String (Minimon, Minimon)
-- round i m1 m2 =
--  case pickAttack i m1 of
--    Right (_, )
