module Minimon
  (Attack(..),
  Minimon(..),
  makeDmg,
  strike,
  use,
  anyTooUsed,
  pickAttack,
  roundz
  ) where
import MinimonTypes (MiniType, effCoeff)
import Data.Vector ((!), Vector)

data Attack = Attack {
  damage :: !Integer,
  number :: !Integer,
  the_type :: !MiniType
} deriving (Eq, Show)

data Minimon = Minimon {
  hp :: !Integer,
  minitype :: !MiniType,
  attacks :: Vector Attack
} deriving (Eq, Show)



makeDmg :: Integer -> Integer -> MiniType -> MiniType -> Integer
makeDmg hpi val min_type att_type =
  hpi - val * effCoeff att_type min_type

strike :: Attack -> Minimon -> Minimon
strike (Attack dam _ typ)  (Minimon h mintyp att) =
  Minimon (makeDmg h dam mintyp typ) mintyp att
-- attackedBy :: Minimon -> Minimon -> Int -> Minimon

-- uncurredFlippedStrike :: (Minimon, Attack) -> Maybe (Minimon, Attack)
-- uncurredFlippedStrike = (uncurry . flip) strike

use :: Attack -> Attack
use (Attack { damage=d, number=n, the_type=t }) = Attack { damage = d, number=n-1, the_type=t }

anyTooUsed :: Foldable t => t Attack -> Bool
anyTooUsed = any (\x -> number x < 0)

pickAttack :: Int -> Minimon -> Maybe (Attack, Minimon)
pickAttack i (Minimon { hp = h, attacks = l, minitype=t }) =
  let att = l ! i
      newAtt = fmap (\x -> if x == att then use x else x) l
  in if anyTooUsed newAtt then Nothing else Just (att, Minimon {hp=h, attacks=newAtt, minitype=t} )


stillAlive :: (Minimon, Minimon) -> Either String (Minimon, Minimon)
stillAlive t@(_, m2) = if hp m2 > 0 then Right t else Left "Pokemon died"


roundz :: Int -> Minimon -> Minimon -> Either String (Minimon, Minimon)
roundz i m1 m2 = case pickAttack i m1 of
  Just (att, m) -> stillAlive (m, strike att m2)
  Nothing -> Left "No more attacks"


-- round :: Integer -> Minimon -> Minimon -> Either String (Minimon, Minimon)
-- round i m1 m2 =
--  case pickAttack i m1 of
--    Right (_, )
