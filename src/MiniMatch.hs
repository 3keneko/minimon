-- {-# LANGUAGE RecordWildCards #-}

module MiniMatch (MiniMatch(..), Phase(..), Turn(..), turn, getAttack, mkMatch) where

import System.Random (StdGen, mkStdGen)
import Minimon ( Minimon(..), Attack(..) )
import Data.Maybe (fromMaybe)
import MinimonTypes (MiniType)
import Creatures (createGenericCreature)
-- import Control.Monad (when)

-- Je sépare le match pokémon en plusieurs phases, c'est très codifié,
-- mais ça permet d'exprimer simplement comment passer d'une étape à l'autre
data Phase =
  Dealing
  | NoGoodDeal
  | MessageDeal1
--  | MessageDeal2
  | Win
  | Receiving
  | NoGoodRec
  | MessageReceive1
  -- | MessageReceive2
  | Lose deriving (Eq, Show)

data Turn = Their | Ours deriving Eq

data MiniMatch = MiniMatch {
  ourPoke :: !Minimon,
  themPoke :: !Minimon,
  phase :: !Phase,
  currAtt :: !(Maybe Attack),
  endPhase :: !Bool,
  randomSeed :: !StdGen
} deriving (Eq, Show)


turn :: Turn -> (MiniMatch->Minimon,
                 MiniMatch->Minimon,
                 Phase, Phase)
turn Their = (themPoke, ourPoke, Lose, NoGoodRec)
turn Ours  = (ourPoke, themPoke, Win, NoGoodDeal)

getAttack :: MiniMatch -> Attack
getAttack = fromMaybe (error "No attack pending!") . currAtt

mkMatch :: MiniType -> MiniType -> Int -> MiniMatch
mkMatch t1 t2 sd = MiniMatch {ourPoke=createGenericCreature t1,
                             themPoke=createGenericCreature t2,
                             phase=Dealing,
                             currAtt=Nothing,
                             endPhase=False,
                             randomSeed=mkStdGen sd}
