{-# LANGUAGE RecordWildCards #-}

module MiniMatch (MiniMatch(..), Phase(..), Turn(..), turn, getAttack) where

import System.Random (StdGen)
import Minimon ( Minimon(..), Attack(..) )
import Data.Maybe (fromMaybe)
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
  | Lose deriving Eq

data Turn = Their | Ours deriving Eq

data MiniMatch = MiniMatch {
  ourPoke :: !Minimon,
  themPoke :: !Minimon,
  phase :: !Phase,
  currAtt :: !(Maybe Attack),
  endPhase :: !Bool,
  randomSeed :: !StdGen
} deriving Eq


turn :: Turn -> (MiniMatch->Minimon,
                 MiniMatch->Minimon,
                 Phase, Phase)
turn Their = (themPoke, ourPoke, Lose, NoGoodRec)
turn Ours  = (ourPoke, themPoke, Win, NoGoodDeal)

getAttack :: MiniMatch -> Attack
getAttack = fromMaybe (error "No attack pending!") . currAtt

