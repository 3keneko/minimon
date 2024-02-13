{-# LANGUAGE RecordWildCards #-}
module MiniMatchShow (showModel) where

import Minimon
import Graphics.Gloss (translate, Picture, pictures)
import Display (displayMinimon, WhichMinimon(..), displayDialogue, displayText)
import MiniMatch (MiniMatch (..), Phase(..), getAttack)

-- Helper function
dispMons :: MiniMatch -> [Picture]
dispMons (MiniMatch{..}) = displayMinimon ourPoke Lower ++ displayMinimon themPoke Upper

-- The main displaying function
showModel :: [Picture] -> MiniMatch -> Picture
showModel pics mm@(MiniMatch{ .. }) = case phase of
  Dealing -> showDeal pics mm
  Receiving -> showRec pics mm
  MessageDeal1 -> showMess1 pics mm
  MessageReceive1 -> showRec1 pics mm
  Win -> showWon pics mm
  Lose -> showLost pics mm
  _ -> showBad pics mm
-- Takes care of the Dealing/Receving phases
showGeneral :: (MiniMatch -> Minimon) -> [Picture] -> MiniMatch -> Picture
showGeneral f pics m = pictures $ pics ++ dispMons m ++ displayDialogue dialogue
  where dialogue = name (f m) ++ " va attaquer " -- ++ attackName (getAttack m)

showDeal :: [Picture] -> MiniMatch -> Picture
showDeal = showGeneral ourPoke

showRec :: [Picture] -> MiniMatch -> Picture
showRec = showGeneral themPoke

-- S'occupe de la phase 2
showDial :: (MiniMatch -> Minimon) -> (MiniMatch -> Minimon) -> [Picture] -> MiniMatch -> Picture
showDial f g pics m = pictures $ pics ++ dispMons m ++ displayDialogue dialogue ++ [efficiency]
  where
    att = getAttack m
    attName = attackName att
    poke = f m
    pokeName = name poke
    endM = show $ effOfAtt (getAttack m) (g m)
    dialogue =  pokeName ++ " utilise " ++ attName ++ "\n"
    efficiency = displayText 20 (-50) 0.15 0.15 endM
showMess1 :: [Picture] -> MiniMatch -> Picture
showMess1 = showDial ourPoke themPoke

showRec1 :: [Picture] -> MiniMatch -> Picture
showRec1 = showDial themPoke ourPoke

-- Gère le display quand il s'agit de montrer au joueur qu'il a gagné car c'est le plus fort
showWon :: [Picture] -> MiniMatch -> Picture
showWon bg (MiniMatch{..}) = pictures $ bg ++ displayMinimon ourPoke Lower
  ++ displayDialogue (name ourPoke ++ " a vaincu!")

showLost :: [Picture] -> MiniMatch -> Picture
showLost bg (MiniMatch{..}) = pictures $ bg ++ displayMinimon themPoke Upper
  ++ displayDialogue (name themPoke ++ " a vaincu!")


showBad :: [Picture] -> MiniMatch -> Picture
showBad bg m = pictures $
  showMess1 bg m : displayDialogue "Le minimon ne peut plus utiliser cette attaque"
