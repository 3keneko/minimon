{-# LANGUAGE ViewPatterns #-}
module Markov where

import Data.Graph.DGraph (DGraph(..), insertArcs)
import Data.Graph.Types (Arc(..), empty)
import MinimonTypes (MiniType(..), Effectivness(..), types, effectiveness)
import Simulation (cartProdNoDup)

-- Number of types = 18

{-
On rappelle juste ici la table donnée dans le document Latex
Fort & 0.5 & 0.9 &  0.8 & 0 \\
\hline
Faible & 0.1 & 0.5 & 0.2 & 0 \\
\hline
Neutre & 0.2 & 0.8 & 0.5 & 0 \\
\hline
Immunisé & 1 & 1 & 1 & 0.5 \\

On sait qu'il y a 18 types, pour savoir comment se déplacer d'un type à un autre,
on va simplement faire comme si il y avait 10 matchs entre
notre type de départ, et le type adverse, et on va répéter la procédure
pour chacun des autres types, au final, c'est comme si on aura fait tous les matches
-}

modelTable :: MiniType -> MiniType -> Int
modelTable x ((==) x -> True) = 5
modelTable x (effectiveness x -> Immune) = 10
modelTable x (flip effectiveness x -> Immune) = 0
modelTable x y = case effectiveness x y of
  Effective -> if effectiveness y x == Ineffective then 9 else 8
  Ineffective -> if effectiveness y x == Effective then 1 else 2
  NoEffect -> if effectiveness y x == Effective then 2 else 8
  _ -> undefined
distributeMatches :: MiniType -> ([(MiniType, Int)], Int)
distributeMatches mt = (list, victories)
  where list = map (\x -> (x,  10 - modelTable mt x)) types
        victories = foldr (\(_, x) acc -> acc + 10 - x) 0 list

makeFirstGraph :: DGraph String Int
makeFirstGraph = insertArcs arcz empty
  where arcz = types >>= \x -> let (a, b) = distributeMatches x in Arc (show x) (show x) b : map (\(s,t) -> Arc (show x) (show s) t) a

makeGraphWeighted :: DGraph String Double
makeGraphWeighted = fmap ((/180) . fromIntegral) makeFirstGraph
