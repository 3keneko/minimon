{-# LANGUAGE ViewPatterns #-}
module Markov where

import Data.Graph.DGraph (DGraph(..), insertArcs, toArcsList)
import Data.Graph.Types (Arc(..), empty, IsEdge (fromTriple, toTriple))
import MinimonTypes (MiniType(..), Effectivness(..), types, effectiveness)
import Simulation (cartProdNoDup)
import Numeric.LinearAlgebra (toList, Matrix(..), (><), diag, eig, inv, dispcf, cmap, disp, vector, (#>))

import Data.Function (on)
import Data.List (sortBy)
import Data.Complex (Complex, realPart)
import Graphics.Rendering.Chart (identity)

import Control.Monad (forM_)

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
modelTable x ((==) x -> True) = 0 -- Le 0 ici est arbitraire
modelTable x (effectiveness x -> Immune) = 10
modelTable x (flip effectiveness x -> Immune) = 0
modelTable x y = case effectiveness x y of
  Effective -> if effectiveness y x == Ineffective then 1 else 2
  Ineffective -> if effectiveness y x == Effective then 9 else 8
  NoEffect -> if effectiveness y x == Effective then 8 else
    if effectiveness y x == Ineffective then 2 else 5
  _ -> undefined

distributeMatchesOverTypes :: [MiniType] -> MiniType -> ([(MiniType, Int)], Int)
distributeMatchesOverTypes typs mt = (list, victories)
  where list = map (\x -> (x,  modelTable mt x)) typs
        victories = foldr (\(_, x) acc -> acc + 10 - x) 0 list

distributeMatches :: MiniType -> ([(MiniType, Int)], Int)
distributeMatches = distributeMatchesOverTypes types

makeFirstGraph :: DGraph String Int
makeFirstGraph = insertArcs arcz empty
  where arcz = types >>= \x -> let (a, b) = distributeMatches x in
          Arc (show x) (show x) b : map (\(s,t) -> Arc (show x) (show s) t) a

makeGraphWeighted :: DGraph String Double
makeGraphWeighted = fmap ((/180) . fromIntegral) makeFirstGraph

typeToInt :: MiniType -> Int
typeToInt Normal = 0
typeToInt x = 1 + typeToInt (pred x)

intToType :: Int -> MiniType
intToType 0 = Normal
intToType x = succ (intToType (pred x))

strTypToIn :: String -> Int
strTypToIn = typeToInt . read

graphList :: [Arc String Double]
graphList = toArcsList makeGraphWeighted

graphAsTriple :: [(Int, Int, Double)]
graphAsTriple = map ((\(a, b, c) -> (strTypToIn a, strTypToIn b, c)) . toTriple) graphList

graphAsOrderedList :: [Double]
graphAsOrderedList =
  map snd $ sortBy (compare `on` fst) $ map (\(x, y, z) -> (x + 18 * y, z)) graphAsTriple

mkMatrix ::  Matrix Double
mkMatrix = (18 >< 18) graphAsOrderedList

mkRoundMatrix :: Matrix Double
mkRoundMatrix =
  (18 >< 18) $ map snd $ sortBy (compare `on` fst) $ map ((\(x, y, z) -> (strTypToIn x + 18 * strTypToIn y, z)) . toTriple) $ toArcsList
  (fromIntegral <$> makeFirstGraph)

makeClearGraph :: DGraph String Int
makeClearGraph = insertArcs arcz empty
  where
    clearTypes = take 5 types
    arcz = clearTypes >>= \x -> let (a, b) = distributeMatchesOverTypes clearTypes x in
          Arc (show x) (show x) b : map (\(s,t) -> Arc (show x) (show s) t) a

multALot :: Int -> Matrix Double
multALot n = cmap realPart res
  where (l, v) = eig mkMatrix
        res = v <> diag l ^ n <> inv v

multALotPrint :: Int -> IO ()
multALotPrint n =
  disp 18 $ multALot n

markovPrediction :: Int -> IO ()
markovPrediction n =
  let res = multALot n  #> vector (replicate 18 1)
      o = zip [0..] (Numeric.LinearAlgebra.toList res)
      v = reverse $ sortBy (compare `on` snd) o
  in do
    putStrLn "Les meilleurs types pokémeons rangés dans l'ordre sont"
    forM_ v $ \(a, b) ->
      putStrLn $ show (intToType a) ++ ": " ++ show b
