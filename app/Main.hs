{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Display (scaleAndTrans, minimonImg)

import Graphics.Gloss
import Graphics.Gloss.Juicy (loadJuicyPNG, loadJuicyJPG)
import MiniMatch (MiniMatch(..), Phase(..))
import Creatures (createGenericCreature)
import System.Environment (getArgs)
import System.Random (mkStdGen,  randomIO)
import MinimonTypes (MiniType(..))
import MiniMatchShow (showModel)
import MiniMatchUpdate (updateModel)

-- import Control.Monad (unless)
-- import Codec.Picture

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600

image1X, image1Y, image2X, image2Y :: Float
image1X = -1000
image1Y = -1000
image2X = 1000
image2Y = 1000

loadAnImg :: (FilePath -> IO (Maybe Picture)) -> FilePath -> IO Picture
loadAnImg f path = do
  mayImg <- f path
  case mayImg of
    Just s -> return s
    _ -> error $ "Error opening the file" ++ path

loadPNG :: FilePath -> IO Picture
loadPNG = loadAnImg loadJuicyPNG

loadJPG :: FilePath -> IO Picture
loadJPG = loadAnImg loadJuicyJPG

loadForest :: IO Picture
loadForest = scale 1.5 1.5 <$> loadJPG "imgs/foret.jpg"

nameIt :: MiniType -> String
nameIt Fire = "Drakof"
nameIt Plant = "Bulbiz"
nameIt Steel = "Steelix"
nameIt Ice = "Icecone"
nameIt Normal = "Josh"

readIt :: String -> MiniType
readIt "feu" = Fire
readIt "glace" = Ice
readIt "acier" = Steel
readIt "plante" = Plant
readIt _ = error "Pas un type correct"

imagesToDisplay :: MiniType -> MiniType -> IO Picture
imagesToDisplay type1 type2 = do
  image1 <- loadPNG $ minimonImg type1
  image2 <- loadPNG $ minimonImg type2
  return $ pictures [scaleAndTrans 0.2 0.1 image1X image1Y image1,
                     scaleAndTrans 0.2 0.1 image2X image2Y image2]

car :: [a] -> a
car = head

cadr :: [a] -> a
cadr = head . tail

steps :: Int
steps = 1

main :: IO ()
main =
  let window = InWindow "MINIMON" (windowWidth, windowHeight) (20, 20)
    in do
    args <- fmap readIt <$> getArgs
    itd <- imagesToDisplay (car args) (cadr args)
    seed <- randomIO :: IO Int
    bg <- loadForest
    let poko1 =  createGenericCreature (car args) (nameIt (car args))
        poko2 = createGenericCreature (cadr args) (nameIt (cadr args))
      in do
       simulate window white steps (MiniMatch { ourPoke=poko1, themPoke=poko2, phase=Dealing, currAtt=Nothing, endPhase=False, randomSeed=mkStdGen seed })
        (showModel [bg, itd]) updateModel
