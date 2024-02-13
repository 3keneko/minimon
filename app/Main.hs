{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Display (displayRectangle, displayText, scaleAndTrans)

import Graphics.Gloss
import Graphics.Gloss.Juicy (loadJuicyPNG, loadJuicyJPG, loadJuicy)
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

loadPokes :: IO (Picture, Picture)
loadPokes = do
  image1 <- loadPNG "imgs/firekatchu.png"
  image2 <- loadPNG "imgs/plantkatchu.png"
  pure (image1, image2)

imagesToDisplay :: IO Picture
imagesToDisplay = do
  (image1, image2) <- loadPokes
  return $ pictures [scaleAndTrans 0.2 0.1 image1X image1Y image1,
                     scaleAndTrans 0.2 0.1 image2X image2Y image2]

steps :: Int
steps = 1

main :: IO ()
main = do
  let window = InWindow "MINIMON" (windowWidth, windowHeight) (20, 20)
  itd <- imagesToDisplay
  forest <- loadForest
  display window white (pictures [forest, itd, displayRectangle (-170) (-200) 200 30 green,
                                  displayRectangle 170 200 200 30 green,
                                  displayText (-200) (-250) 0.15 0.15 "Drakof",
                                  displayText 150 230 0.15 0.15 "Bulbiz",
                                  displayRectangle 200 (-100) 400 200 white,
                                  displayText 30 (-30) 0.15 0.15 "This was not very effective"])
  -- simulate window white pokematch displayPokeMatch
