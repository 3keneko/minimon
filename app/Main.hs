{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib

import Graphics.Gloss
import Control.Monad (unless)
import Codec.Picture

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600

image1X, image1Y, image2X, image2Y :: Float
image1X = -100
image1Y = 0
image2X = 100
image2Y = 0

loadPNG :: FilePath -> IO
loadPNG path = do
  image <- readImage path
  case image of
    Left err -> error $ "Error loading image: " ++ err
    Right dynamicImage -> return $ case dynamicImage of
      ImageRGBA8 img -> bitmapOfForeignPtr (imageWidth img) (imageHeight img)
                                           (BitmapFormat topToBottom RGBA8) (imageData img) True
    _ -> error "Unsupported image format"

loadImages :: IO (Picture, Picture)
loadImages = do
  image1 <- loadPNG "imgs/firekatcthu.png"
  image2 <- loadPNG "imgs/plantkatchu.png"
  pure (image1, image2)

main :: IO ()
main = do
  (image1, image2) <- loadImages

  let window = InWindow "MINIMON" (windowWidth, windowHeight) (20, 20)
      background = white
   display window background (pictures [translate image1X image1Y image1, translate image2X image2Y image2])
