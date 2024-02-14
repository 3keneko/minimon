{-# LANGUAGE RecordWildCards #-}
module Display (displayRectangle, displayText, displayMinimon, WhichMinimon(..), displayDialogue, scaleAndTrans, minimonImg) where

import Graphics.Gloss
    ( Picture, Color, translate, color, rectangleSolid, text, scale, pictures, yellow, green, red, white )

import MinimonTypes (MiniType(..))
import Minimon ( Minimon(..) )

data WhichMinimon = Upper | Lower deriving Eq
-- type Attacked = Bool

scaleAndTrans :: Float -> Float -> Float -> Float -> Picture -> Picture
scaleAndTrans x y z w = scale x y . translate z w


displayRectangle :: Float -> Float -> Float -> Float -> Color -> Picture
displayRectangle x y l w c = translate x y $ color c $ rectangleSolid l w

healthColor :: Int -> Color
healthColor h | h > 200   = green
              | h > 30    = yellow
              | otherwise = red

-- On va dire que la vie est à 400 de base pour simplifier les choses
displayHealth :: Float -> Float -> Int -> Picture
displayHealth x y h =
  displayRectangle x y (fromIntegral (h `div` 2)) 30 (healthColor h)

displayText :: Float -> Float -> Float -> Float -> String -> Picture
displayText x y l w = translate x y . scale l w . text


minimonImg :: MiniType -> FilePath
minimonImg Fire = "imgs/firekatchu.png"
minimonImg Plant = "imgs/plantkatchu.png"
minimonImg Steel = "imgs/steelkatchu.png"
minimonImg Ice = "imgs/icekatchu.png"
minimonImg _ = ""


displayMinimon :: Minimon -> WhichMinimon -> [Picture]
displayMinimon (Minimon {..}) Upper
  = [ displayHealth 170 200 hp,
      displayText 150 230 0.15 0.15 name ]
displayMinimon (Minimon{..}) Lower
  = [ displayHealth (-170) (-200) hp,
      displayText (-200) (-250) 0.15 0.15 name ]

displayDialogue :: String -> [Picture]
displayDialogue dial = [ displayRectangle 200 (-100) 400 200 white,
           displayText 30 (-30) 0.15 0.15 dial]
