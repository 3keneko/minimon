{-# LANGUAGE RecordWildCards #-}
module Display (displayRectangle, displayText, displayMinimon, WhichMinimon(..)) where

import Graphics.Gloss
    ( Picture, Color, translate, color, rectangleSolid, text, scale, pictures, yellow, green, red )

import MinimonTypes (MiniType(..))
import Minimon ( Minimon(..) )

data WhichMinimon = Upper | Lower deriving Eq
type Attacked = Bool

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


minimonImg :: Minimon -> FilePath
minimonImg (Minimon{minitype=Fire}) = "imgs/firekatchu.png"
minimonImg (Minimon{minitype=Plant}) = "imgs/plantkatchu.png"
minimonImg _ = ""


displayMinimon :: Minimon -> WhichMinimon -> [Picture]
displayMinimon (Minimon {..}) Upper
  = [ displayHealth 170 200 hp,
      displayText 150 230 0.15 0.15 name]
displayMinimon (Minimon{..}) Lower
  = [ displayHealth (-170) (-200) hp,
      displayText (-200) (-250) 0.15 0.15 name ]
