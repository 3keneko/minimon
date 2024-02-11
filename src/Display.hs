module Display (displayRectangle, displayText) where

import Graphics.Gloss
    ( Picture, Color, translate, color, rectangleSolid, text, scale )

displayRectangle :: Float -> Float -> Float -> Float -> Color -> Picture
displayRectangle x y l w c = translate x y $ color c $ rectangleSolid l w

displayText :: Float -> Float -> Float -> Float -> String -> Picture
displayText x y l w = translate x y . scale l w . text
